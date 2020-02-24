# RNN
# Set wd
# setwd("C:\\Users\\KNewhart\\Documents\\GitHub\\MWRD\\PAA ICT")
# setwd("C:\\Users\\kbnewhart\\Dropbox\\Code\\MWRD\\PAA ICT")

# Install packages
{
  pkgs <- c("xts", "dplyr", "parallel", "doSNOW", "foreach")
  sapply(pkgs, function(p) {
    if(!(p %in% rownames(installed.packages()))) {
      install.packages(p)
    }
    library(p, character.only=TRUE)
  })
  if(!("keras" %in% rownames(installed.packages()))) {
    install.packages("keras")
    library(keras)
    keras::install_keras()
  } else {
    library(keras)
  }
}


# Initialize Parallel backend
{
  # detect threads with parallel()
  nThreads<- detectCores(logical = TRUE)
}

# Initialize dataset
{
  average.parameters <- TRUE
  if(average.parameters) master.timestamps <- read.csv(list.files("results/averages", full.names = TRUE)[1], colClasses=c("NULL", NA, "NULL"))
  if(!average.parameters) master.timestamps <- read.csv(list.files("results/raw_data", full.names = TRUE)[1], colClasses=c("NULL", NA, "NULL"))
  if(average.parameters) master.data <- lapply(list.files("results/averages", full.names = TRUE), read.csv, colClasses=c("NULL", NA, NA), header=TRUE)
  if(!average.parameters) master.data <- lapply(list.files("results/raw_data", full.names = TRUE), read.csv, colClasses=c("NULL", NA, NA), header=TRUE)
  master.data <- lapply(master.data, function(data) {data[which(data[,1] %in% unlist(master.timestamps)),2]})
  master.data <-  do.call("cbind", master.data)
  if(average.parameters) colnames(master.data) <- unlist(lapply(list.files("results/averages"), function(x) substr(x,1,nchar(x)-4)))
  if(!average.parameters) colnames(master.data) <- unlist(lapply(list.files("results/raw_data"), function(x) substr(x,1,nchar(x)-4)))
  
  # Create time object
  master.timestamps <- as.POSIXct(unlist(master.timestamps), origin="1970-01-01", tz="GMT")
  attributes(master.timestamps)$tzone <- "America/Denver"
  master.zoo <- zoo(master.data, master.timestamps)
  # Remove rows that are not in the phosphorus dataset
  # rows2keep <- which(!is.na(master.zoo[,"OP_FC24"]))
  # master.zoo <- master.zoo[rows2keep,]
  # Remove cols that have NAs
  apply(master.zoo,2,function(x) length(which(is.na(x))))
  master.zoo <- master.zoo[,-which(apply(master.zoo,2, anyNA))] # remove NAs
  
  all.data <- master.zoo
  all.data <- apply(all.data, 2, as.numeric)
  cols <- colnames(all.data)
  cols <- gsub("-", ".", gsub("[)]", ".", gsub("[(]", ".", gsub(" ", ".", cols))))
  colnames(all.data) <- cols
  predict.col <- which(colnames(all.data)=="ECIDX_G")
  all.data[,predict.col] <- log10(all.data[,predict.col])
  all.data <- cbind(all.data[2:nrow(all.data),], all.data[1:(nrow(all.data)-1),predict.col]) # lag ecoli
  # all.data <- all.data[100:nrow(all.data),]
}

# Use parallel loop for rolling window
{
  n_train <- 0.9
  # predictions <- foreach(test.obs=seq(round(nrow(all.data)*n_train), nrow(all.data)), .combine = "rbind", .packages = c("dplyr", "keras")) %dopar% {
  all.predictions <- list()
  start <- round(nrow(all.data)*n_train)
  end <- start+nThreads-1
  while(start < nrow(all.data)) {
    # Create doSNOW compute cluster
    cluster = makeCluster(nThreads, type = "SOCK")
    # register the cluster
    registerDoSNOW(cluster)
    
    predictions <- foreach(test.obs=seq(start, end), .combine = "rbind", .packages = c("dplyr", "keras")) %dopar% {
      # Setup training data
      train.start <- 1+test.obs-round(nrow(all.data)*n_train)
      train.end <- test.obs-1
      # train.data <- all.data[train.start:train.end,]
      train.mean <- apply(all.data[train.start:train.end,],2,mean)
      train.sd <- apply(all.data[train.start:train.end,],2,sd)
      train.x <- simplify2array(list(scale(all.data[train.start:train.end,], center=train.mean, scale=train.sd)[,-predict.col]))
      train.y <- simplify2array(list(scale(all.data[train.start:train.end,], center=train.mean, scale=train.sd)[,predict.col]))
      n_batch <- dim(train.x)[1]
      n_epoch <- 1000
      
      # Setup model
      model <- keras_model_sequential() %>%
        layer_lstm(units=dim(train.x)[2], input_shape=list(dim(train.x)[2],dim(train.x)[3]), activation = "tanh") %>%
        layer_dense(units = 1)
      model %>% compile(
        optimizer = optimizer_rmsprop(),
        loss = "mean_squared_error"
      )
      
      # Train model
      for(i in seq(1,n_epoch)) {
        history <- model %>% fit(
          x=train.x,
          y=train.y,
          batch_size=n_batch,
          epochs=1,
          shuffle=FALSE, 
          stateful=TRUE
        )
        model %>% reset_states()
      }
      
      # Calculate model fit to training data
      validation <- model %>% predict(
        x=train.x,
        batch_size=1
      )
      r2 <- cor(validation, train.y)^2
      
      # Setup testing data
      test.x <- scale(all.data, center=train.mean, scale=train.sd)[test.obs,-predict.col]
      test.y <- scale(all.data, center=train.mean, scale=train.sd)[test.obs,predict.col]
      
      # Predict E coli
      pred <- model %>% predict(
        x=array_reshape(test.x, c(1,length(test.x),1)),
        batch_size=1
      )
      
      # Calculate prediction error (in E. coli units)
      error <- pred*train.sd[predict.col]+train.mean[predict.col]-as.numeric(test.y*train.sd[predict.col]+train.mean[predict.col])
      
      data.frame("R2"=r2, "Prediction.Error"=error)
    } # parallel loop
    
    # stop cluster and remove clients
    stopCluster(cluster)
    # insert serial backend, otherwise error in repetetive tasks
    registerDoSEQ()
    
    print(paste("Observations", start, "to", end, "completed."))
    
    all.predictions[[length(all.predictions)+1]] <- predictions
    start <- end + 1
    end <- start + nThreads - 1
    if(end > nrow(all.data)) end <- nrow(all.data)
  } # for loop
  all.predictions <- do.call("rbind", all.predictions)
}


# Clean up parallel
{
  # stop cluster and remove clients
  stopCluster(cluster)
  # insert serial backend, otherwise error in repetetive tasks
  registerDoSEQ()
  # clean up a bit.
  invisible(gc); remove(nThreads); remove(cluster); 
  
}

# mean(abs(predictions[,1])) # R2
# mean(predictions[,2]^2)^0.5 # RMSE




