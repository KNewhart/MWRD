# Function to create a rnn, train and test in parallel
rnn <- function(all.data, predict.col, train.obs,
                act.function="softsign") {
  # Load required packages
  pkgs <- c("xts", "dplyr", "parallel", "doSNOW", "foreach")
  sapply(pkgs, require, character.only = TRUE)
  if(!("keras" %in% rownames(installed.packages()))) {
    install.packages("keras")
    library(keras)
    keras::install_keras()
  } else {
    library(keras)
  }
  
  # How many threads can you run in parallel
  nThreads<- detectCores(logical = TRUE) 
  
  print(paste(Sys.time(),": Starting training..."))
  
  # Initialize where each parallel iteration is to be saved
  all.predictions <- list()
  # Initialize start and end testing observations
  start <- train.obs+1
  end <- start+nThreads*2-1
  if(end > nrow(all.data)) end <- nrow(all.data)
  
  # Start loop
  while(start < nrow(all.data)) {
    
    if(end == nrow(all.data)) nThreads <- end-start
    
    # Create doSNOW compute cluster
    cluster = makeCluster(nThreads, type = "SOCK")
    # register the cluster
    registerDoSNOW(cluster)
    
    predictions <- foreach(test.obs=seq(start, end), .combine = "rbind", .packages = c("dplyr", "keras")) %dopar% {
      # Setup training data
      train.start <- test.obs-train.obs
      train.end <- test.obs-1
      # train.data <- all.data[train.start:train.end,] # Not included to save memory
      # Scale training data
      train.mean <- apply(all.data[train.start:train.end,],2,mean)
      train.sd <- apply(all.data[train.start:train.end,],2,sd)
      # Create 3D array of training data to simulate time sequence in batches of 1
      train.x <- simplify2array(list(scale(all.data[train.start:train.end,], center=train.mean, scale=train.sd)[,-predict.col]))
      train.y <- simplify2array(list(scale(all.data[train.start:train.end,], center=train.mean, scale=train.sd)[,predict.col]))
      n_batch <- dim(train.x)[1]
      n_epoch <- 1000
      
      # Setup model
      model <- keras_model_sequential() %>%
        layer_lstm(units=dim(train.x)[2], input_shape=list(dim(train.x)[2],dim(train.x)[3]), activation = act.function) %>%
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
    
    print(paste(Sys.time(),": Observations", start, "to", end, "completed."))
    
    all.predictions[[length(all.predictions)+1]] <- predictions
    start <- end + 1
    end <- start + nThreads*2 - 1
    if(end > nrow(all.data)) end <- nrow(all.data)
  } # for loop
  all.predictions <- do.call("rbind", all.predictions)
  return(all.predictions)
}
# Function to compile all data, averaged or instantaneous (TRUE or FALSE)
compile.data <- function(average.parameters) {
  require(xts)
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
  
  # Ammend column names
  cols <- colnames(master.zoo)
  cols <- gsub("-", ".", gsub("[)]", ".", gsub("[(]", ".", gsub(" ", ".", cols))))
  colnames(master.zoo) <- cols
  
  return(master.zoo)
}


# All NAs:
# [1] "AC_N94A"                         "AC_N94B"                         "AC_N94C"                        
# [4] "AI_N93D"                         "AI_N99C"                         "ECIDX_G"                        
# [7] "FC_N231"                         "FC_N236"                         "Main.Inf.Channel.NSEC..TI_N171."
# [10] "NSEC.EFF.FLOW..FY.F225."         "Quad.1.Basins.In.Service"        "Quad.2.Basins.In.Service"       
# [13] "Quad.3.Basins.In.Service"        "Quad.4.Basins.In.Service"

# # Test 1: Log transform E.coli, Average values, remove all NAs, 90%/10% train/test
# all.data <- compile.data(average.parameters=TRUE) # Pull data
# all.data <- all.data[,-which(apply(all.data,2, anyNA))] # Remove all NAs by column
# all.data <- apply(all.data, 2, as.numeric)  # Make numberic
# ecoli.col <- which(colnames(all.data)=="ECIDX_G") # Select E.coli column
# all.data[,ecoli.col] <- log10(all.data[,ecoli.col]) # Log transform E.coli
# test1.raw <- all.data
# test1.rnn <- rnn(all.data, predict.col=ecoli.col, train.obs=ceiling(nrow(all.data)*.9))
# test1.rnn[,2] <- 10^test1.rnn[,2] # Undo log transform
# 
# # Test 2: Average values, removal all NAs, 90%/10% train/test
# all.data <- compile.data(average.parameters=TRUE) # Pull data
# all.data <- all.data[,-which(apply(all.data,2, anyNA))] # Remove all NAs by column
# all.data <- apply(all.data, 2, as.numeric)  # Make numberic
# ecoli.col <- which(colnames(all.data)=="ECIDX_G") # Select E.coli column
# test2.raw <- all.data
# test2.rnn <- rnn(all.data, predict.col=ecoli.col, train.obs=ceiling(nrow(all.data)*.9))
# 
# # Test 3: Log transform E.coli, Instantaneous values, remove all NAs, 90%/10% train/test
# all.data <- compile.data(average.parameters=FALSE) # Pull data
# all.data <- all.data[,-which(apply(all.data,2, anyNA))] # Remove all NAs by column
# all.data <- apply(all.data, 2, as.numeric)  # Make numberic
# ecoli.col <- which(colnames(all.data)=="ECIDX_G") # Select E.coli column
# all.data[,ecoli.col] <- log10(all.data[,ecoli.col]) # Log transform E.coli
# test3.raw <- all.data
# test3.rnn <- rnn(all.data, predict.col=ecoli.col, train.obs=ceiling(nrow(all.data)*.9))
# test3.rnn[,2] <- 10^test3.rnn[,2] # Undo log transform
# 
# # Test 4: Instantaneous values, remove all NAs, 90%/10% train/test
# all.data <- compile.data(average.parameters=FALSE) # Pull data
# all.data <- all.data[,-which(apply(all.data,2, anyNA))] # Remove all NAs by column
# all.data <- apply(all.data, 2, as.numeric)  # Make numberic
# ecoli.col <- which(colnames(all.data)=="ECIDX_G") # Select E.coli column
# test4.raw <- all.data
# test4.rnn <- rnn(all.data, predict.col=ecoli.col, train.obs=ceiling(nrow(all.data)*.9))
# 
# # Compare tests
# tests <- list(test1.rnn, test2.rnn, test3.rnn, test4.rnn)
# lapply(tests, function(t) mean(t[,1])) # R2
# lapply(tests, function(t) mean((t[,2])^2)^0.5) # RMSE
# rm(tests)

# Results:

# R2:
# [1] 0.3445759
# [2] 0.2358411
# [3] 0.3356192
# [4] 0.2211226

# RMSE:
# [1] 1.304123
# [2] 6348.766
# [3] 1.337066
# [4] 6805.203

# Log transform is imperative to accuracy
# Average vs Instantaneous is not significant 


# Test 5-45: Log transform E.coli, Average values, remove NAs from one variable at a time, 90%/10% train/test
rnn.list <- list()
raw.list <- list() 
raw.data <- compile.data(average.parameters=TRUE) # Pull data
for(i in 1:ncol(raw.data)) {
  all.data <- raw.data[-which(is.na(raw.data[,i])),] # Remove NAs from one column
  if(nrow(all.data)==0) next
  all.data <- all.data[,-which(apply(all.data,2, anyNA))] # Remove columns with NAs
  all.data <- apply(all.data, 2, as.numeric)  # Make numberic
  ecoli.col <- which(colnames(all.data)=="ECIDX_G") # Select E.coli column
  all.data[,ecoli.col] <- log10(all.data[,ecoli.col]) # Log transform E.coli
  raw.list[[length(raw.list)+1]] <- all.data
  test <- rnn(all.data, predict.col=ecoli.col, train.obs=ceiling(nrow(all.data)*.9))
  test[,2] <- 10^test[,2] # Undo log transform
  obj.list[[length(obj.list)+1]] <- test
}
unlist(lapply(obj.list, function(t) mean(t[,1]))) # R2
unlist(lapply(obj.list, function(t) mean((t[,2])^2)^0.5)) # RMSE

# # Test 46-86: Log transform E.coli, Instantaneous values, remove NAs from one variable at a time, 90%/10% train/test
# raw.data <- compile.data(average.parameters=TRUE) # Pull data
# for(i in 1:ncol(raw.data)) {
#   all.data <- raw.data[-which(is.na(raw.data[,i])),] # Remove NAs from one column
#   all.data <- all.data[,-which(apply(all.data,2, anyNA))] # Remove columns with NAs
#   all.data <- apply(all.data, 2, as.numeric)  # Make numberic
#   ecoli.col <- which(colnames(all.data)=="ECIDX_G") # Select E.coli column
#   all.data[,ecoli.col] <- log10(all.data[,ecoli.col]) # Log transform E.coli
#   assign(paste0("test",i+45,".raw"), all.data)
#   test <- rnn(all.data, predict.col=ecoli.col, train.obs=ceiling(nrow(all.data)*.9))
#   test[,2] <- 10^test[,2] # Undo log transform
#   obj.list[[length(obj.list)+1]] <- test
# }




# Test N: Lag E.coli
# all.data <- cbind(all.data[2:nrow(all.data),], all.data[1:(nrow(all.data)-1),predict.col]) # lag ecoli

