min.max.norm <- function(train, test=NULL) {
  if(is.vector(train)) train <- data.frame(train)
  if(is.null(test)) test <- train
  ma <- apply(train,2,max)
  mi <- apply(train,2,min)
  results <- do.call("cbind", lapply(names(ma), function(var) {
    (test[,var]-mi[var])/(ma[var]-mi[var])
  }))
  if(length(rownames(test))>0) rownames(results) <- rownames(test)
  return(results)
}

# Function to create a rnn, train and test in parallel/rolling window
rollingRNN <- function(all.data, predict.col, train.obs,
                act.function="softsign", n_epoch=500, scale=TRUE, n_nodes=NULL) {
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
  end <- start+nThreads-1
  if(end > nrow(all.data)) end <- nrow(all.data)
  
  # Start loop
  while(start <= nrow(all.data)) {
    
    if(end == nrow(all.data)) nThreads <- end-start
    if(nThreads<1) nThreads <- 1
    
    # Create doSNOW compute cluster
    cluster = makeCluster(nThreads, type = "SOCK")
    # register the cluster
    registerDoSNOW(cluster)
    
    predictions <- foreach(test.obs=seq(start, end),
                           .packages = c("dplyr", "keras","zoo"), .export=c("min.max.norm")) %dopar% {
      # Setup training data
      train.start <- test.obs-train.obs
      train.end <- test.obs-1
      # Scale training data
      # train.mean <- apply(all.data[train.start:train.end,],2,mean)
      # train.sd <- apply(all.data[train.start:train.end,],2,sd)
      # Create 3D array of training data to simulate time sequence in batches of 1
      # train.x <- scale(all.data[train.start:train.end,], center=train.mean, scale=train.sd)[,-predict.col]
      if(scale) train.x <- min.max.norm(all.data[train.start:train.end,-predict.col])
      if(!scale) train.x <- all.data[train.start:train.end,-predict.col]
      
      cols2remove <- which(apply(train.x,2,anyNA))
      if(length(cols2remove>0)) train.x <- train.x[,-cols2remove]
      
      
      train.x <- simplify2array(list(train.x))
      if(length(dim(train.x)) == 3) {
        
        train.y <- simplify2array(list(min.max.norm(all.data[train.start:train.end,predict.col])))
        n_batch <- dim(train.x)[1]
        if(is.null(n_nodes)) n_nodes <- c(dim(train.x)[2], round(dim(train.x)[2]*3))
        
        # Setup model
        # model <- keras_model_sequential() %>%
        #   layer_lstm(units=dim(train.x)[2], input_shape=list(dim(train.x)[2],dim(train.x)[3]), activation = act.function) %>%
        #   layer_dense(units = 1)
        model <- keras_model_sequential() %>%
          layer_lstm(units=n_nodes[1], 
                      input_shape=list(dim(train.x)[2],dim(train.x)[3]), 
                      activation = act.function,
                     return_sequences=TRUE)
        for(i in 2:length(n_nodes)) {
          layer_lstm(object=model,
                      units=n_nodes[i], 
                      activation = act.function)
        }
        layer_dense(object=model, units = 1)
        
        
        
        model %>% compile(
          optimizer = optimizer_rmsprop(),
          loss = "mean_squared_error"
        )
        
        # Train model
        # history.loss <- vector(length=n_epoch)
        for(i in seq(1,n_epoch)) {
          history <- model %>% fit(
            x=train.x,
            y=train.y,
            batch_size=n_batch,
            epochs=1,
            shuffle=FALSE, 
            stateful=TRUE
          )
          # history.loss[i] <- history$metrics$loss
          model %>% reset_states()
        }
        
        # # Testing...
        #   history <- model %>% fit(
        #     x=train.x,
        #     y=train.y,
        #     batch_size=n_batch,
        #     epochs=n_epoch,
        #     shuffle=FALSE, 
        #     stateful=TRUE
        #   )
        #   validation <- model %>% fit(
        #     x=train.x,
        #     y=train.y,
        #     batch_size=n_batch,
        #     epochs=n_epoch,
        #     shuffle=FALSE, 
        #     stateful=TRUE
        #   )
        # plot(history$metric$loss,pch=20, ylim=c(0,1));points(validation$metrics$loss, col="red", pch=20)
        
        # Calculate model fit to training data
        validation <- model %>% predict(
          x=train.x,
          batch_size=1
        )
        
        r2 <- cor(validation, train.y)^2
        
        # Setup testing data
        if(scale) test.x <- min.max.norm(all.data[train.start:train.end,-predict.col],all.data[test.obs,-predict.col] )
        if(!scale) test <- all.data[test.obs,-predict.col]
        
        if(length(cols2remove>0)) test.x <- test.x[-cols2remove]
        # test.x <- test.x[,which(colnames(test.x) %in% colnames(train.x[,,1]))]
        # test.y <- scale(all.data, center=train.mean, scale=train.sd)[test.obs,predict.col]
        
        # Predict E coli
        pred <- model %>% predict(
          x=array_reshape(test.x, c(1,length(test.x),1)),
          batch_size=1
        )
        
        if(scale) pred <- pred*(max(all.data[train.start:train.end,predict.col]) - min(all.data[train.start:train.end,predict.col]))+ min(all.data[train.start:train.end,predict.col])
        
        
          data.frame("R2"=r2,
                   "Prediction"=pred,
                   "Actual"=all.data[test.obs,predict.col],
                   "Persistance"=all.data[(test.obs-2),predict.col])
      } else {
          data.frame("R2"=NA, 
                   "Prediction"=NA,
                   "Actual"=NA,
                   "Persistance"=NA)
      }
    }
    
    # stop cluster and remove clients
    stopCluster(cluster)
    # insert serial backend, otherwise error in repetetive tasks
    registerDoSEQ()
    
    print(paste(Sys.time(),": Observations", start, "to", end, "completed."))
    
    all.predictions[[length(all.predictions)+1]] <- do.call("rbind", predictions)
    start <- end + 1
    end <- start + nThreads - 1
    if(end > nrow(all.data)) end <- nrow(all.data)
  } # while loop
  
  all.predictions <- do.call("rbind", all.predictions)
  return(all.predictions)
}
