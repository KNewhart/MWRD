# Function to create a rnn, train and test in parallel/rolling window
rolling.rnn <- function(all.data, predict.col, train.obs,
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
  
  # print(paste(Sys.time(),": Starting training..."))
  
  # Initialize where each parallel iteration is to be saved
  all.predictions <- list()
  # Initialize start and end testing observations
  start <- train.obs+1
  end <- start+nThreads-1
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
      train.x <- scale(all.data[train.start:train.end,], center=train.mean, scale=train.sd)[,-predict.col]
      train.x <- train.x[,-which(apply(train.x,2,anyNA))]
      train.x <- simplify2array(list(train.x))
      
      train.y <- simplify2array(list(scale(all.data[train.start:train.end,], center=train.mean, scale=train.sd)[,predict.col]))
      n_batch <- dim(train.x)[1]
      n_epoch <- 2000
      
      # Setup model
      model <- keras_model_sequential() %>%
        layer_lstm(units=dim(train.x)[2], input_shape=list(dim(train.x)[2],dim(train.x)[3]), activation = act.function) %>%
        layer_dense(units = 1)
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
      test.x <- scale(all.data, center=train.mean, scale=train.sd)[test.obs,-predict.col]
      test.x <- test.x[,which(colnames(test.x) %in% colnames(train.x[,,1]))]
      # test.y <- scale(all.data, center=train.mean, scale=train.sd)[test.obs,predict.col]
      
      # Predict E coli
      pred <- model %>% predict(
        x=array_reshape(test.x, c(1,length(test.x),1)),
        batch_size=1
      )
      
      # Calculate prediction error (in E. coli units)
      # error <- pred*train.sd[predict.col]+train.mean[predict.col]-as.numeric(test.y*train.sd[predict.col]+train.mean[predict.col])
      
      data.frame("R2"=r2, 
                 "Prediction"=pred*train.sd[predict.col]+train.mean[predict.col],
                 "Actual"=all.data[test.obs,predict.col],
                 "Persistance"=all.data[(test.obs-1),predict.col])
    } # parallel loop
    
    # stop cluster and remove clients
    stopCluster(cluster)
    # insert serial backend, otherwise error in repetetive tasks
    registerDoSEQ()
    
    print(paste(Sys.time(),": Observations", start, "to", end, "completed."))
    
    all.predictions[[length(all.predictions)+1]] <- predictions
    start <- end + 1
    end <- start + nThreads - 1
    if(end > nrow(all.data)) end <- nrow(all.data)
  } # while loop
  
  all.predictions <- do.call("rbind", all.predictions)
  return(all.predictions)
}