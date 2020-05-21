# Function to create a rnn, train and test in parallel/rolling window
randomANN <- function(all.data,
                       predict.col,
                       act.function="softsign", 
                       n_epoch=3000,
                       n_nodes=NULL, 
                       scale=TRUE) {
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
  nThreads <- detectCores(logical = TRUE)
  
  print(paste(Sys.time(),": Starting training..."))
  
  # Initialize where each parallel iteration is to be saved
  all.predictions <- list()
  
  # Initialize start and end testing observations
  start <- 1
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
                             # Scale training data
                             train.sd <- apply(all.data[-test.obs,],2,sd)
                             if(length(which(train.sd==0))) {
                               predict.var <- colnames(all.data)[predict.col]
                               all.data <- all.data[,-which(train.sd==0)]
                               train.sd <- apply(all.data[-test.obs,],2,sd)
                               predict.col <- which(colnames(all.data)==predict.var)
                               
                             }
                             # train.mean <- apply(all.data[-test.obs,],2,mean)
                             
                             # Create 3D array of training data to simulate time sequence in batches of 1
                             # if(scale) train.x <- scale(all.data[-test.obs,], center=train.mean, scale=train.sd)[,-predict.col]
                             if(scale) train.x <- min.max.norm(all.data[-test.obs,-predict.col])
                             if(!scale) train.x <- all.data[-test.obs,-predict.col]
                             if(length(which(apply(train.x,2,anyNA)))>0) train.x <- train.x[,-which(apply(train.x,2,anyNA))]
                             
                             train.x <- simplify2array(list(train.x))
                             
                             
                             
                             if(length(dim(train.x)) == 3) {
                               # if(scale) train.y <- simplify2array(list(scale(all.data[-test.obs,], center=train.mean, scale=train.sd)[,predict.col]))
                               if(scale) train.y <- simplify2array(list(min.max.norm(all.data[-test.obs,])[,predict.col]))
                               if(!scale) train.y <- simplify2array(list(all.data[-test.obs,predict.col]))
                               
                               n_batch <- dim(train.x)[1]
                               if(is.null(n_nodes)) n_nodes <- c(dim(train.x)[2], round(dim(train.x)[2]*3))
                               
                               # Setup model
                               model <- keras_model_sequential() %>%
                                 layer_dense(units=n_nodes[1], 
                                             input_shape=dim(train.x)[2], 
                                             activation = act.function)
                               for(i in 2:length(n_nodes)) {
                                 layer_dense(object=model,
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
                               # for(i in seq(1,n_epoch)) {
                               history <- model %>% fit(
                                 x=train.x[,,1],
                                 y=train.y,
                                 batch_size=n_batch,
                                 epochs=n_epoch,
                                 shuffle=TRUE
                                 # stateful=TRUE
                               )
                               # history.loss[i] <- history$metrics$loss
                               # model %>% reset_states()
                               # }
                               
                               # # Testing...
                               #   history <- model %>% fit(
                               #     x=train.x[,,1],
                               #     y=train.y,
                               #     batch_size=n_batch,
                               #     epochs=n_epoch,
                               #     shuffle=FALSE,
                               #     stateful=TRUE
                               #   )
                               #   validation <- model %>% fit(
                               #     x=train.x[,,1],
                               #     y=train.y,
                               #     batch_size=n_batch,
                               #     epochs=n_epoch,
                               #     shuffle=FALSE,
                               #     stateful=TRUE
                               #   )
                               # plot(history$metric$loss,pch=20, ylim=c(0,1));points(validation$metrics$loss, col="red", pch=20)
                               # plot(history$metric$loss,pch=20, ylim=c(0,1))
                               
                               # Calculate model fit to training data
                               validation <- model %>% predict(
                                 x=train.x[,,1],
                                 batch_size=1
                               )
                               
                               r2 <- cor(validation, train.y)^2;r2
                               
                               # Setup testing data
                               # if(scale) test.x <- scale(all.data, center=train.mean, scale=train.sd)[test.obs,-predict.col]
                               if(scale) test.x <- min.max.norm(all.data[-test.obs,-predict.col], all.data[test.obs,-predict.col])
                               if(!scale) test.x <- all.data[test.obs,-predict.col]
                               # test.x <- test.x[,which(colnames(test.x) %in% colnames(train.x[,,1]))]
                               # test.y <- scale(all.data, center=train.mean, scale=train.sd)[test.obs,predict.col]
                               
                               # Predict E coli
                               pred <- model %>% predict(
                                 x=matrix(test.x, nrow=1),
                                 batch_size=1
                               )
                               
                               # if(scale) pred <- pred*train.sd[predict.col]+train.mean[predict.col]
                               if(scale) pred <- pred*(max(all.data[-test.obs,predict.col]) - min(all.data[-test.obs,predict.col]))+ min(all.data[-test.obs,predict.col])
                               
                               data.frame("R2"=r2,
                                          "Prediction"=pred,
                                          "Actual"=all.data[test.obs,predict.col])
                             } else {
                               data.frame("R2"=NA, 
                                          "Prediction"=NA,
                                          "Actual"=NA)
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

