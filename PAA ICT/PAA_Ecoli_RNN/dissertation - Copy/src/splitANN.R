# Function to create a rnn, train and test in parallel/rolling window
splitANN <- function(all.data,
                       predict.col,
                       act.function="softsign", 
                       n_epoch=3000,
                       n_nodes=NULL,
                       node_multiply=3,
                       scale=TRUE,
                      train_split=0.5,
                     test.obs=seq(round(nrow(all.data)*train_split+1),nrow(all.data))) {
  # Load required packages
  pkgs <- c("xts", "dplyr", "parallel", "doSNOW", "foreach")
  sapply(pkgs, require, character.only = TRUE)
  if(!("keras" %in% rownames(installed.packages()))) {
    install.packages("keras")
    library(keras)
    keras::install_keras()
    library(keras)
  }
  
  
  
  print(paste(Sys.time(),": Starting training..."))
  
  # Initialize where each parallel iteration is to be saved
  all.predictions <- list()
  
   # Scale training data
   train.sd <- apply(all.data[-test.obs,],2,sd)
   if(length(which(train.sd==0))) {
     predict.var <- colnames(all.data)[predict.col]
     all.data <- all.data[,-which(train.sd==0)]
     train.sd <- apply(all.data[-test.obs,],2,sd)
     predict.col <- which(colnames(all.data)==predict.var)
     
   }
                             
   # Create 3D array of training data to simulate time sequence in batches of 1
   # if(scale) train.x <- scale(all.data[-test.obs,], center=train.mean, scale=train.sd)[,-predict.col]
   if(scale) train.x <- min.max.norm(all.data[-test.obs,-predict.col])
   if(!scale) train.x <- all.data[-test.obs,-predict.col]
   if(length(which(apply(train.x,2,anyNA)))>0) train.x <- train.x[,-which(apply(train.x,2,anyNA))]
   
   train.x <- array(data=as.matrix(train.x), dim=c(nrow(train.x), ncol(train.x),1))
  
   if(length(dim(train.x)) == 3) {
     # if(scale) train.y <- simplify2array(list(scale(all.data[-test.obs,], center=train.mean, scale=train.sd)[,predict.col]))
     
     if(scale) {
       train.y <- min.max.norm(all.data[-test.obs,predict.col])
     }
     if(!scale) {
       train.y <- all.data[-test.obs,predict.col]
     }
     train.y <- array(data=as.matrix(train.y), dim=c(nrow(train.y), 1,1))
     
     n_batch <- dim(train.x)[1]
     if(is.null(n_nodes)) n_nodes <- c(dim(train.x)[2], round(dim(train.x)[2]*node_multiply))
     
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
       # , validation_split=0.33
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
     sse <- sum((validation-train.y[,,1])^2)
     n <- dim(validation)[1]
     p <- count_params(model)
     aic <- n*log(sse/n)+2*p
     bic <- n*log(sse/n)+p*log(n)
     
     # Setup testing data
     # if(scale) test.x <- scale(all.data, center=train.mean, scale=train.sd)[test.obs,-predict.col]
     if(scale) test.x <- min.max.norm(all.data[-test.obs,-predict.col], all.data[test.obs,-predict.col])
     if(!scale) test.x <- all.data[test.obs,-predict.col]
     # test.x <- test.x[,which(colnames(test.x) %in% colnames(train.x[,,1]))]
     # test.y <- scale(all.data, center=train.mean, scale=train.sd)[test.obs,predict.col]
     
     # Predict E coli
     pred <- model %>% predict(
       x=matrix(test.x, nrow=nrow(test.x)),
       batch_size=nrow(test.x)
     )
     
     # if(scale) pred <- pred*train.sd[predict.col]+train.mean[predict.col]
     if(scale) pred <- pred*(max(all.data[-test.obs,predict.col]) - min(all.data[-test.obs,predict.col]))+ min(all.data[-test.obs,predict.col])
     
     return(data.frame("R2"=r2,
                "Prediction"=pred,
                "Actual"=all.data[test.obs,predict.col],
                "AIC"=aic,
                "BIC"=bic))
   } else {
     return(data.frame("R2"=NA, 
                "Prediction"=NA,
                "Actual"=NA,
                "AIC"=NA,
                "BIC"=NA))
   }
}

