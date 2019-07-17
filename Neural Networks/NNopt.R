NNopt <- function(all.data, predict.col.name, percent.train = 0.8, training.index=NULL) {
  ## Prep data
  predict.column <- which(colnames(all.data) == predict.col.name)
  all.data <- na.omit(data.frame(all.data))
  
  ## Scale data using min-max method
  max <- apply(all.data, 2, max)
  min <- apply(all.data, 2, min)
  scaled.data <- as.data.frame(scale(all.data, center = min, scale = max - min)) #Testing
  # scaled.data <- all.data
  
  ## Create training and test datasets
  if(is.null(training.index)) {
    set.seed(Sys.time())
    training.index <- sample(seq_len(nrow(all.data)), size=(percent.train*nrow(all.data)))
  }
  training.data <- all.data[training.index,]
  testing.data <- all.data[-training.index,]
  training.NN <- scaled.data[training.index,]
  testing.NN <- scaled.data[-training.index,]
  
  fmla <- as.formula(paste0(colnames(all.data)[predict.column],"~",
                            paste(colnames(all.data)[-predict.column], collapse= "+")))
  
  require(parallel)
  require(neuralnet)
  require(doSNOW)
  # detect cores with parallel() package
  nCores <- detectCores(logical = FALSE)
  # detect threads with parallel()
  nThreads<- detectCores(logical = TRUE)
  
  # Create doSNOW compute cluster
  cluster = makeCluster(nThreads, type = "SOCK")
  class(cluster);
  
  # register the cluster
  registerDoSNOW(cluster)
  
  
  ## Find optimum number of hidden nodes
  results <- foreach::foreach(i = 1:(ncol(all.data)-1), .combine = rbind) %dopar% {
    
    hidden.nodes <- i
    NN <- neuralnet::neuralnet(fmla, training.NN, hidden = hidden.nodes, linear.output = FALSE)
    
    ## Predict using NN
    predict.NN <-  neuralnet::compute(NN, testing.NN[,-predict.column])
    predict.NN <- predict.NN$net.result
    # predict.NN <- (predict.NN$net.result * (max(all.data[,predict.column]) - min(all.data[,predict.column]))) + min(all.data[,predict.column])
    
    # Calculate Root Mean Square Error (RMSE)
    RMSE.NN <- (sum((testing.data[,predict.column] - predict.NN)^2) / nrow(testing.data)) ^ 0.5
    
    # Calculate R2
    R2.NN <- cor(testing.data[,predict.column], predict.NN)^2
    
    data.frame("Hidden Nodes" = hidden.nodes,
               "RMSE" = RMSE.NN,
               "R2" = R2.NN)
  }
  
  
  # stop cluster and remove clients
  stopCluster(cluster)
  
  # insert serial backend, otherwise error in repetetive tasks
  registerDoSEQ()
  
  # clean up a bit.
  invisible(gc); remove(nCores); remove(nThreads); remove(cluster); 
  
  # Plot number of nodes vs error
  plot(x=as.numeric(results[,"Hidden.Nodes"]), 
       y=results[,c("RMSE")], 
       type = "b", 
       frame = FALSE,
       pch = 19,
       col = "red", 
       xlab = "Number of Hidden Nodes", 
       ylab = "RMSE", 
       lty = 1, 
       lwd = 1)
  par(new=T)
  plot(x=as.numeric(results[,"Hidden.Nodes"]), 
        y=results[,c("R2")], 
        pch = 18, 
        col = "blue", 
        type = "b", 
        lty = 2, 
        lwd = 1, 
        yaxt="n")
  axis(side=4)
  
  
  
  ## Build best NN
  hidden.nodes <- min(results[,c("RMSE")])
  hidden.nodes <- which(results[,c("RMSE")] == hidden.nodes)
  hidden.nodes <- as.numeric(results[hidden.nodes,"Hidden.Nodes"])
  
  NN <- neuralnet(fmla, training.NN, hidden = hidden.nodes, linear.output = FALSE)
  
  ## Predict using NN
  predict.NN <-  neuralnet::compute(NN, testing.NN[,-predict.column])
  # predict.NN <- (predict.NN$net.result * (max(all.data[,predict.column]) -
                                            min(all.data[,predict.column]))) + 
    # min(all.data[,predict.column]) # Testing
  predict.NN <- predict.NN$net.results
  
  # testing <- (testing.data[,predict.column] * (max(all.data[,predict.column]) -
                                                 min(all.data[,predict.column]))) + 
    # min(all.data[,predict.column])
  testing <- testing.data[,predict.column]
  
  # Calculate Root Mean Square Error (RMSE)
  RMSE.NN <- (sum((testing - predict.NN)^2) / nrow(testingunscaled)) ^ 0.5
  
  
  return(list("NN" = NN,
              "Actual" = testing.data[,predict.column],
              "Predicted" = predict.NN,
              "RMSE" = RMSE.NN))
} 