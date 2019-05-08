library(doParallel)
registerDoParallel(detectCores()) # use multicore, set to the number of our cores

library(foreach)
# results <- foreach (j=1:(ncol(all.data[-predict.cols])-1), .combine=rbind) %dopar% {
results <- foreach (j=1:3, .combine=rbind) %dopar% {
  
  hidden.nodes <- j
  NN <- neuralnet::neuralnet(fmla, training.NN, hidden = hidden.nodes, linear.output = FALSE)
  
  ## Predict using NN
  predict.NN <-  neuralnet::compute(NN, testing.NN[,-predict.cols])
  predict.NN <- (predict.NN$net.result * (max(all.data[,predict.cols[predictor]]) - min(all.data[,predict.cols[predictor]]))) + min(all.data[,predict.cols[predictor]])
  
  # Calculate Root Mean Square Error (RMSE)
  RMSE.NN <- (sum((testing.data[,predict.cols[predictor]] - predict.NN)^2) / nrow(testing.data)) ^ 0.5
  
  # matrix(c(hidden.nodes,RMSE.NN), ncol=2, dimnames = list(c(NULL), c("Nodes","RMSE")))
  data.frame(c(hidden.nodes, RMSE.NN))
}