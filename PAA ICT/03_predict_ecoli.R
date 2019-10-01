model.data <- ecoli.CT[,colnames(ecoli.CT) %in% c("NUMERIC_RESULT", "hour.of.day",
                         "DIS.North.Flow", "PAA.Setpoint",
                         "DIS.PAA.N.Upstream.Residual", "NSEC.Aerobic.SRT",
                         "NSEC.Effluent.NH3", "NSEC.Effluent.NO3",
                         "NSEC.Effluent.OP", "NSEC.Effluent.TSS",
                         "NSEC.Effluent.Flow", "HRT..min.","Log Removal")]
model.data <- apply(model.data, 2, as.numeric)
model.data <- data.frame(na.omit(model.data))
colnames(model.data) <- make.names(colnames(model.data))

# Variable to predict
predict.col.name <- c("Log.Removal")

# Prep data
predict.column <- which(colnames(model.data) == predict.col.name)
fmla <- as.formula(paste0(predict.col.name,"~",paste(colnames(model.data[,-predict.column]),collapse = "+")))

# Scale data using min-max method
max <- apply(model.data, 2, max)
min <- apply(model.data, 2, min)
scaled.data <- as.data.frame(scale(model.data, center = min, scale = max - min))

# Number of observations to train on
obs <- 170
percent.train = obs/nrow(model.data)

for (i in 1:(nrow(model.data)*(1-percent.train))) {
  # Set Training/Testing indices
  training.index <- seq(i,(nrow(model.data)*percent.train-1+i),by=1)
  training.data <- model.data[training.index,]
  testing.data <- model.data[-training.index,]
  
  # # Scale data using min-max method
  max <- apply(training.data, 2, max)
  min <- apply(training.data, 2, min)
  # scaled.data <- as.data.frame(scale(training.data, center = min, scale = max - min))
  # 
  # training.NN <- scaled.data[training.index,]
  # testing.NN <- scaled.data[-training.index,]
  
  training.NN <- training.data
  testing.NN <- testing.data
  
  nodes <- 2
  NN <- neuralnet::neuralnet(fmla, training.NN, hidden = nodes, linear.output = FALSE)
  predict.NN <-  neuralnet::compute(NN, testing.NN[i,])
  # predict.NN <- (predict.NN$net.result * (max[predict.col.name] - min[predict.col.name])) + min[predict.col.name]
  
  if(i == 1) results <- c(testing.data[i,predict.column],
                          predict.NN$net.result)
  if(i > 1) results <- rbind(results, c(testing.data[i,predict.column],
                                        predict.NN$net.result))
}
rownames(results) <- rownames(model.data)[-c(1:(nrow(model.data)*(percent.train)))]
colnames(results) <- c("Actual", "NN")