time.1 <- Sys.time()
setwd("C:\\Users\\knewhart\\Documents\\MWRD\\PAA ICT\\PAA_Ecoli_RNN\\real_time")
pkgs <- c("xts", "readxl", "lubridate", "stringr", "xlsx", "doSNOW", "foreach", "devtools", "dplyr", "keras")
sapply(pkgs, function(x) library(x, character.only = TRUE))
sapply(list.files("src", full.names = TRUE), source)

##### Setup training data ####
if(!("training-data.RData" %in% list.files("data/"))) {
  # 1. Import 2019 PAA sampling data (offline)
  if(!("paa.RData" %in% list.files("data/"))) {
    paa <- import2019() # UTC
    save(paa, file="data/paa.RData")
  } else {
    load("data/paa.RData")
  }
  
  # 2. Import process data (online, instantaneous interpolated values)
  if(!("process.RData" %in% list.files("data/"))) {
    process <- importProcess(times=index(paa))
    save(process, file="data/process.RData")
  } else {
    load("data/process.RData")
  }
  
  # 3. Calculate HRT
  hrt <- sapply(1:ncol(paa), function(i) calculateHRT(flow=process[,"North Flow to Dis"],c=paa[,i]))
  colnames(hrt) <- gsub("PAA","HRT",colnames(paa))
  
  # 4. Calculate CT
  dose <- process$PAA_N_FlowRate*0.15*1.16/(process$`North Flow to Dis`)
  flow <- process[,"North Flow to Dis"]
  t_f <- 2690.3*flow^-0.959 # HRT of basin
  data <- array(data=NA, 
                dim=c(ncol(paa), # 2 or 4 rows (sampling location)
                      2, # 2 columns (C and t)
                      nrow(paa))) # matrices (each sampling event)
  results <- matrix(data=NA,
                    nrow=nrow(paa),ncol=1)
  for(j in 1:dim(data)[3]) {
    data[,,j] <- cbind(as.numeric(paa[j,]),as.numeric(hrt[j,]))
    if(any(data[,,j]==0)) {
      data[which(data[,,j]==0),,j] <- NA 
    }
  }
  
  for(j in 1:dim(results)[1]) {
    C <- data[,1,j]
    t <- data[,2,j]
    exponential.mod <- lm(log(C)~ t)
    # r2 <- cor(x=na.omit(C),y=na.omit(exp(t*exponential.mod$coefficients[2]+exponential.mod$coefficients[1])))
    C0 <- dose[j]
    D <- C0-exp(as.numeric(exponential.mod$coefficients)[1])
    k <- -as.numeric(exponential.mod$coefficients)[2]
    CT <- (C0-D)/k-(C0-D)/k*exp(-k*t_f[j])
    # results[j,] <- c(as.numeric(exponential.mod$coefficients), C0, CT, r2)
    results[j,] <- CT
    
  }
  ct <- results
  
  # 5. Merge all 
  merged.data <- cbind(ct, process, hrt)
  save(merged.data, file="data/training-data.RData")
} else {
  load("data/training-data.RData")
}

##### Build ANN CT model #####
# 1. Setup model parameters
predict.var <- "ct"
act.function <- "softsign"
n_epoch <- 250
n_nodes <- NULL
scale <- TRUE


# 2. Scale training data
predict.col <- which(colnames(merged.data)==predict.var)
if(scale) {
  train.x <- min.max.norm(merged.data[,-predict.col])
  train.y <- min.max.norm(merged.data[,predict.col])
}
if(!scale) {
  train.x <- merged.data[,-predict.col]
  train.y <- merged.data[,predict.col]
}
if(length(which(apply(train.x,2,anyNA)))>0) {
  cols.removed <- which(apply(train.x,2,anyNA))
  vars.removed <- colnames(merged.data[,-predict.col])[cols.removed]
  train.x <- train.x[,-cols.removed]
}

train.x <- array(data=as.matrix(train.x), dim=c(nrow(train.x), ncol(train.x),1))
train.y <- array(data=as.matrix(train.y), dim=c(nrow(train.y), 1,1))

# 3. Setup model
if(is.null(n_nodes)) n_nodes <- c(dim(train.x)[2], round(dim(train.x)[2]*3))
tryCatch({
  model <- load_model_tf("model")
}, error = function(e) {
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
  
  # 4. Train model
  history <- model %>% fit(
    x=train.x[,,1],
    y=train.y,
    epochs=n_epoch,
    shuffle=TRUE
  )
  model %>% save_model_tf("model")
})

##### Setup testing data ####
# 1. Import process data (online, instantaneous interpolated values)
t <- Sys.time()
process <- importProcess(times=t)

# 2. Calculate HRT
dummy.df.ls <- list(data.frame("PAA.2019.1"=NA),
                    data.frame("PAA.2019.2"=NA),
                    data.frame("PAA.2019.3"=NA),
                    data.frame("PAA.2019.4"=NA))
hrt <- data.frame(t(unlist(lapply(dummy.df.ls, function(i) calculateHRT(flow=process[,"North Flow to Dis"],c=i)))))
colnames(hrt) <- paste0("HRT.2019.",1:4)

# 3. Merge data
test.data <- cbind(process, hrt)

# 4. Scale test data
if(scale) {
  test.x <- min.max.norm(train=merged.data[,-predict.col], test=test.data)
}
if(!scale) {
  test.x <- test.data
}
test.x <- test.x[,!(colnames(test.data) %in% vars.removed)]
test.x <- as.matrix(t(test.x))
test.x <- array(data=test.x, dim=c(nrow(test.x), ncol(test.x),1))

##### Predict CT #####
pred <- model %>% predict(
  x=matrix(test.x, nrow=1)
)

if(scale) pred <- min.max.norm(merged.data[,predict.col], pred, unscale = TRUE)

##### Write to PI #####
piPush(tag="\\\\applepi\\test_knewhart", time=t, val=pred)

# And csv for results

# Save results
flow <- process[,"North Flow to Dis"] # Flow to disinfection basin
dose <- process$PAA_N_FlowRate*0.15*1.16/flow # PAA dose
hrt <- 2690.3*flow^-0.959 # HRT of basin
CT <- pred
# nls.results <- nls(CT~(dose-demand)/k-(dose-demand)/k*exp(-k*hrt),
#                    start=list(demand=1.28,k=0.023),
#                    lower=c(1,0.01),
#                    upper=c(1.5,0.03),
#                    algorithm = "port")
model.fit <- data.frame("System.Time" = as.character(Sys.time()),
                        "Prediction.Time" = as.character(t), 
                        "CT" = pred,
                        "HRT" = hrt,
                        "Dose" = dose,
                        stringsAsFactors = FALSE)
if(!("ModelResults.csv" %in% list.files())) {
  write.csv(model.fit, file="ModelResults.csv", row.names = FALSE)
} else {
  all.model.fit <- read.csv(file="ModelResults.csv")
  all.model.fit <- taRifx::remove.factors(all.model.fit)
  all.model.fit <- rbind(data.frame(all.model.fit, stringsAsFactors = FALSE), model.fit)
  write.csv(all.model.fit, file="ModelResults.csv", row.names = FALSE)
}
time.2 <- Sys.time()
time.2-time.1