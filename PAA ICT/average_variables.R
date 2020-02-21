setwd("C:\\Users\\KNewhart\\Documents\\GitHub\\MWRD\\PAA ICT")

# Initialize R-PI connection
library(piwebapi)

# Login information
useKerberos <- TRUE
username <- "knewhart"
password <- "Lunabear2@"
validateSSL <- TRUE
debug <- TRUE
piWebApiService <- piwebapi$new("https://pivision/piwebapi", useKerberos, username, password, validateSSL, debug)

# Function to import pi data
pull.and.save <- function(tag, label=NULL, save=TRUE, obj.return = FALSE, start='*-365d', end='*') {
  if(inherits(start, "POSIXct")) start <- paste0(as.character(format(lubridate::with_tz(start, tzone="UTC"),"%Y-%m-%d")),"T",
                                                 as.character(format(lubridate::with_tz(start, tzone="UTC"),"%H:%M:%S")),"Z")
  if(inherits(end, "POSIXct")) end <- paste0(as.character(format(lubridate::with_tz(end, tzone="UTC"),"%Y-%m-%d")),"T",
                                             as.character(format(lubridate::with_tz(end, tzone="UTC"),"%H:%M:%S")),"Z")
  pi.points <- piWebApiService$attribute$getByPath(path=tag)
  pi.data <- piWebApiService$stream$getRecorded(webId = pi.points$WebId, startTime=start, endTime=end)[[2]]
  pi.data <- do.call("rbind", lapply(pi.data, function(x) {
    time.obj <- paste(strsplit(as.character(x[1]), "T")[[1]][1], 
                      strsplit(strsplit(as.character(x[1]), "T")[[1]][2], "Z")[[1]][1])
    time.obj <- lubridate::with_tz(as.POSIXct(time.obj, tz="UTC"), tzone = Sys.timezone())
    value.obj <- x[[2]]
    if(length(value.obj) > 1) value.obj <- NA
    return(data.frame("Timestamp"=time.obj, 
                      "Value" = value.obj))
  })
  )
  all.pi.data <- na.omit(pi.data)
  if(is.null(all.pi.data)) return(NULL)
  
  # if(length(list.files("results/raw_data/active/"))>0) sapply(list.files("results/raw_data/active/", full.names = TRUE), file.remove)
  # file <- 1
  # write.csv(na.omit(pi.data), file=paste0("results/raw_data/active/",sprintf("%05d", file),".csv"), row.names = FALSE)
  # 
  # If not all of the data was pulled
  if(nrow(pi.data)==1000) {
    needsUpdating <- TRUE
    while(needsUpdating) {
      time.obj <- as.POSIXct(all.pi.data[nrow(all.pi.data), 1], tz=Sys.timezone())
      # time.obj <- as.POSIXct(pi.data[nrow(pi.data), 1], tz=Sys.timezone())
      begin <- paste0(as.character(format(lubridate::with_tz(time.obj, tzone="UTC"),"%Y-%m-%d")),"T",
                              as.character(format(lubridate::with_tz(time.obj, tzone="UTC"),"%H:%M:%S")),"Z")
      pi.data <- piWebApiService$stream$getRecorded(webId = pi.points$WebId, startTime=begin, endTime=end)[[2]]
      pi.data <- do.call("rbind", lapply(pi.data, function(x) {
        time.obj <- paste(strsplit(as.character(x[1]), "T")[[1]][1], 
                          strsplit(strsplit(as.character(x[1]), "T")[[1]][2], "Z")[[1]][1])
        time.obj <- lubridate::with_tz(as.POSIXct(time.obj, tz="UTC"), tzone = Sys.timezone())
        value.obj <- x[[2]]
        if(length(value.obj) > 1) value.obj <- NA
        return(data.frame("Timestamp"=time.obj, 
                          "Value" = value.obj))
      })
      )
      # file <- file+1
      # write.csv(na.omit(pi.data), file=paste0("results/raw_data/active/",sprintf("%05d", file),".csv"), row.names = FALSE)
      all.pi.data <- rbind(all.pi.data, na.omit(pi.data))
      if(nrow(pi.data) < 1000) needsUpdating <- FALSE
    }
  }
  
  # data.list <- lapply(list.files("results/raw_data/active/", full.names = TRUE), function(x) {read.csv(file=x,header=T)})
  # all.pi.data <- Reduce(function(...) {merge(..., all=T)}, data.list)

  file.name <- gsub("[.]", "_", make.names(tail(strsplit(tag,"[\\]")[[1]],n=1)))
  if(save && is.null(label)) write.csv(all.pi.data, file=paste0("results/raw_data/",file.name,".csv"), row.names = FALSE)
  if(save && !is.null(label)) write.csv(all.pi.data, file=paste0("results/raw_data/",label,".csv"), row.names = FALSE)
  if(obj.return) return(all.pi.data)
}

# Import PI tags
data.parameters <- read.csv("src/data-parameters.csv", stringsAsFactors = FALSE)
# Make unique tag names
tag.names <- sapply(data.parameters[,1], function(x) tail(strsplit(tail(strsplit(x,"[\\]")[[1]],n=1),"[|]")[[1]],n=1))
names(tag.names) <- NULL
duplicates <- names(which(sapply(unique(tag.names), function(x) length(which(x==tag.names)))>1))
for(i in which(tag.names %in% duplicates)) {
  tag.names[i] <- paste(strsplit(tail(strsplit(data.parameters[i,1],"[\\]")[[1]],n=1),"[|]")[[1]], collapse=" ")
}
# Pull E coli data
master.file <- pull.and.save(tag=data.parameters[1,1], start='*-763d', end='*-5d', obj.return = TRUE, save=FALSE)
write.csv(master.file, file="results/Ecoli.csv")
master.timestamps <- master.file[,1]

# Compile all averaged or raw data (takes 1.5 hours to run serialized)
# Initialize parallelization
{
  library(parallel)
  # detect threads with parallel()
  nThreads<- detectCores(logical = TRUE)
  # Create doSNOW compute cluster
  cluster = makeCluster(nThreads, type = "SOCK")
  # register the cluster
  registerDoSNOW(cluster)
}
library(foreach)

foreach(p=1:length(tag.names),.packages = c("xts", "lubridate", "piwebapi")) %dopar% {
# for(p in 1:length(tag.names)) {
  print(paste("Starting variable", p))
  file <- rep(NA, nrow(master.file))
  file.avg <- rep(NA, nrow(master.file))

  avg <- data.parameters[p,2]
  if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="m") d <- lubridate::dminutes(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
  if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="h") d <- lubridate::dhours(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))

  for(t in 1:length(master.timestamps)) {
    print(paste("Starting timestamp", t))
    start <- as.POSIXct(master.timestamps[t]) - as.numeric(d)
    end <- as.POSIXct(master.timestamps[t])

    test <- pull.and.save(tag=data.parameters[p,1], start=start, end=end, save=FALSE, obj.return = TRUE)

    if((nrow(test)==0) || (length(test)==0)) {
      file[t] <- NA
      file.avg[t] <- NA
    } else {

        file[t] <- as.numeric(test[nrow(test),2])

        weights <- as.numeric(difftime(test[,1], start, units="secs"))
        if(nrow(test) > 1) {
          for(i in length(weights):2) weights[i] <- weights[i]-weights[i-1]
          file.avg[t] <- weighted.mean(test[,2], w=weights/sum(weights))
        } else {
          file.avg[t] <- as.numeric(test[,2])
        }

    }
  }
  write.csv(cbind(master.timestamps, file), file=paste0("results/raw_data/",tag.names[p],".csv"))
  write.csv(cbind(master.timestamps, file.avg), file=paste0("results/averages/",tag.names[p],".csv"))
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


# Read in and compile averaged data
setwd("C:\\Users\\KNewhart\\Documents\\GitHub\\MWRD\\PAA ICT")

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


# 
# 
# 
# library(xts)
# master.zoo <- zoo(master.data, master.timestamps)
# 
# # Plot it
# par(mfrow=c(3,1), mar=c(2,4,2,.1), oma=c(0,0,1,0), cex=1.05)
# 
# y <- master.zoo$ECIDX_G
# n <- which(format(time(y), "%d")=="01")
# plot(y, xlab = "", ylab="E.coli", xaxt="n")
# 
# axis.ticks <- seq(as.POSIXct(paste(as.Date(range(time(y))[1]),"00:00")),
#                   as.POSIXct(paste(as.Date(range(time(y))[2]),"00:00")), by=60*60*24)
# axis.ticks <- axis.ticks[which(format(axis.ticks, "%d")=="01")]
# axis(1, at=axis.ticks, labels=format(axis.ticks, "%m-%d"))
# 
# predict.col <- which(colnames(master.zoo)=="ECIDX_G")
# master.zoo[,predict.col] <- log10(master.zoo[,predict.col]) # Log transform Ecoli
# 
# y <- master.zoo$ECIDX_G
# plot(y, xlab = "", ylab="Log E.coli", xaxt="n")
# axis(1, at=axis.ticks, labels=format(axis.ticks, "%m-%d"))
# 
# y <- rollmean(master.zoo$ECIDX_G,30, align = "right")
# x <- zoo(rep(NA, length(which(!(index(master.zoo$ECIDX_G) %in% index(y))))),
#          index(master.zoo$ECIDX_G[which(!(index(master.zoo$ECIDX_G) %in% index(y)))]))
# y <- rbind(x,y)
# plot(y, xlab = "", ylab="30-obs avg log E.coli", xaxt="n")
# axis(1, at=axis.ticks, labels=format(axis.ticks, "%m-%d"))
# 
# mtext(paste("All E.coli Data from",as.Date(range(time(y))[1]),"to",as.Date(range(time(y))[2])),side=3,outer=TRUE,line=-1,font=2)
# 
# 
# 
# ## Just plot phosphorus
# master.zoo <- zoo(master.data, master.timestamps)
# ## Remove rows that are not in the phosphorus dataset
# rows2keep <- which(!is.na(master.zoo[,"PW_FC24"]))
# master.zoo <- master.zoo[rows2keep,]
# # ## Remove cols that have NAs
# # master.zoo <- master.zoo[,-which(apply(master.zoo,2, anyNA))] # remove NAs
# 
# # Plot it
# par(mfrow=c(3,1), mar=c(2,4,2,.1), oma=c(0,0,1,0), cex=1.05)
# 
# y <- master.zoo$ECIDX_G
# n <- which(format(time(y), "%d")=="01")
# plot(y, xlab = "", ylab="E.coli", xaxt="n")
# 
# axis.ticks <- seq(as.POSIXct(paste(as.Date(range(time(y))[1]),"00:00")),
#                   as.POSIXct(paste(as.Date(range(time(y))[2]),"00:00")), by=60*60*24)
# axis.ticks <- axis.ticks[which(format(axis.ticks, "%d")=="01")]
# axis(1, at=axis.ticks, labels=format(axis.ticks, "%m-%d"))
# 
# predict.col <- which(colnames(master.zoo)=="ECIDX_G")
# master.zoo[,predict.col] <- log10(master.zoo[,predict.col]) # Log transform Ecoli
# 
# y <- master.zoo$ECIDX_G
# plot(y, xlab = "", ylab="Log E.coli", xaxt="n")
# axis(1, at=axis.ticks, labels=format(axis.ticks, "%m-%d"))
# 
# y <- rollmean(master.zoo$ECIDX_G,30, align = "right")
# x <- zoo(rep(NA, length(which(!(index(master.zoo$ECIDX_G) %in% index(y))))),
#          index(master.zoo$ECIDX_G[which(!(index(master.zoo$ECIDX_G) %in% index(y)))]))
# y <- rbind(x,y)
# plot(y, xlab = "", ylab="30-obs avg log E.coli", xaxt="n")
# axis(1, at=axis.ticks, labels=format(axis.ticks, "%m-%d"))
# 
# mtext(paste("PW & E.coli Data from",as.Date(range(time(y))[1]),"to",as.Date(range(time(y))[2])),side=3,outer=TRUE,line=-1,font=2)
# 
# 


##### NEURAL NETWORK #####
pkgs <- c("xts", "dplyr", "parallel", "doSNOW")
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



master.zoo <- zoo(master.data, master.timestamps)
## Remove rows that are not in the phosphorus dataset
rows2keep <- which(!is.na(master.zoo[,"PW_FC24"]))
master.zoo <- master.zoo[rows2keep,]
# ## Remove cols that have NAs
# rows2keep <- which(!is.na(master.zoo[,"NSEC.INF..FY.F25."]))
# master.zoo <- master.zoo[rows2keep,]
apply(master.zoo,2,function(x) length(which(is.na(x))))
master.zoo <- master.zoo[,-which(apply(master.zoo,2, anyNA))] # remove NAs

all.data <- master.zoo
all.data <- apply(all.data, 2, as.numeric)
cols <- colnames(all.data)
cols <- gsub("-", ".", gsub("[)]", ".", gsub("[(]", ".", gsub(" ", ".", cols))))
colnames(all.data) <- cols
# test.data <- all.data[(round(0.8*nrow(all.data))+1):nrow(all.data),]
# all.data <- all.data[1:round(0.8*nrow(all.data)),]
predict.col <- which(colnames(all.data)=="ECIDX_G")

# all.data <- cbind(all.data[2:nrow(all.data),], all.data[1:(nrow(all.data)-1),predict.col])

# detect threads with parallel()
# nThreads<- detectCores(logical = TRUE)
# # Create doSNOW compute cluster
# cluster = makeCluster(nThreads, type = "SOCK", outfile="")
# # register the cluster
# registerDoSNOW(cluster)
# Create model, add layers, and compile the model
# TO RUN THIS, NEED TO INSTALL MINICONDA
# WILL PROMPT INSTALL IF NONE FOUND
# all.r2 <- rep(NA,10)
# Initialize parallelization
{
  library(parallel)
  # detect threads with parallel()
  nThreads<- detectCores(logical = TRUE)
  # Create doSNOW compute cluster
  cluster = makeCluster(nThreads, type = "SOCK")
  # register the cluster
  registerDoSNOW(cluster)
}
library(foreach)
train.mean <- apply(all.data,2,mean)
train.sd <- apply(all.data,2,sd)
train.x <- scale(all.data, center=train.mean, scale=train.sd)[,-predict.col]
train.y <- scale(all.data, center=train.mean, scale=train.sd)[,predict.col]
all.r2 <- foreach(p=1:10, .combine='c', .packages = c("keras", "dplyr")) %dopar% {
# for(iteration in 1:length(all.r2)) {
model <- keras_model_sequential() %>%
  layer_dense(units=round((ncol(all.data))*2/3), 
              input_shape=c(NULL, ncol(all.data[,-predict.col])), 
              activation = "softplus") %>%
  layer_dense(units=ncol(all.data)) %>%
  layer_dense(units = 1)
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mean_squared_error"
)
history <- model %>% fit(
  x=train.x,
  y=train.y,
  batch_size=1,
  epochs=10
)
validation <- model %>% predict(
  x=train.x,
  batch_size=1
)
r2 <- cor(validation, train.y)^2;r2
all.r2[iteration] <- r2
}
average.parameters
mean(all.r2)
dim(all.data)





test.x <- scale(test.data, center=train.mean, scale=train.sd)[,-predict.col]
test.y <- scale(test.data, center=train.mean, scale=train.sd)[,predict.col]

predictions <- model %>% predict(
  x=test.x,
  batch_size=1
)
# stopCluster(cluster)

plot(train.y);points(validation, pch=20)
plot(test.y);points(predictions, pch=20)

# Test feature importance
for(i in 1:ncol(test.x)) {
  iteration.x <- test.x
  iteration.x[,-i] <- 0
  iteration.x[,i] <- seq(-3,3,length.out=nrow(test.x))
  predictions <- model %>% predict(
    x=iteration.x,
    batch_size=1
  )
  plot(x=iteration.x[,i],y=test.y)
}
# Nothing! Dammit...


# Try building a NN model one variable at a time?
master.zoo <- zoo(master.data, master.timestamps)
all.data <- master.zoo
all.data <- apply(all.data, 2, as.numeric)
cols <- colnames(all.data)
cols <- gsub("-", ".", gsub("[)]", ".", gsub("[(]", ".", gsub(" ", ".", cols))))
colnames(all.data) <- cols
predict.col <- which(colnames(all.data)=="ECIDX_G")
train.mean <- apply(all.data,2,mean)
train.sd <- apply(all.data,2,sd)
train.x <- scale(all.data, center=train.mean, scale=train.sd)[,-predict.col]
train.y <- scale(all.data, center=train.mean, scale=train.sd)[,predict.col]
r2.matrix <- matrix(NA, nrow=ncol(all.data))
for(i in 1:ncol(all.data)) {
  if(i==predict.col) next
  model <- keras_model_sequential() %>%
    layer_dense(units=round((ncol(all.data))*2/3), input_shape=c(NULL, 1)) %>%
    layer_dense(units=ncol(all.data)) %>%
    layer_dense(units = 1)
  model %>% compile(
    optimizer = optimizer_rmsprop(),
    loss = "mean_squared_error"
  )
  rows2keep <- which(!is.na(train.x[,i]))
  history <- model %>% fit(
    x=train.x[rows2keep,i],
    y=train.y[rows2keep],
    batch_size=1,
    epochs=10
  )
  validation <- model %>% predict(
    x=train.x[rows2keep,i],
    batch_size=1
  )
  r2.matrix[i,1] <- cor(validation, train.y[rows2keep])^2
}




# RNN
# Set wd
setwd("C:\\Users\\KNewhart\\Documents\\GitHub\\MWRD\\PAA ICT")

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
  # Create doSNOW compute cluster
  cluster = makeCluster(nThreads, type = "SOCK")
  # register the cluster
  registerDoSNOW(cluster)
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
}

# Use parallel loop for rolling window
n_train <- 0.8
predictions <- foreach(test.obs=seq(round(nrow(all.data)*n_train), nrow(all.data)), .combine = "rbind", .packages = c("dplyr", "keras")) %dopar% {
  # Setup training data
  train.start <- 1+test.obs-round(nrow(all.data)*n_train)
  train.end <- test.obs-1
  train.data <- all.data[train.start:train.end,]
  train.mean <- apply(train.data,2,mean)
  train.sd <- apply(train.data,2,sd)
  train.x <- simplify2array(list(scale(train.data, center=train.mean, scale=train.sd)[,-predict.col]))
  train.y <- simplify2array(list(scale(train.data, center=train.mean, scale=train.sd)[,predict.col]))
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
  test.x <- array_reshape(test.x, c(1,length(test.x),1))
  test.y <- scale(all.data, center=train.mean, scale=train.sd)[test.obs,predict.col]
  
  # Predict E coli
  pred <- model %>% predict(
    x=test.x,
    batch_size=1
  )
  
  # Calculate prediction error (in E. coli units)
  error <- pred*train.sd[predict.col]+train.mean[predict.col]-as.numeric(test.y*train.sd[predict.col]+train.mean[predict.col])
  
  data.frame("R2"=r2, "Prediction.Error"=error)
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
mean(abs(predictions[,1])) # R2
mean(predictions[,2]^2)^0.5 # RMSE



lookback <- 365
j <- 1
# train.x <- simplify2array(list(scale(all.data[j:(lookback+j-1),], center=train.mean, scale=train.sd)[,-predict.col]))
# train.y <- simplify2array(list(scale(all.data[j:(lookback+j-1),], center=train.mean, scale=train.sd)[,predict.col]))
train.x <- simplify2array(list(scale(all.data, center=train.mean, scale=train.sd)[,-predict.col]))
train.y <- simplify2array(list(scale(all.data, center=train.mean, scale=train.sd)[,predict.col]))
# all.r2 <- rep(NA,10)
n_batch = dim(train.x)[1]
n_epoch = 1000 
all.r2 <- foreach(iteration=1:10, .combine = "c", .packages = c("dplyr", "keras")) %dopar% {

model <- keras_model_sequential() %>%
  layer_lstm(units=dim(train.x)[2], input_shape=list(dim(train.x)[2],dim(train.x)[3])) %>%
  layer_dense(units = 1)
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mean_squared_error"
)

for(i in seq(1,n_epoch)) {
  history <- model %>% fit(
    x=train.x,
    y=train.y,
    batch_size=n_batch,
    epochs=1,
    shuffle=FALSE
    , stateful=TRUE
  )
  model %>% reset_states()
}
validation <- model %>% predict(
  x=train.x,
  batch_size=1
)
r2 <- cor(validation, train.y)^2
r2
}
mean(all.r2)
# Stateful, reset states r2 = 0.9004378
# Stateful, do not reset states r2 = 0.8827505
# Not stateful, do not reset states r2 = 0.8371064
# Not stateful, reset states r2 = 0.8354444

all.r2[iteration] <- r2
# }
mean(all.r2)
dim(all.data)





master.iterations <- list()
for(i in seq(2,ncol(master.data),by=2)) {
  master.zoo <- zoo(master.data, master.timestamps)
  print(paste0(colnames(master.zoo)[i],":"))
  rows2keep <- which(!is.na(master.zoo[,i]))
  master.zoo <- master.zoo[rows2keep,]
  master.zoo <- master.zoo[,-which(apply(master.zoo,2, anyNA))] # remove NAs
  all.data <- master.zoo
  all.data <- apply(all.data, 2, as.numeric)
  cols <- colnames(all.data)
  cols <- gsub("-", ".", gsub("[)]", ".", gsub("[(]", ".", gsub(" ", ".", cols))))
  colnames(all.data) <- cols
  predict.col <- which(colnames(all.data)=="ECIDX_G")
  
  train.mean <- apply(all.data,2,mean)
  train.sd <- apply(all.data,2,sd)
  train.x <- scale(all.data, center=train.mean, scale=train.sd)[,-predict.col]
  train.y <- scale(all.data, center=train.mean, scale=train.sd)[,predict.col]
  all.r2 <- foreach(p=1:10, .combine='c', .packages = c("keras", "dplyr")) %dopar% {
    # for(iteration in 1:length(all.r2)) {
    model <- keras_model_sequential() %>%
      layer_dense(units=ncol(all.data), 
                  input_shape=c(NULL, ncol(all.data[,-predict.col])), 
                  activation = "softsign") %>%
      layer_dense(units=ncol(all.data)) %>%
      layer_dense(units = 1)
    model %>% compile(
      optimizer = optimizer_rmsprop(),
      loss = "mean_squared_error"
    )
    history <- model %>% fit(
      x=train.x,
      y=train.y,
      batch_size=1,
      epochs=20
    )
    validation <- model %>% predict(
      x=train.x,
      batch_size=1
    )
    r2 <- cor(validation, train.y)^2;r2
    all.r2[iteration] <- r2
  }
  print(mean(all.r2))
  master.iterations[[length(master.iterations)+1]] <- mean(all.r2)
}





library(forecast)
auto.arima(master.zoo$Ecoli)




y <- master.zoo$Ecoli
# Step 1: do the FFT
raw.fft = fft(y)

# Step 2: drop anything past the N/2 - 1th element.
# This has something to do with the Nyquist-shannon limit, I believe
# (https://en.wikipedia.org/wiki/Nyquist%E2%80%93Shannon_sampling_theorem)
truncated.fft = raw.fft[seq(1, length(y)/2 - 1)]

# Step 3: drop the first element. It doesn't contain frequency information.
truncated.fft[1] = 0

# Step 4: the importance of each frequency corresponds to the absolute value of the FFT.
# The 2, pi, and length(y) ensure that omega is on the correct scale relative to t.
# Here, I set omega based on the largest value using which.max().
omega = which.max(abs(truncated.fft)) * 2 * pi / length(y)
# lapply(all.tags, function(tag) pull.and.save(tag=tag, start='*-800d', end='*-5d'))
# library(parallel)
# cl <- parallel::makeCluster(detectCores())
# parLapply(cl=cl, all.tags, function(tag) pull.and.save(tag=tag, start='*-735d', end='*-5d'))
# stopCluster(cl)

# Compile all data and average
library(xts)
master.file <- paste0(gsub("[.]", "_", make.names(tail(strsplit(tag.names[1],"[\\]")[[1]],n=1))),".csv")
master.obj <- read.csv(file=paste0("results/raw_data/",master.file), stringsAsFactors = FALSE)
master.obj <- master.obj[-1,] # Due to averaging, the first one will not be a fair average
master.timestamps <- master.obj[,1]
for(p in 2:nrow(data.parameters)) {
  file <- paste0(gsub("[.]", "_", make.names(tail(strsplit(data.parameters[p,1],"[\\]")[[1]],n=1))),".csv")
  if(!(file %in% list.files(path="results/raw_data"))) next
  obj <- read.csv(file=paste0("results/raw_data/",file), stringsAsFactors = FALSE)
  data <- xts(obj[,2], order.by=as.POSIXct(obj[,1]))
  avg <- data.parameters[p,2]
  if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="m") d <- lubridate::dminutes(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
  if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="h") d <- lubridate::dhours(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
  all.data <- master.obj
  for(t in 1:length(master.timestamps)) {
    start <- as.POSIXct(master.timestamps[t]) - as.numeric(d)
    end <- as.POSIXct(master.timestamps[t])
    test <- na.omit(data[paste0(start,"/",end),])
    if(length(test)==0) {
      all.data[t,2] <- NA
    } else {
      weights <- as.numeric(difftime(index(test), start, units="secs"))
      if(nrow(test) > 1) {
        for(i in length(weights):2) weights[i] <- weights[i]-weights[i-1]
        all.data[t,2] <- weighted.mean(test, w=weights/sum(weights))
      } else {
        all.data[t,2] <- as.numeric(test)
      }
      
    }
    
  }
  master.obj <- cbind(master.obj, all.data[,2])
  colnames(master.obj)[ncol(master.obj)] <- tail(strsplit(tail(strsplit(data.parameters[p,1],"[\\]")[[1]],n=1),"[|]")[[1]],n=1)
}

## Plot it!
# Timeseries
for(i in 2:ncol(master.obj)){plot(master.obj[,i], main=colnames(master.obj)[i],xaxt="n");axis(side=1,at=seq(1,nrow(master.obj),by=10), labels=format(as.POSIXct(master.obj[seq(1,nrow(master.obj),by=10),1]), "%m-%d"))}

# master.obj.na <- master.obj[,-which(apply(master.obj,2,anyNA))]
master.obj.na <- master.obj
##### NEURAL NETWORK #####
pkgs <- c("dplyr", "parallel", "doSNOW")
sapply(pkgs, function(p) {
  if(!(p %in% rownames(installed.packages()))) {
    install.packages(p)
  }
  library(p, character.only=TRUE)
})
if(!("keras" %in% rownames(installed.packages()))) {
  install.packages("keras")
  keras::install_keras()
}
library(keras)

all.data <- master.obj.na[1:round(0.8*nrow(master.obj.na)),-1]
all.data <- apply(all.data, 2, as.numeric)
predict.col <- 1
cols <- colnames(all.data)
cols <- gsub("-", ".", gsub("[)]", ".", gsub("[(]", ".", gsub(" ", ".", cols))))
colnames(all.data) <- cols
# detect threads with parallel()
# nThreads<- detectCores(logical = TRUE)
# # Create doSNOW compute cluster
# cluster = makeCluster(nThreads, type = "SOCK", outfile="")
# # register the cluster
# registerDoSNOW(cluster)
# Create model, add layers, and compile the model
# TO RUN THIS, NEED TO INSTALL MINICONDA
# WILL PROMPT INSTALL IF NONE FOUND
model <- keras_model_sequential() %>%
  layer_dense(units=round((ncol(all.data))*2/3), input_shape=c(NULL, ncol(all.data[,-predict.col]))) %>%
  layer_dense(units=ncol(all.data)) %>%
  layer_dense(units = 1)
model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mean_squared_error"
)
train.mean <- apply(all.data,2,mean)
train.sd <- apply(all.data,2,sd)
# train.x <- simplify2array(list(scale(all.data[i:(lookback+i-1),], center=train.mean, scale=train.sd)))
train.x <- scale(all.data, center=train.mean, scale=train.sd)[,-predict.col]
# train.y <- simplify2array(list(scale(all.data[(i+delay):(lookback+delay+i-1),], center=train.mean, scale=train.sd)[,predict.col]))
train.y <- scale(all.data, center=train.mean, scale=train.sd)[,predict.col]
history <- model %>% fit(
  x=train.x,
  y=train.y,
  batch_size=1,
  epochs=20
)
validation <- model %>% predict(
  x=train.x,
  batch_size=1
)
r2 <- cor(validation, train.y)^2;r2
# stopCluster(cluster)

library(mgcv)
fmla <- as.formula(paste(colnames(all.data)[predict.col], "~", paste(colnames(all.data)[-predict.col], collapse =" + ")))
gam.model <- gam(fmla, data=data.frame(all.data))


# test.x <- simplify2array(list(scale(t(all.data[(lookback+i),]), center=train.mean, scale=train.sd)))
# test.x <- scale(t(all.data[(lookback+i),]), center=train.mean, scale=train.sd)
# prediction <- model %>% predict(
#   x=test.x,
#   batch_size=1
# )
# prediction <- prediction*train.sd[predict.col]+train.mean[predict.col]
# actual <- all.data[(lookback+i+delay),predict.col]
# e <- abs(actual-prediction);e

# Compile all data
library(xts)
master.file <- paste0(gsub("[.]", "_", make.names(tail(strsplit(tag.names[1],"[\\]")[[1]],n=1))),".csv")
master.obj <- read.csv(file=paste0("results/raw_data/",master.file), stringsAsFactors = FALSE)
master.obj <- master.obj[-1,] # Due to averaging, the first one will not be a fair average
master.timestamps <- master.obj[,1]
for(p in 2:nrow(data.parameters)) {
  file <- paste0(gsub("[.]", "_", make.names(tail(strsplit(data.parameters[p,1],"[\\]")[[1]],n=1))),".csv")
  if(!(file %in% list.files(path="results/raw_data"))) next
  obj <- read.csv(file=paste0("results/raw_data/",file), stringsAsFactors = FALSE)
  data <- xts(obj[,2], order.by=as.POSIXct(obj[,1]))
  all.data <- master.obj
  for(t in 1:length(master.timestamps)) {
    end <- as.POSIXct(master.timestamps[t])
    test <- na.omit(data[paste0("/",end),])
    if(length(test)==0) {
      all.data[t,2] <- NA
    } else {
      all.data[t,2] <- as.numeric(test[nrow(test)])
    }
  }
  master.obj <- cbind(master.obj, all.data[,2])
  colnames(master.obj)[ncol(master.obj)] <- tail(strsplit(tail(strsplit(data.parameters[p,1],"[\\]")[[1]],n=1),"[|]")[[1]],n=1)
}
master.obj.na <- master.obj[,-which(apply(master.obj,2,anyNA))]
