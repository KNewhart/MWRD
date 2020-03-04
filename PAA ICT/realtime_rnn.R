# Import PI data/averages for 24 hour period
## Initialize R-PI connection
{
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
}
## Import and average data 
{
  # PI tags to import
  data.parameters <- read.csv("src/data-parameters.csv", stringsAsFactors = FALSE)
  # Make unique tag names
  tag.names <- sapply(data.parameters[,1], function(x) tail(strsplit(tail(strsplit(x,"[\\]")[[1]],n=1),"[|]")[[1]],n=1))
  names(tag.names) <- NULL
  duplicates <- names(which(sapply(unique(tag.names), function(x) length(which(x==tag.names)))>1))
  for(i in which(tag.names %in% duplicates)) {
    tag.names[i] <- paste(strsplit(tail(strsplit(data.parameters[i,1],"[\\]")[[1]],n=1),"[|]")[[1]], collapse=" ")
  }
  
  # Timestamps (one hour intervals)
  timestamps <- as.POSIXct(seq(Sys.time()-60*60*24*3, Sys.time(), by=60*60))
  
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
  
  # Import and average each PI tag
  library(foreach)
  foreach(p=2:length(tag.names),.packages = c("xts", "lubridate", "piwebapi")) %dopar% {
    print(paste("Starting variable", p))
    file.avg <- rep(NA, length(timestamps))
    
    avg <- data.parameters[p,2]
    if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="m") d <- lubridate::dminutes(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
    if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="h") d <- lubridate::dhours(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
    
    for(t in 1:length(timestamps)) {
      print(paste("Starting timestamp", t))
      start <- as.POSIXct(timestamps[t]) - as.numeric(d)
      end <- as.POSIXct(timestamps[t])
      test <- pull.and.save(tag=data.parameters[p,1], start=start, end=end, save=FALSE, obj.return = TRUE)
      if((nrow(test)==0) || (length(test)==0)) {
        file.avg[t] <- NA
      } else {
        weights <- as.numeric(difftime(test[,1], start, units="secs"))
        if(nrow(test) > 1) {
          for(i in length(weights):2) weights[i] <- weights[i]-weights[i-1]
          file.avg[t] <- weighted.mean(test[,2], w=weights/sum(weights))
        } else {
          file.avg[t] <- as.numeric(test[,2])
        }
      }
    }
    write.csv(cbind(as.character(timestamps), file.avg), file=paste0("results/realtime/testing/",tag.names[p],".csv"))
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
}
## Combine averaged data into a single data object
{
  all.data <- lapply(list.files("results/realtime/testing", full.names = TRUE, pattern=".csv"), 
                     read.csv, 
                     colClasses=c("NULL", "NULL", NA),
                     header=TRUE)
  all.data <- do.call("cbind", all.data)
  for(i in 1:ncol(all.data)) {colnames(all.data)[i] <- substr(list.files("results/realtime/testing", pattern=".csv")[i], start=1, stop=nchar(list.files("results/realtime/testing", pattern=".csv")[i])-4)}
  test.data <- xts(x=all.data, order.by=timestamps)
}
## Interpolate/infer current FC24 values
test.data <- na.locf(test.data)
test.data <- test.data[,-which(apply(test.data,2,anyNA))]

# Build model from historical data
## Extract timestamps from all historical ecoli data
{
  all.ecoli <- pull.and.save(tag=data.parameters[1,1], start='*-763d', end='*', obj.return = TRUE, save=FALSE)
  timestamps.train <- all.ecoli[,1]
  write.csv(all.ecoli, file=paste0("results/realtime/training/",tag.names[1],".csv"))
  
}
## Initialize parallelization
{
  library(parallel)
  # detect threads with parallel()
  nThreads<- detectCores(logical = TRUE)
  # Create doSNOW compute cluster
  cluster = makeCluster(nThreads, type = "SOCK")
  # register the cluster
  registerDoSNOW(cluster)
}
## Import and average each PI tag
{
  library(foreach)
  vars <- colnames(test.data)
  foreach(p=c(1,which(tag.names %in% vars)),.packages = c("xts", "lubridate", "piwebapi")) %dopar% {
    print(paste("Starting variable", p))
    file.avg <- rep(NA, length(timestamps.train))
    
    avg <- data.parameters[p,2]
    if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="m") d <- lubridate::dminutes(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
    if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="h") d <- lubridate::dhours(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
    
    for(t in 1:length(timestamps.train)) {
      print(paste("Starting timestamp", t))
      start <- as.POSIXct(timestamps.train[t]) - as.numeric(d)
      end <- as.POSIXct(timestamps.train[t])
      test <- pull.and.save(tag=data.parameters[p,1], start=start, end=end, save=FALSE, obj.return = TRUE)
      if((nrow(test)==0) || (length(test)==0)) {
        file.avg[t] <- NA
      } else {
        weights <- as.numeric(difftime(test[,1], start, units="secs"))
        if(nrow(test) > 1) {
          for(i in length(weights):2) weights[i] <- weights[i]-weights[i-1]
          file.avg[t] <- weighted.mean(test[,2], w=weights/sum(weights))
        } else {
          file.avg[t] <- as.numeric(test[,2])
        }
      }
    }
    write.csv(cbind(as.character(timestamps.train), file.avg), file=paste0("results/realtime/training/",tag.names[p],".csv"))
  }
}
## Clean up parallel
{
  # stop cluster and remove clients
  stopCluster(cluster)
  # insert serial backend, otherwise error in repetetive tasks
  registerDoSEQ()
  # clean up a bit.
  invisible(gc); remove(nThreads); remove(cluster); 
  
}
## Combine all training data
{
  train.data <- lapply(list.files("results/realtime/training", full.names = TRUE, pattern=".csv"), 
                       read.csv, 
                       colClasses=c("NULL", "NULL", NA),
                       header=TRUE)
  train.data <- do.call("cbind", train.data)
  for(i in 1:ncol(train.data)) {colnames(train.data)[i] <- substr(list.files("results/realtime/training", pattern=".csv")[i], start=1, stop=nchar(list.files("results/realtime/training", pattern=".csv")[i])-4)}
  rownames(train.data) <- as.character(timestamps.train)
  train.data <- na.locf(train.data)[-1,]
  if(length(which(apply(train.data,2,anyNA))) > 0) train.data <- train.data[,-which(apply(train.data,2,anyNA))]
  # Log transfrm E.coli
  predict.col <- which(colnames(train.data)=="ECIDX_G")
  train.data[,predict.col] <- log10(train.data[,predict.col])
  train.data <- cbind(train.data[3:nrow(train.data),], train.data[2:(nrow(train.data)-1),predict.col], train.data[1:(nrow(train.data)-2),predict.col])
}
## Build RNN
{
  train.mean <- apply(train.data,2,mean)
  train.sd <- apply(train.data,2,sd)
  # Create 3D array of training data to simulate time sequence in batches of 1
  train.x <- simplify2array(list(scale(train.data, center=train.mean, scale=train.sd)[,-predict.col]))
  train.y <- simplify2array(list(scale(train.data, center=train.mean, scale=train.sd)[,predict.col]))
  n_batch <- dim(train.x)[1]
  n_epoch <- 1000
  act.function <- "softsign"
  # Setup model
  model <- keras_model_sequential() %>%
    layer_lstm(units=dim(train.x)[2], 
               input_shape=list(dim(train.x)[2],dim(train.x)[3]), 
               activation = act.function) %>%
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
  
  # Validate model
  validate <- model %>% predict(
    x=train.x,
    batch_size=n_batch
  )
  
  ## Un-scale
  validate <- validate*train.sd[predict.col]+train.mean[predict.col]
  
  ## R2
  cor(10^train.data[,predict.col], 10^validate)
  
  ## RMSE
  mean((10^train.data[,predict.col]-10^validate)^2)^0.5
  
  ## Plot
  actual.predicted.plot <- function(actual, predicted, label) {
    r <- c(min(c(actual[,2], predicted[,2])), max(c(actual[,2], predicted[,2])))
    plot(x=actual[,1], y=actual[,2], main=label, pch=20,xlab="", ylab="", ylim=c(r[1], r[2]))
    points(x=predicted[,1], y=predicted[,2], pch=20, col="purple")
    line.matrix <- matrix(data=c(actual[,1], actual[,2], predicted[,1], predicted[,2]), ncol=4)
    sapply(1:nrow(actual), function(i) lines(x=line.matrix[i,c(1,3)], y=line.matrix[i,c(2,4)]))
  }
  # plot(x=as.POSIXct(rownames(train.data)), y=validate, ylab="Predisinfection E.coli", pch=20)
  label <- "Predisinfection E.coli"
  actual <- data.frame(as.POSIXct(rownames(train.data)), 10^train.data[,predict.col])
  predicted <- data.frame(as.POSIXct(rownames(train.data)), 10^validate)
  for(i in seq(1,nrow(actual),by=100)) actual.predicted.plot(actual[i:(i+100),], predicted[i:(i+100),], label)
  actual.predicted.plot(actual[1:100,], predicted[1:100,], label)
  
  actual <- data.frame(as.POSIXct(rownames(train.data)), train.data[,predict.col])
  predicted <- data.frame(as.POSIXct(rownames(train.data)), validate)
  actual.predicted.plot(actual, predicted, label)
}

    
# Test model
{
  ## Setup testing data
  test.x <- scale(test.data, 
                  center=train.mean[which(names(train.mean) %in% colnames(test.data))], 
                  scale=train.sd[which(names(train.sd) %in% colnames(test.data))])
  ## Predict E coli
  pred <- model %>% predict(
    x=array_reshape(test.x, c(nrow(test.x),ncol(test.x),1)),
    batch_size=nrow(test.x)
  )
  
  ## Un-scale
  pred <- pred*train.sd[predict.col]+train.mean[predict.col]
  
  ## Un-log transform
  pred <- 10^pred
  
  ## Plot
  plot(pred, ylab="Predisinfection E.coli", pch=20)
}
