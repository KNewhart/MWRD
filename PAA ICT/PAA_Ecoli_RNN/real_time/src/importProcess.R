importProcess <- function(times) {
  
  
  # Import PI tags
  data.parameters <- read.csv("data/data-parameters-north-paa.csv", stringsAsFactors = FALSE)
  data.parameters <- data.parameters[!duplicated(data.parameters[,1]),]
  
  # Create tag names from PI paths
  {
    tag.names <- sapply(data.parameters[,1], function(x) tail(strsplit(tail(strsplit(x,"[\\]")[[1]],n=1),"[|]")[[1]],n=1))
    
    # For cleanliness, remove path names from list
    names(tag.names) <- NULL
    
    # Rename duplicate tag names by including more characters from the PI path
    duplicates <- names(which(sapply(unique(tag.names), function(x) length(which(x==tag.names)))>1))
    for(i in which(tag.names %in% duplicates)) {
      tag.names[i] <- paste(strsplit(tail(strsplit(data.parameters[i,1],"[\\]")[[1]],n=1),"[|]")[[1]], collapse=" ")
    }
  }

  
  {
    library(parallel)
    library(doSNOW)
    # detect threads with parallel()
    nThreads<- detectCores(logical = TRUE)
    # Create doSNOW compute cluster
    cluster = makeCluster(nThreads, type = "SOCK")
    # register the cluster
    registerDoSNOW(cluster)
  }
  
  library(foreach)
  process.data.ls <- foreach(p=1:length(tag.names),.packages = c("xts", "lubridate", "piwebapi"), .export=c("piPull")) %dopar% {
  # process.data.ls <- foreach(p=1:2,.packages = c("xts", "lubridate", "piwebapi"), .export=c("piPull")) %dopar% {
    # file <- rep(NA, length(times))
    file.avg <- rep(NA, length(times))
    # file.avg.24 <- rep(NA, length(times))
    
    # Original: Variable averages
    avg <- data.parameters[p,2]
    if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="m") d <- lubridate::dminutes(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
    if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="h") d <- lubridate::dhours(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
    # 
    # Testing: 24h avg for all parameters
    # d.24 <- lubridate::dhours(24)
    
    for(t in 1:length(times)) {
    # for(t in 1:3) {
      print(paste("Starting timestamp", t))
      start <- as.POSIXct(times[t]) - as.numeric(d)
      # start.24 <- as.POSIXct(times[t]) - as.numeric(d.24)
      end <- as.POSIXct(times[t])
      
      test <- piPull(tag=data.parameters[p,1], start=start, end=end, save=FALSE, obj.return = TRUE)
      # test.24 <- piPull(tag=data.parameters[p,1], start=start.24, end=end, save=FALSE, obj.return = TRUE)
      
      if((nrow(test)==0) || (length(test)==0)) {
        # file[t] <- NA
        file.avg[t] <- NA
        # file.avg.24[t] <- NA
      } else {
        
        # file[t] <- as.numeric(test[nrow(test),2])
        
        weights <- as.numeric(difftime(test[,1], start, units="secs"))
        # weights.24 <- as.numeric(difftime(test.24[,1], start.24, units="secs"))
        if(nrow(test) > 1) {
          for(i in length(weights):2) weights[i] <- weights[i]-weights[i-1]
          file.avg[t] <- weighted.mean(test[,2], w=weights/sum(weights))
          # for(i in length(weights.24):2) weights.24[i] <- weights.24[i]-weights.24[i-1]
          # file.avg.24[t] <- weighted.mean(test.24[,2], w=weights.24/sum(weights.24))
        } else {
          file.avg[t] <- as.numeric(test[,2])
          # file.avg.24[t] <- as.numeric(test.24[,2])
        }
        
      }
    }

    # write.csv(cbind(times, file), file=paste0("data/paa/north/instant/",tag.names[p],".csv"))
    # write.csv(cbind(times, file.avg), file=paste0("data/paa/north/average/",tag.names[p],".csv"))
    # write.csv(cbind(times, file.avg.24), file=paste0("data/paa/north/average-24/",tag.names[p],".csv"))
    
    # return(list(data.frame(times, 
    #                        file),
    #             data.frame(times, 
    #                        file.avg),
    #             data.frame(times, file.avg.24)))
    return(data.frame(file.avg))
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
  
  process.data.ls <- do.call("cbind", process.data.ls)
  colnames(process.data.ls) <- tag.names
  
  
  return(process.data.ls)

}