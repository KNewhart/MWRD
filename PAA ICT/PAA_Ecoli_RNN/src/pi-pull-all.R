pi.pull.all <- function(parameters, # pi tags and data interals
                        start=as.POSIXct("2019-04-08 00:00:00"), # Earliest predisinfection Ecoli
                        end='*',
                        obj.return = TRUE, 
                        save = FALSE,
                        folder.path = file.path("data","postdisinfection-ecoli","north")) {
  
  source("src/pi-pull.R")
  
  # Create tag names from PI paths
  {
    tag.names <- sapply(parameters[,1], function(x) tail(strsplit(tail(strsplit(x,"[\\]")[[1]],n=1),"[|]")[[1]],n=1))
    
    # For cleanliness, remove path names from list
    # names(tag.names) <- NULL 
    
    # Rename duplicate tag names by including more characters from the PI path
    duplicates <- names(which(sapply(unique(tag.names), function(x) length(which(x==tag.names)))>1))
    for(i in which(tag.names %in% duplicates)) {
      tag.names[i] <- paste(strsplit(tail(strsplit(parameters[i,1],"[\\]")[[1]],n=1),"[|]")[[1]], collapse=" ")
    }
  }
  
  # Pull E coli data
  master.file <- pi.pull(tag=parameters[1,1],  # First tag should always be E.coli
                         start=start, # Earliest  predisinfection E.coli sample
                         end=end, 
                         obj.return = obj.return, 
                         save=save)
  
  # Save E coli data
  write.csv(master.file, file=file.path(folder.path,"master.csv"))
  
  # Initialize timestamps from E coli samples
  master.timestamps <- master.file[,1]
  
  # Compile all averaged or raw data (takes 1.5 hours to run serialized, 15 minutes in parallel)
  # Initialize parallelization
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
  foreach(p=1:length(tag.names),.packages = c("xts", "lubridate", "piwebapi")) %dopar% {
    file <- rep(NA, nrow(master.file))
    file.avg <- rep(NA, nrow(master.file))
    file.avg.24 <- rep(NA, nrow(master.file))
    
    # Original: Variable averages
    avg <- parameters[p,2]
    if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="m") d <- lubridate::dminutes(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
    if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="h") d <- lubridate::dhours(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
    
    # Testing: 24h avg for all paramters
    d.24 <- lubridate::dhours(24)
    
    for(t in 1:length(master.timestamps)) {
      print(paste("Starting timestamp", t))
      start <- as.POSIXct(master.timestamps[t]) - as.numeric(d)
      start.24 <- as.POSIXct(master.timestamps[t]) - as.numeric(d.24)
      end <- as.POSIXct(master.timestamps[t])
      
      test <- pi.pull(tag=parameters[p,1], start=start, end=end, save=FALSE, obj.return = TRUE)
      test.24 <- pi.pull(tag=parameters[p,1], start=start.24, end=end, save=FALSE, obj.return = TRUE)
      
      if((nrow(test)==0) || (length(test)==0)) {
        file[t] <- NA
        file.avg[t] <- NA
        file.avg.24[t] <- NA
      } else {
        
        file[t] <- as.numeric(test[nrow(test),2])
        
        weights <- as.numeric(difftime(test[,1], start, units="secs"))
        weights.24 <- as.numeric(difftime(test.24[,1], start.24, units="secs"))
        if(nrow(test) > 1) {
          for(i in length(weights):2) weights[i] <- weights[i]-weights[i-1]
          file.avg[t] <- weighted.mean(test[,2], w=weights/sum(weights))
          for(i in length(weights.24):2) weights.24[i] <- weights.24[i]-weights.24[i-1]
          file.avg.24[t] <- weighted.mean(test.24[,2], w=weights.24/sum(weights.24))
        } else {
          file.avg[t] <- as.numeric(test[,2])
          file.avg.24[t] <- as.numeric(test.24[,2])
        }
        
      }
    }
    write.csv(cbind(master.timestamps, file), file=file.path(folder.path,"instant",paste0(tag.names[p],".csv")))
    write.csv(cbind(master.timestamps, file.avg), file=file.path(folder.path,"average",paste0(tag.names[p],".csv")))
    write.csv(cbind(master.timestamps, file.avg.24), file=file.path(folder.path,"average-24",paste0(tag.names[p],".csv")))
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



