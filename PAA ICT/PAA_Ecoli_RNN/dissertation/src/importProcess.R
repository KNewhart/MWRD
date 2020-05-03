importProcess <- function(times) {
  
  
  # Import PI tags
  data.parameters <- read.csv("data/paa/data-parameters-north-paa.csv", stringsAsFactors = FALSE)
  
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
  # process.data.ls <- foreach(p=1:length(tag.names),.packages = c("xts", "lubridate", "piwebapi")) %dopar% {
  process.data.ls <- foreach(p=1:2,.packages = c("xts", "lubridate", "piwebapi")) %dopar% {
    file <- rep(NA, length(times))
    file.avg <- rep(NA, length(times))
    file.avg.24 <- rep(NA, length(times))
    
    # Original: Variable averages
    avg <- data.parameters[p,2]
    if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="m") d <- lubridate::dminutes(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
    if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="h") d <- lubridate::dhours(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
    
    # Testing: 24h avg for all parameters
    d.24 <- lubridate::dhours(24)
    
    # for(t in 1:length(times)) {
    for(t in 1:3) {
      print(paste("Starting timestamp", t))
      start <- as.POSIXct(times[t]) - as.numeric(d)
      start.24 <- as.POSIXct(times[t]) - as.numeric(d.24)
      end <- as.POSIXct(times[t])
      
      test <- piPull(tag=data.parameters[p,1], start=start, end=end, save=FALSE, obj.return = TRUE)
      test.24 <- piPull(tag=data.parameters[p,1], start=start.24, end=end, save=FALSE, obj.return = TRUE)
      
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

    # write.csv(cbind(times, file), file=paste0("data/paa/north/instant/",tag.names[p],".csv"))
    # write.csv(cbind(times, file.avg), file=paste0("data/paa/north/average/",tag.names[p],".csv"))
    # write.csv(cbind(times, file.avg.24), file=paste0("data/paa/north/average-24/",tag.names[p],".csv"))
    return(list(data.frame(times, 
                           file),
                data.frame(times, 
                           file.avg),
                data.frame(times, file.avg.24)))
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
  
  for(i in 1:length(process.data.ls)) {
    # lapply(process.data.ls, function(x) x[i]) # returns ith object in each list
    for(j in 1:length(process.data.ls[[i]])) {
      colnames(process.data.ls[[i]][[j]])[2] <- tag.names[i]
    }
  }
  
  return(process.data.ls)
  # 
  # 
  # # Install and load piwebapi package from Github
  # # install.packages("devtools")
  # # library(devtools)
  # # install_github("rbechalany/PI-Web-API-Client-R")
  # library(piwebapi)
  # 
  # # Login information
  # useKerberos <- TRUE
  # username <- "knewhart"
  # password <- ""
  # validateSSL <- TRUE
  # debug <- TRUE
  # piWebApiService <- piwebapi$new("https://pivision/piwebapi", useKerberos, username, password, validateSSL, debug)
  # 
  # # Go from MT to UTC
  # date.time <- as.POSIXct(times, origin="1970-01-01")
  # pi.times <- matrix(NA,nrow=length(date.time),ncol=1)
  # for(i in 1:length(date.time)) {
  #   time.obj <- date.time[i]
  #   time.obj <- lubridate::with_tz(time.obj, tzone="UTC")
  #   pi.times[i,1] <- paste0(as.character(format(time.obj,"%Y-%m-%d")),"T",
  #                           as.character(format(time.obj,"%H:%M:%S")),"Z")
  # }
  # 
  # # Declare with variables/pi tags to pull
  # pi.tags <- matrix(c("DIS North Flow", "\\\\applepi\\PAA_North_Plant_Flow",
  #                     "PAA Setpoint", "\\\\applepi\\PAA_N_Target_Dose",
  #                     "DIS PAA N Upstream Residual", "\\\\applepi\\AI_K826",
  #                     "NSEC Aerobic SRT", "\\\\applepi\\ASRT_ASRT_N",
  #                     "NSEC Effluent NH3", "\\\\applepi\\AI_N501A",
  #                     "NSEC Effluent NO3", "\\\\applepi\\AI_N501D",
  #                     "NSEC Effluent OP", "\\\\applepi\\AI_N501F",
  #                     "NSEC Effluent TSS", "\\\\applepi\\AI-K530N",
  #                     "NSEC Effluent NO5", "\\\\applepi\\AI-K570N", # All zeros
  #                     "NSEC Effluent Flow", "\\\\applepi\\FY-F225"), ncol=2, byrow=TRUE)
  # # Pull data
  # for(tag in 1:nrow(pi.tags)) {
  #   pi.points <- piWebApiService$point$getByPath(path=as.character(pi.tags[tag,2]))
  #   data.holder <- piWebApiService$data$stream$getInterpolatedAtTimes(webId = pi.points$WebId, 
  #                                                                     time = c(pi.times[,1]))[[2]]
  #   good.vals <- which(unlist(lapply(data.holder, function(x) length(x$Value)==1)))
  #   data.holder <- do.call("rbind", lapply(data.holder[good.vals], function(x) c(x$Timestamp, x$Value)))
  #   colnames(data.holder) <- c("Datetime", make.names(pi.tags[tag,1]))
  #   if(tag==1) all.data <- data.holder
  #   if(tag>1) {
  #     all.data <- cbind(all.data, data.holder[,2])
  #     colnames(all.data)[ncol(all.data)] <- make.names(pi.tags[tag,1])
  #   }
  # }
  # # Fix tags from UTC to MT
  # date.time <- all.data[,1]
  # new.date.time <- .POSIXct(rep(NA, length(date.time)))
  # for(i in 1:length(date.time)) {
  #   time.obj <- paste(strsplit(date.time[i], "T")[[1]][1], 
  #                     strsplit(strsplit(date.time[i], "T")[[1]][2], "Z")[[1]][1])
  #   time.obj <- lubridate::with_tz(as.POSIXct(time.obj, tz="UTC"), tzone = Sys.timezone())
  #   new.date.time[i] <- time.obj
  # }
  # all.data <- data.frame(all.data)
  # all.data[,1] <- new.date.time
  # colnames(all.data)[1] <- "date.time"
  # return(all.data)
}