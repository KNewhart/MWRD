# Load required packages
library(xts)
library(xlsx)

#######################################
# We need three types of data:
# 1. Water qualility
# 2. PAA dose and concentration
# 3. E.coli concentration and removal
#
# It is quickest to load PAA and E.coli 
# and match timestamps to pull process
# water quality data
#######################################

##### PAA ##### 
if("paa.RData" %in% list.files(path="./data/compiled")) {
  load("./data/compiled/paa.RData")
} else {
  # Load raw data from sampling campaign
  PAA.PROFILE.DATA <- read.xlsx("data/PAA PROFILE DATA_08-08-19.xlsx", sheetIndex = 1)
  
  # Subset just PAA data
  paa <- PAA.PROFILE.DATA[which(PAA.PROFILE.DATA$ANALYSIS_CODE == "PAAR"),]
  
  # Remove erroneous/bad data
  paa <- paa[which(paa$COMBINATION_RESULT != "Scratched"),]
  paa <- paa[which(!is.na(paa$NUMERIC_RESULT)),]
  
  # Create timestamps
  date.time <- strptime(paste(as.character(paa$COLLECTION_DATE), as.character(paa$COLLECTION_TIME)), format="%Y-%m-%d %H:%M")
  
  # Create clean data object
  data <- data.frame(date.time, stringsAsFactors = FALSE)
  data <- cbind(data, as.data.frame(paa$COMMON_NAME, stringsAsFactors = FALSE))
  data <- cbind(data, as.data.frame(as.numeric(as.character(paa$NUMERIC_RESULT)),
                                    stringsAsFactors = FALSE))
  colnames(data) <- c("date.time", "COMMON_NAME", "NUMERIC_RESULT")
  data <- data[order(data[,1]),]
  
  # Label sampling campaigns & hour of the day
  sample.count <- vector()
  hour <- vector()
  for(i in 2:nrow(data)){
    if(i == 2) {
      last.row <- 1
      sampling.campaign <- 1
    }
    
    if(difftime(data[,"date.time"][i],data[,"date.time"][i-1], units = "mins") > 60) {
      if(i == nrow(data)) {
        sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:i))))
        hour <- c(hour, rep(format(round(data$date.time[i], units="hours"), "%H"), length(c(last.row:(i)))))
      } else {
        sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:(i-1)))))
        hour <- c(hour, rep(format(round(data$date.time[i-1], units="hours"), "%H"), length(c(last.row:(i-1)))))
        last.row <- i
        sampling.campaign <- sampling.campaign + 1
      }
    } else {
      if(i == nrow(data)) {
        sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:i))))
        hour <- c(hour, rep(format(round(data$date.time[i], units="hours"), "%H"), length(c(last.row:(i)))))
      }
    }
  }
  data2 <- cbind(data, as.data.frame(sample.count, stringsAsFactors=FALSE), as.data.frame(as.numeric(hour)))
  colnames(data2)[ncol(data2)] <- "hour.of.day"
  paa <- data2
  save(paa, file="./data/compiled/paa.RData")
}

##### ECOLI #####
if("ecoli.RData" %in% list.files(path="./data/compiled")) {
  load("./data/compiled/ecoli.RData")
} else {
  # Load raw data from sampling campaign
  PAA.PROFILE.DATA <- read.xlsx("data/PAA PROFILE DATA_08-08-19.xlsx", sheetIndex = 1)
  
  # Subset just Ecoli data
  ecoli <- PAA.PROFILE.DATA[which(PAA.PROFILE.DATA$ANALYSIS_CODE == "ECIDX"),]
  
  # Remove irrelevant data
  ecoli <- ecoli[-which(ecoli$COMMON_NAME == "S_PREPAA"),] # Not considering data from the South
  ecoli <- ecoli[-which(ecoli$COMMON_NAME == "N_PREPAA"),] # Not considering data not included in a sampling campaign (i.e., no number precluding label)
  
  # Create timestamps
  date.time <- strptime(paste(as.character(ecoli$COLLECTION_DATE), as.character(ecoli$COLLECTION_TIME)), format="%Y-%m-%d %H:%M")
  
  # Create clean data object
  data <- data.frame(date.time, stringsAsFactors = FALSE)
  data <- cbind(data, as.data.frame(ecoli$COMMON_NAME, stringsAsFactors = FALSE))
  data <- cbind(data, as.data.frame(as.numeric(as.character(ecoli$NUMERIC_RESULT)),
                                    stringsAsFactors = FALSE))
  colnames(data) <- c("date.time", "COMMON_NAME", "NUMERIC_RESULT")
  data <- data[order(data[,1]),]
  
  # Label sampling campaigns
  sample.count <- vector()
  for(i in 2:nrow(data)){
    if(i == 2) {
      last.row <- 1
      sampling.campaign <- 1
    }
    
    if(difftime(data[,"date.time"][i],data[,"date.time"][i-1], units = "mins") > 60) {
      if(i == nrow(data)) {
        sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:i))))
      } else {
        sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:(i-1)))))
        last.row <- i
        sampling.campaign <- sampling.campaign + 1
      }
    } else {
      if(i == nrow(data)) {
        sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:i))))
        }
    }
  }
  data2 <- cbind(data, as.data.frame(sample.count, stringsAsFactors=FALSE))
  
  # Calculate log removal
  log.removal <- vector()
  for(i in unique(data2$sample.count)) {
    experiment <- data2[which(data2$sample.count == i),]
    i.ecoli <- experiment[grepl("N_PREPAA", experiment$COMMON_NAME),"NUMERIC_RESULT"]
    if(length(i.ecoli) == 0) {
      log.removal <- c(log.removal, rep(NA, nrow(experiment)))
    } else {
      exp.removal <- log10(i.ecoli/experiment$NUMERIC_RESULT)
      log.removal <- c(log.removal, exp.removal)
    }
  }
  data3 <- cbind(data2, as.data.frame(log.removal))
  ecoli <- data3
  save(ecoli, file="./data/compiled/ecoli.RData")
}

##### Water quality #####
if("wq.RData" %in% list.files(path="./data/compiled")) {
  load("./data/compiled/wq.RData")
} else {
  # Install and load piwebapi package from Github
  # install.packages("devtools")
  # library(devtools)
  # install_github("rbechalany/PI-Web-API-Client-R")
  library(piwebapi)
  
  # Login information
  useKerberos <- TRUE
  username <- "knewhart"
  password <- "Lunabear2@"
  validateSSL <- TRUE
  debug <- TRUE
  piWebApiService <- piwebapi$new("https://pivision/piwebapi", useKerberos, username, password, validateSSL, debug)
  # Go from MT to UTC
  pi.times <- matrix(NA,nrow=length(date.time),ncol=1)
  date.time <- paa$date.time
  for(i in 1:length(date.time)) {
    time.obj <- date.time[i]
    time.obj <- lubridate::with_tz(time.obj, tzone="UTC")
    pi.times[i,1] <- paste0(as.character(format(time.obj,"%Y-%m-%d")),"T",
                            as.character(format(time.obj,"%H:%M:%S")),"Z")
  }
  # Declare with variables/pi tags to pull
  pi.tags <- matrix(c("DIS North Flow", "\\\\applepi\\PAA_North_Plant_Flow",
                      "PAA Setpoint", "\\\\applepi\\PAA_N_Target_Dose",
                      "DIS PAA N Upstream Residual", "\\\\applepi\\AI_K826",
                      "NSEC Aerobic SRT", "\\\\applepi\\ASRT_ASRT_N",
                      "NSEC Effluent NH3", "\\\\applepi\\AI_N501A",
                      "NSEC Effluent NO3", "\\\\applepi\\AI_N501D",
                      "NSEC Effluent OP", "\\\\applepi\\AI_N501F",
                      "NSEC Effluent TSS", "\\\\applepi\\AI-K530N",
                      # "NSEC Effluent NO5", "\\\\applepi\\AI-K570N", # All zeros
                      "NSEC Effluent Flow", "\\\\applepi\\FY-F225"), ncol=2, byrow=TRUE)
  # Pull data
  for(tag in 1:nrow(pi.tags)) {
    pi.points <- piWebApiService$point$getByPath(path=as.character(pi.tags[tag,2]))
    data.holder <- piWebApiService$data$stream$getInterpolatedAtTimes(webId = pi.points$WebId, 
                                                                      time = c(pi.times[,1]))[[2]]
    data.holder <- do.call("rbind", lapply(data.holder, function(x) c(x$Timestamp, x$Value)))
    colnames(data.holder) <- c("Datetime", make.names(pi.tags[tag,1]))
    if(tag==1) all.data <- data.holder
    if(tag>1) {
      all.data <- cbind(all.data, data.holder[,2])
      colnames(all.data)[ncol(all.data)] <- make.names(pi.tags[tag,1])
    }
  }
  # Fix tags from UTC to MT
  date.time <- all.data[,1]
  new.date.time <- .POSIXct(rep(NA, length(date.time)))
  for(i in 1:length(date.time)) {
    time.obj <- paste(strsplit(date.time[i], "T")[[1]][1], 
                      strsplit(strsplit(date.time[i], "T")[[1]][2], "Z")[[1]][1])
    time.obj <- lubridate::with_tz(as.POSIXct(time.obj, tz="UTC"), tzone = Sys.timezone())
    new.date.time[i] <- time.obj
  }
  all.data <- data.frame(all.data)
  all.data[,1] <- new.date.time
  colnames(all.data)[1] <- "date.time"
  wq.data <- all.data
  save(wq.data, file="./data/compiled/wq.RData")
}

##### Merge & Calculate HRT #####
if("paa-wq-ecoli.RData" %in% list.files(path="./data/compiled")) {
  load("./data/compiled/paa-wq-ecoli.RData")
} else {
  paa.wq <- cbind(paa[which(paa$date.time %in% wq.data$date.time),], 
                  wq.data[which(wq.data$date.time %in% paa$date.time),-1])
  # Calculate HRT
  hrt <- matrix(data=NA, nrow=nrow(paa.wq), ncol = 1)
  hrt[grep("NPAA1M", paa.wq$COMMON_NAME)] <- 122.2683*as.numeric(as.vector(paa.wq[c(grep("NPAA1M", paa.wq$COMMON_NAME)),"DIS.North.Flow"]))^(-0.9312)
  hrt[grep("NPAA10M", paa.wq$COMMON_NAME)] <- 1044.6*as.numeric(as.vector(paa.wq[c(grep("NPAA10M", paa.wq$COMMON_NAME)),"DIS.North.Flow"]))^(-0.956)
  hrt[grep("NPAA20M", paa.wq$COMMON_NAME)] <- 1909.8*as.numeric(as.vector(paa.wq[c(grep("NPAA20M", paa.wq$COMMON_NAME)),"DIS.North.Flow"]))^(-0.958)
  hrt[grep("NPAA30M", paa.wq$COMMON_NAME)] <- 2775*as.numeric(as.vector(paa.wq[c(grep("NPAA30M", paa.wq$COMMON_NAME)),"DIS.North.Flow"]))^(-0.959)
  paa.wq <- cbind(paa.wq, as.data.frame(hrt, stringsAsFactors = FALSE))
  colnames(paa.wq)[ncol(paa.wq)] <- "HRT (min)"
  
  ##### PAA & WQ & Ecoli #####
  paa.wq.ecoli <- cbind(paa.wq, rep(NA, nrow(paa.wq)))
  paa.wq.ecoli[which(paa.wq.ecoli$date.time %in% ecoli$date.time), ncol(paa.wq.ecoli)] <- 
    ecoli[which(ecoli$date.time %in% paa.wq.ecoli$date.time), "log.removal"]
  colnames(paa.wq.ecoli)[ncol(paa.wq.ecoli)] <- "Log Removal"
  save(paa.wq.ecoli, file="./data/compiled/paa-wq-ecoli.RData")
}

