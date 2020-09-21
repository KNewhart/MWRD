import2019 <- function() {
  # Load required packages
  library(xts)
  library(xlsx)
  
  ##### PAA ##### 
  # Load raw data from sampling campaign
  PAA.PROFILE.DATA <- read.xlsx("data/paa/PAA PROFILE DATA_08-08-19.xlsx", sheetIndex = 1)
  
  # Subset just PAA data
  paa <- PAA.PROFILE.DATA[which(PAA.PROFILE.DATA$ANALYSIS_CODE == "PAAR"),]
  
  # Remove erroneous/bad data
  paa <- paa[which(paa$COMBINATION_RESULT != "Scratched"),]
  paa <- paa[which(!is.na(paa$NUMERIC_RESULT)),]
  
  # Create timestamps
  date.time <- lubridate::with_tz(as.POSIXct(strptime(paste(as.character(paa$COLLECTION_DATE), as.character(paa$COLLECTION_TIME)), format="%Y-%m-%d %H:%M")), "UTC")
  
  # Create clean data object
  data <- data.frame(date.time, stringsAsFactors = FALSE)
  data <- cbind(data, as.data.frame(paa$COMMON_NAME, stringsAsFactors = FALSE))
  data <- cbind(data, as.data.frame(as.numeric(as.character(paa$NUMERIC_RESULT)),
                                    stringsAsFactors = FALSE))
  colnames(data) <- c("date.time", "COMMON_NAME", "NUMERIC_RESULT")
  data <- data[order(data[,1]),]
  
  # Label sampling campaigns & hour of the day
  sample.count <- vector()
  min <- vector()
  hour <- vector()
  for(i in 2:nrow(data)){
    if(i == 2) {
      last.row <- 1
      sampling.campaign <- 1
    }
    
    if(difftime(data[,"date.time"][i],data[,"date.time"][i-1], units = "mins") > 60) {
      if(i == nrow(data)) {
        sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:i))))
        min <- c(min, rep(as.numeric(difftime(as.POSIXct(paste(Sys.Date(), format(data$date.time[i],'%H:%M'))), as.POSIXct(paste(Sys.Date(), "00:00")), units = 'min')), length(c(last.row:(i)))))
        hour <- c(hour, rep(format(round(data$date.time[i], units="hours"), "%H"), length(c(last.row:(i)))))
      } else {
        sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:(i-1)))))
        min <- c(min, rep(as.numeric(difftime(as.POSIXct(paste(Sys.Date(), format(data$date.time[i-1],'%H:%M'))), as.POSIXct(paste(Sys.Date(), "00:00")), units = 'min')), length(c(last.row:(i-1)))))
        hour <- c(hour, rep(format(round(data$date.time[i-1], units="hours"), "%H"), length(c(last.row:(i-1)))))
        last.row <- i
        sampling.campaign <- sampling.campaign + 1
      }
    } else {
      if(i == nrow(data)) {
        sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:i))))
        hour <- c(hour, rep(format(round(data$date.time[i], units="hours"), "%H"), length(c(last.row:(i)))))
        min <- c(min, rep(as.numeric(difftime(as.POSIXct(paste(Sys.Date(), format(data$date.time[i],'%H:%M'))), as.POSIXct(paste(Sys.Date(), "00:00")), units = 'min')), length(c(last.row:(i)))))
      }
    }
  }
  data2 <- cbind(data, as.data.frame(sample.count, stringsAsFactors=FALSE))
  data3 <- lapply(unique(sample.count), function(x) {
    n <- which(data2$sample.count==x)
    c <- data2$NUMERIC_RESULT[n]
    t <- data2$date.time[n][1]
    data <- data.frame(t,t(c))
    data
  })
  data3 <- do.call("rbind", data3)

  # data2 <- cbind(data, as.data.frame(sample.count, stringsAsFactors=FALSE), as.data.frame(min))
  # colnames(data2)[ncol(data2)] <- "min.of.day"
  paa <- xts(data3[,-1], order.by=data3[,1])
  colnames(paa) <- c(paste0("PAA.2019.",1:4))
  

  #   # Calculate HRT
  #   hrt <- matrix(data=NA, nrow=nrow(paa.wq), ncol = 1)
  #   hrt[grep("NPAA1M", paa.wq$COMMON_NAME)] <- 57.866*as.numeric(as.vector(paa.wq[c(grep("NPAA1M", paa.wq$COMMON_NAME)),"DIS.North.Flow"]))^(-0.959)
  #   hrt[grep("NPAA10M", paa.wq$COMMON_NAME)] <- 970.18*as.numeric(as.vector(paa.wq[c(grep("NPAA10M", paa.wq$COMMON_NAME)),"DIS.North.Flow"]))^(-0.959)
  #   hrt[grep("NPAA20M", paa.wq$COMMON_NAME)] <- 1825.3*as.numeric(as.vector(paa.wq[c(grep("NPAA20M", paa.wq$COMMON_NAME)),"DIS.North.Flow"]))^(-0.959)
  #   hrt[grep("NPAA30M", paa.wq$COMMON_NAME)] <- 2690.3*as.numeric(as.vector(paa.wq[c(grep("NPAA30M", paa.wq$COMMON_NAME)),"DIS.North.Flow"]))^(-0.959)
  #   paa.wq <- cbind(paa.wq, as.data.frame(hrt, stringsAsFactors = FALSE))
  #   colnames(paa.wq)[ncol(paa.wq)] <- "HRT (min)"

  

  
  return(paa)
}