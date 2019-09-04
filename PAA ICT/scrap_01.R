library(piwebapi)
# Login information
useKerberos <- TRUE
username <- "knewhart"
password <- "Lunabear2@"
validateSSL <- TRUE
debug <- TRUE
piWebApiService <- piwebapi$new("https://pivision/piwebapi", useKerberos, username, password, validateSSL, debug)

library(xts)
library(xlsx)

##### PAA #####
# Load raw data
# PAA.PROFILE.DATA <- read.csv("data/PAA PROFILE DATA_07-12-19.csv")
PAA.PROFILE.DATA <- read.xlsx("~/GitHub/MWRD/Sampling/data/PAA PROFILE DATA_08-02-19.xlsx", sheetIndex = 1)

# Subset PAA data
PAA.PROFILE.DATA <- PAA.PROFILE.DATA[which(PAA.PROFILE.DATA$ANALYSIS_CODE == "PAAR"),]

# Remove erroneous data
PAA.PROFILE.DATA <- PAA.PROFILE.DATA[which(PAA.PROFILE.DATA$COMBINATION_RESULT != "Scratched"),]
PAA.PROFILE.DATA <- PAA.PROFILE.DATA[which(!is.na(PAA.PROFILE.DATA$NUMERIC_RESULT)),]

# Fix timestamps
# date.time <- strptime(paste(as.character(PAA.PROFILE.DATA$COLLECTION_DATE), as.character(PAA.PROFILE.DATA$COLLECTION_TIME)), format="%m/%d/%Y %H:%M")
date.time <- strptime(paste(as.character(PAA.PROFILE.DATA$COLLECTION_DATE), as.character(PAA.PROFILE.DATA$COLLECTION_TIME)), format="%Y-%m-%d %H:%M")
PAA.PROFILE.DATA <- cbind(PAA.PROFILE.DATA[,1:3],date.time,PAA.PROFILE.DATA[,6:ncol(PAA.PROFILE.DATA)])


pi.times <- matrix(NA,nrow=length(date.time),ncol=1)
for(i in 1:length(date.time)) {
  time.obj <- date.time[i]
  time.obj$hour <- time.obj$hour - 6
  pi.times[i,1] <- paste0(as.character(format(time.obj,"%Y-%m-%d")),"T",
                          as.character(format(time.obj,"%H:%M:%S")),"Z")
}
pi.tags <- matrix(c("DIS PAA N Upstream Residual", "\\\\applepi\\AI_K826"), ncol=2, byrow=TRUE)
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
data <- cbind(as.data.frame(date.time), as.numeric(as.vector(PAA.PROFILE.DATA$NUMERIC_RESULT)), as.numeric(all.data[,-1]))
colnames(data) <- c("Time", "PAA Sample", "Chemscan")

plot(x=data$Time, y=data$`PAA Sample`, pch=20)
points(x=data$Time, y=data$Chemscan, pch=20, col="blue")

# Let's just use June 24 - July 23
data <- xts(data[,2:3], order.by = data[,1])
data <- data["2019-06-24/2019-07-23"]

plot(as.zoo(data$`PAA Sample`), type="p",pch=20, ylab="PAA")
points(as.zoo(data$Chemscan), pch=20, col="blue", ylab="PAA")
