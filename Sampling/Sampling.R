library(xts)

# Load raw data
PAA.PROFILE.DATA <- read.csv("data/PAA PROFILE DATA_07-12-19.csv")

# Subset PAA data
PAA.PROFILE.DATA <- PAA.PROFILE.DATA[which(PAA.PROFILE.DATA$ANALYSIS_CODE == "PAAR"),]

# Remove erroneous data
PAA.PROFILE.DATA <- PAA.PROFILE.DATA[which(PAA.PROFILE.DATA$COMBINATION_RESULT != "Scratched"),]
PAA.PROFILE.DATA <- PAA.PROFILE.DATA[which(!is.na(PAA.PROFILE.DATA$NUMERIC_RESULT)),]

# Fix timestamps
date.time <- strptime(paste(as.character(PAA.PROFILE.DATA$COLLECTION_DATE), as.character(PAA.PROFILE.DATA$COLLECTION_TIME)), format="%m/%d/%Y %H:%M")
PAA.PROFILE.DATA <- cbind(PAA.PROFILE.DATA[,1:3],date.time,PAA.PROFILE.DATA[,6:ncol(PAA.PROFILE.DATA)])

# Create xts object
PAA.PROFILE.DATA <- xts(PAA.PROFILE.DATA, order.by = PAA.PROFILE.DATA$date.time, origin = lubridate::origin)
dates <- as.POSIXct(PAA.PROFILE.DATA$date.time)

# Color by sampling campagin
PAA.PROFILE.DATA <- cbind(PAA.PROFILE.DATA, rep(NA, nrow(PAA.PROFILE.DATA)))
for(i in 2:nrow(PAA.PROFILE.DATA)){
  if(i == 2) {
    last.row <- 1
    sampling.campaign <- 1
  }
  if(i == nrow(PAA.PROFILE.DATA)) {
    PAA.PROFILE.DATA[last.row:(i),ncol(PAA.PROFILE.DATA)] <- sampling.campaign
  }
  if(difftime(dates[i],dates[i-1], units = "mins") > 60) {
    PAA.PROFILE.DATA[last.row:(i-1),ncol(PAA.PROFILE.DATA)] <- sampling.campaign
    last.row <- i
    sampling.campaign <- sampling.campaign + 1
  }
}
sampling.count <- sapply(unique(PAA.PROFILE.DATA[,ncol(PAA.PROFILE.DATA)]), function(x) length(which(PAA.PROFILE.DATA[,ncol(PAA.PROFILE.DATA)] == x)))
sampling.count <- as.numeric(names(sampling.count[which(sampling.count > 1)]))
PAA.PROFILE.DATA <- PAA.PROFILE.DATA[PAA.PROFILE.DATA[,ncol(PAA.PROFILE.DATA)] %in% sampling.count,]

# Pull HRT


# Pull dose


# Calculate k and D
for(s in sampling.count) {
  k <- # First order decay coefficient
}








plotting.data <- as.zoo(PAA.PROFILE.DATA)
xx <- index(plotting.data)
yy <- plotting.data$NUMERIC_RESULT
library(RColorBrewer)

plot(xx, as.numeric(yy), pch=20,
     col=brewer.pal(unique(sampling.campaign),"Set1")[as.numeric(plotting.data[,ncol(plotting.data)])])
# Add labels
# text(xx,as.numeric(yy),labels=xx, cex=.5, pos=2)

# From plot use 
