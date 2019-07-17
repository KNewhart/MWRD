library(xts)
PAA.PROFILE.DATA <- read.csv("~/GitHub/MWRD/Sampling/Copy of PAA PROFILE DATA_6-27-19_2.csv")
date.time <- strptime(paste(as.character(PAA.PROFILE.DATA$COLLECTION_DATE), as.character(PAA.PROFILE.DATA$COLLECTION_TIME)), format="%m/%d/%Y %H:%M")
PAA.PROFILE.DATA <- cbind(PAA.PROFILE.DATA[,1:3],date.time,PAA.PROFILE.DATA[,6:ncol(PAA.PROFILE.DATA)])
# Color by sampling campagin
PAA.PROFILE.DATA <- cbind(PAA.PROFILE.DATA, rep(NA, nrow(PAA.PROFILE.DATA)))

PAA.PROFILE.DATA <- xts(PAA.PROFILE.DATA, order.by = PAA.PROFILE.DATA$date.time, origin = lubridate::origin)
dates <- as.POSIXct(PAA.PROFILE.DATA$date.time)

for(i in 2:nrow(PAA.PROFILE.DATA)){
  if(i == 2) {
    last.row <- 1
    sampling.campaign <- 1
  }
  # if(difftime(index(PAA.PROFILE.DATA)[i],index(PAA.PROFILE.DATA)[i-1], units = "mins") > 60) {
  if(difftime(dates[i],dates[i-1], units = "mins") > 60) {
    PAA.PROFILE.DATA[last.row:(i-1),ncol(PAA.PROFILE.DATA)] <- sampling.campaign
    last.row <- i
    sampling.campaign <- sampling.campaign + 1
  }
}


# Plot data
# with(PAA.PROFILE.DATA[PAA.PROFILE.DATA$ANALYSIS_CODE == "PAAR",], plot (date.time, NUMERIC_RESULT))


plotting.data <- as.zoo(PAA.PROFILE.DATA[PAA.PROFILE.DATA$ANALYSIS_CODE == "PAAR",])
xx <- index(plotting.data)
yy <- plotting.data$NUMERIC_RESULT
library(RColorBrewer)

plot(xx, as.numeric(yy), pch=20,
     col=brewer.pal(unique(sampling.campaign),"Set1")[as.numeric(plotting.data[,ncol(plotting.data)])])
# Add labels
text(xx,as.numeric(yy),labels=xx, cex=.5, pos=2)

# From plot use 
