# Install and load xts package to work with timeseries objects
install.packages("xts")
library(xts)
# Install and load piwebapi package from Github
install.packages("devtools")
library(devtools)
install_github("rbechalany/PI-Web-API-Client-R")
library(piwebapi)
# Pi Login information
useKerberos <- TRUE
username <- "REPLACE WITH YOUR USERNAME, ie. knewhart"
password <- "REPLACE WITH YOUR PASSWORD"
validateSSL <- TRUE
debug <- TRUE
piWebApiService <- piwebapi$new("https://pivision/piwebapi", useKerberos, username, password, validateSSL, debug)

# Set desired times to pull data 
date.time <- as.POSIXct(paste(Sys.Date(),"08:00"))-0:365*60*60*24 

# Convert times to PI format
pi.times <- matrix(NA,nrow=length(date.time),ncol=1)
for(i in 1:length(date.time)) {
  time.obj <- date.time[i]
  time.obj <- lubridate::with_tz(time.obj, tzone="UTC")
  pi.times[i,1] <- paste0(as.character(format(time.obj,"%Y-%m-%d")),"T",
                          as.character(format(time.obj,"%H:%M:%S")),"Z")
}

# Declare with variables/pi tags to pull
pi.tags <- matrix(c("GVT SRT", "\\\\applepi\\GVT_SRT_Daily",
                    "GVT Inf TSS", "\\\\applepi\\GVT_inf_TS_inline_avg",
                    "GTE TSS", "\\\\applepi\\GVT_Eff_TSS_Conc_Daily",
                    "GVT Level", "\\\\applepi\\GVT_Level_Daily",
                    "GVT HLR", "\\\\applepi\\GVT_HLR_Daily",
                    "GVT SLR", "\\\\applepi\\GVT_SLR_Daily",
                    "TPS Flow", "\\\\applepi\\GVT_TPS_Flow_Daily",
                    "TPS TS", "\\\\applepi\\GVT_TPS_Percent_TS_Daily"), ncol=2, byrow=TRUE)

# Pull data from PI and create single data object (all.data)
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

# Fix timestamps from UTC to MT
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

# Save compiled data
raw.data <- all.data

# Create time-based object
rownames(all.data) <- c()
all.data <- xts(all.data[,-1], order.by=all.data[,1])
all.data <- apply(all.data, 2, as.numeric)

# Plot all variables
par(mar=c(2,2,2,2), mfrow=c(4,2))
for(i in 1:ncol(all.data)) {
  plot(as.zoo(all.data)[,i], main=colnames(all.data)[i])
}

# Plot just level and effluent TSS
par(mar=c(4,4,1,1), mfrow=c(1,1))
plot(x=as.zoo(all.data)[,4], y=as.zoo(all.data)[,3], 
     xlab="Average GVT Level (ft)", ylab="GVT Effluent TSS (mg/L)",
     pch=20)
abline(lm(GTE.TSS~GVT.Level, data=data.frame(all.data)), lwd=4)

# Plot scaled level and effluent TSS
par(mar=c(4,4,1,1), mfrow=c(1,1))
plot(x=as.zoo(scale(all.data))[,4], y=as.zoo(scale(all.data))[,3], 
     xlab="Average GVT Level", ylab="GVT Effluent TSS",
     pch=20)
abline(lm(GTE.TSS~GVT.Level, data=data.frame(scale(all.data)[-which(scale(all.data)[,3] > 2),])), lwd=4)

# Plot level and effluent TSS (without outliers)

# Plot scaled level and effluent TSS
par(mar=c(4,4,1,1), mfrow=c(1,1))
plot(x=as.zoo(all.data)[,4], y=as.zoo(all.data)[,3], 
     xlab="Average GVT Level", ylab="GVT Effluent TSS",
     pch=20)
lm.results <- lm(GTE.TSS~GVT.Level, data=data.frame(all.data[-which(scale(all.data)[,3] > 2),]))
abline(lm.results, lwd=4)
text(labels=summary(lm.results)$r.squared, x=min(all.data[,4]), y=max(all.data[,3]), pos=4)


# Confidence interval
df <- data.frame(x = all.data[,4],
                 y = all.data[,3])
plot(y ~ x, data = df)
mod <- lm(y ~ x, data = df)
newx <- seq(min(df$x), max(df$x), length.out=100)
preds <- predict(mod, newdata = data.frame(x=newx), 
                 interval = 'confidence')
plot(y ~ x, data = df, type = 'n', xlab="Average GVT Level (ft)", ylab="GVT Effluent TSS (mg/L)")
polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = 'grey80', border = NA)
abline(mod)
lines(newx, preds[ ,3], lty = 'dashed', col = 'red')
lines(newx, preds[ ,2], lty = 'dashed', col = 'red')
points(df, pch=20)