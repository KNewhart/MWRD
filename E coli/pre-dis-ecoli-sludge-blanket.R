rm(list=ls())
library(xts)
library(readxl)
# devtools::install_github("rbechalany/PI-Web-API-Client-R")
library(piwebapi)

###### Setup Pi #####
# Login information
useKerberos <- TRUE
username <- "knewhart"
password <- "Lunabear2@"
validateSSL <- TRUE
debug <- TRUE
piWebApiService <- piwebapi$new("https://pivision/piwebapi", useKerberos, username, password, validateSSL, debug)
# Load tags
pi.tags <- read_excel("C:/Users/KNewhart/Documents/GitHub/MWRD/pi-tags.xls")
pi.tags <- na.omit(pi.tags)
# Fix tags
for(i in 1:nrow(pi.tags)) {
  if(is.na(pi.tags[i,2])) next
  if(substr(pi.tags[i,2],1,12) == "\\\\APPLEPI_AF") pi.tags[i,2] <- paste0("af:", pi.tags[i,2])
  if(substr(pi.tags[i,2],1,9) == "\\\\applepi") pi.tags[i,2] <- paste0("pi:", pi.tags[i,2])
  if(substr(pi.tags[i,2],1,9) == "\\\\APPLEPI") pi.tags[i,2] <- paste0("pi:", pi.tags[i,2])
}
# Select variables
var <- c("North PAA '1-min'",
         # "Chemscan PAA", # incomplete data, PI tag disconnected?
         "North Pre-PAA E. coli",
         "North Post-PAA E. coli")
# Set time
start <- paste0("2018-01-01", "T06:00:00Z") # Make sure to convert time to GMT
end <- paste0(as.character(as.Date(Sys.time()) - 1), "T00:00:00Z")
var.n <- sapply(var, function(x) which(x == pi.tags[,1]))
# Fix timestamps - this function creates an xts object from the piWebApiService function above
fix.timestamps <- function(pi.data) {
  ch.times <- pi.data[,2]
  ch.times <- sub("T", " ", ch.times)
  ch.times <- sub("Z", " ", ch.times)
  return(cbind(ch.times, pi.data[,1]))
}
# Load daily data
if (exists("all.data")) rm(all.data)
for(i in var.n) {
  # assign(make.names(pi.tags[i,1]), piWebApiService$data$getInterpolatedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end, interval = "1d")[,1:2])
  # assign(make.names(pi.tags[i,1]), fix.timestamps(get(make.names(pi.tags[i,1]))))
  # new.objects <- c(new.objects, list(make.names(pi.tags[i,1])))
  data.holder <- piWebApiService$data$getRecordedValues(path=as.character(pi.tags[i,2]), startTime = start, endTime = end)[,1:2]
  # data.holder <- piWebApiService$data$getInterpolatedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end, interval = "1d")[,1:2]
  data.holder <- fix.timestamps(data.holder)
  data.holder <- xts(as.numeric(data.holder[,2]), order.by = as.POSIXct(data.holder[,1], format = "%Y-%m-%d %H:%M:%S", TZ="MST")-6*60*60)
  colnames(data.holder) <- make.names(pi.tags[i,1])
  if (!exists("all.data")) {
    all.data <- data.holder
  } else {
    all.data <- merge(all.data, data.holder)
  }
}


new.data <- na.locf(all.data)
new.data <- cbind(new.data, log(as.numeric(new.data[,2])/as.numeric(new.data[,3]), base=10))
colnames(new.data)[4] <- "Log.Removal"
ts.plot(new.data[,4], gpars=list(main="Log Removal"))


##### What other variables? #####
var <- c("North Clarifier 1 Sludge Blanket",
         "North Clarifier 2 Sludge Blanket",
         "North Clarifier 3 Sludge Blanket",
         "North Clarifier 5 Sludge Blanket",
         "North Clarifier 6 Sludge Blanket",
         "North Clarifier 7 Sludge Blanket",
         "North Clarifier 8 Sludge Blanket",
         "North Clarifier 9 Sludge Blanket",
         "North Clarifier 10 Sludge Blanket",
         "North Clarifier 11 Sludge Blanket",
         "North Clarifier 12 Sludge Blanket")
         
# Set time
start <- paste0("2018-01-01", "T06:00:00Z") # Make sure to convert time to GMT
end <- paste0(as.character(as.Date(Sys.time()) - 1), "T00:00:00Z")
var.n <- sapply(var, function(x) which(x == pi.tags[,1]))

# Load daily data
if (exists("all.data")) rm(all.data)
for(i in var.n) {
  # data.holder <- piWebApiService$data$getRecordedValues(path=as.character(pi.tags[i,2]), startTime = start, endTime = end)[,1:2]
  data.holder <- piWebApiService$data$getInterpolatedValues(path=as.character(pi.tags[i,2]), startTime = start, endTime = end, interval = "1d")[,1:2]
  data.holder <- fix.timestamps(data.holder)
  data.holder <- xts(as.numeric(data.holder[,2]), order.by = as.POSIXct(data.holder[,1], format = "%Y-%m-%d %H:%M:%S", TZ="MST")-6*60*60)
  colnames(data.holder) <- make.names(pi.tags[i,1])
  if (!exists("all.data")) {
    all.data <- data.holder
  } else {
    all.data <- merge(all.data, data.holder)
  }
}

newer.data <- na.locf(merge(all.data, new.data))
newer.data <- cbind(difftime(index(newer.data),index(newer.data)[1],units="days"), newer.data)
colnames(newer.data)[1] <- "Runtime"

png("pre-dis-ecoli-sludge-blanket.png", res=600, units="in", width=8, height=5)
par(mfrow=c(2,1), mar=c(2,4.5,2,1), oma=c(2,0,0,0))
# Plot 1
plot(data.frame(x = newer.data$Runtime, y = log(newer.data$North.Pre.PAA.E..coli, base=10)), type="l", xlab="", ylab="")
mtext(side=2, "Log(N)", line=3)
mtext(side=3, "Pre-disinfection E.coli", font=2, line=0.5)
# Plot 2
plot(data.frame(x = newer.data$Runtime, y = newer.data$North.Clarifier.1.Sludge.Blanket), ylab="", xlab="", col="blue", type = "l")
lines(data.frame(x = newer.data$Runtime, y = newer.data$North.Clarifier.6.Sludge.Blanket), col="red")
lines(data.frame(x = newer.data$Runtime, y = newer.data$North.Clarifier.11.Sludge.Blanket), col="orange")
lines(data.frame(x = newer.data$Runtime, y = newer.data$North.Clarifier.12.Sludge.Blanket), col="black")
mtext(side=2, "Depth (ft)", line=3)
mtext(side=3, "Clarifier Sludge Blanket", font=2, line=0.5)
mtext(side=1, line=0.5, "Days", outer=TRUE)
dev.off()


png("ecoli-removal-sludge-blanket.png", res=600, units="in", width=8, height=5)
par(mfrow=c(2,1), mar=c(2,4.5,2,1), oma=c(2,0,0,0))
# Plot 1
plot(data.frame(x = newer.data$Runtime, y = log(newer.data$North.Pre.PAA.E..coli/newer.data$North.Post.PAA.E..coli, base=10)), type="l", xlab="", ylab="")
mtext(side=2, "Log(N0/N)", line=3)
mtext(side=3, "E.coli removal", font=2, line=0.5)
# Plot 2
plot(data.frame(x = newer.data$Runtime, y = newer.data$North.Clarifier.1.Sludge.Blanket), ylab="", xlab="", col="blue", type = "l")
lines(data.frame(x = newer.data$Runtime, y = newer.data$North.Clarifier.6.Sludge.Blanket), col="red")
lines(data.frame(x = newer.data$Runtime, y = newer.data$North.Clarifier.11.Sludge.Blanket), col="orange")
lines(data.frame(x = newer.data$Runtime, y = newer.data$North.Clarifier.12.Sludge.Blanket), col="black")
mtext(side=2, "Depth (ft)", line=3)
mtext(side=3, "Clarifier Sludge Blanket", font=2, line=0.5)
mtext(side=1, line=0.5, "Days", outer=TRUE)
dev.off()

