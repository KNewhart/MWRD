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
var <- c("North Pre-PAA E. coli")

# Set time
start <- paste0("2019-01-01", "T06:00:00Z") # Make sure to convert time to GMT
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
  data.holder <- xts(as.numeric(data.holder[,2]), order.by = as.POSIXct(data.holder[,1], format = "%Y-%m-%d %H:%M:%S", TZ="GMT"))
  colnames(data.holder) <- make.names(pi.tags[i,1])
  if (!exists("all.data")) {
    all.data <- data.holder
  } else {
    all.data <- merge(all.data, data.holder)
  }
}

## Do something
mod.data <- all.data








ecoli <- all.data

timestamps <- index(all.data)
new.timestamps <- stringr::str_c(stringr::str_sub(as.character(timestamps),1,10), "T")
new.timestamps <- stringr::str_c(new.timestamps, stringr::str_sub(as.character(timestamps),12,19),"Z")

# 
# start.and.end <- matrix(nrow=nrow(ecoli)-1,ncol=2)
# library(stringr)
# for(row in 2:nrow(ecoli)){
#   start <- index(ecoli)[row-1]
#   end <- index(ecoli)[row]
#   start.and.end[row-1,1] <- str_c(str_sub(as.character(start),1,10), "T",str_sub(as.character(start),12,19),"Z")
#   start.and.end[row-1,2] <- str_c(str_sub(as.character(end),1,10), "T",str_sub(as.character(end),12,19),"Z")
# }
var <- c(
  "North Clarifier 1 Sludge Blanket", # Clarifiers selected from timeseries plots
  "North Clarifier 6 Sludge Blanket", # See pre-dis-ecoli-sludge-blanket.png
  "North Clarifier 11 Sludge Blanket",
  "North Clarifier 12 Sludge Blanket",
  "North AB5 RAS TSS",
  "North AB4 TSS",
  "North AB10 RAS TSS",
  "North AB9 RAS TSS",
  "North Secondary Aerobic SRT",
  "North Secondary Effluent NH3",
  "North Secondary Effluent NO3",
  "North Secondary Effluent OP",
  "North Secondary Effluent TSS",
  "North Secondary Effluent NO5",
  "North Secondary Influent COD",
  "North Secondary Influent Flow",
  "North Secondary Influent NH3",
  "North Secondary Influent NH3-N",
  "North Secondary Influent NO3",
  "North Secondary Influent Temperature",
  # "North Secondary Influent TKN",
  "North Secondary Influent TSS",
  "North Secondary Total RAS Flow (MGD)"
  # "North Disinfection Flow"
)

var.n <- sapply(var, function(x) which(x == pi.tags[,1]))
# 
# # Load daily data
# if (exists("all.data")) rm(all.data)
# for(t in new.timestamps) {
#   data.holder <- piWebApiService$data$getMultipleRecordedValues(paths=c(unlist(pi.tags[var.n,2])), startTime = t)
#   data.holder <- lapply(data.holder, function(x) fix.timestamps(x[1,1:2]))
#   data.holder <- lapply(data.holder, function(x) xts(as.numeric(x[,2]), order.by = as.POSIXct(x[,1], format = "%Y-%m-%d %H:%M:%S", TZ="MST")-6*60*60))
#   data.holder <- do.call(merge, data.holder)
#   colnames(data.holder) <- var
#   if (!exists("all.data")) {
#     all.data <- data.holder
#   } else {
#     all.data <- rbind(all.data, data.holder)
#   }
# }
# save(file = "pre-dis-ecoli-all-data.RData","all.data")
# load("pre-dis-ecoli-all-data.RData")

source("GetPiData.R")
v.data <- data.frame()
for(v in var.n) {
  t.data <- data.frame()
  p <- as.character(unlist(pi.tags[v,2]))
  for(t in 1:length(timestamps)) {
    data <- getPiData(path=p, t=timestamps[t], i=15, n=1)
    
    row <- data[round(nrow(data)/2),2]
    data <- data.frame(data[round(nrow(data)/2),1])
    rownames(data) <- row
    
    t.data <- rbind(t.data, data)
  }
  if(nrow(v.data) != 0) v.data <- cbind(v.data, t.data)
  if(nrow(v.data) == 0) v.data <- t.data
  colnames(v.data)[ncol(v.data)] <- as.character(unlist(pi.tags[v,1]))
  print(paste(Sys.time(), "finished", colnames(v.data)[ncol(v.data)]))
}

ch.times <- row.names(v.data)
ch.times <- sub("T", " ", ch.times)
ch.times <- sub("Z", " ", ch.times)

all.data <- xts(v.data, order.by = as.POSIXct(ch.times, "%Y-%m-%d %H:%M:%S", tz=""))
all.data <- cbind(all.data, ecoli)
cols2remove <- c("North.Secondary.Influent.COD", "North.Secondary.Influent.NO3", "North.Secondary.Influent.TSS")
rows2remove <- c("North.Secondary.Influent.NH3") # try not removing it in next iterations
cols2remove <- sapply(cols2remove, function(x) which(x == colnames(all.data)))
rows2remove <- sapply(rows2remove, function(x) which(x == colnames(all.data)))
all.data <- all.data[which(!is.na(all.data[,rows2remove])), -cols2remove]
save("all.data", file="pre-dis-ecoli-2.RData")










require(parallel)
require(neuralnet)
require(doSNOW)

# Load daily data
  if (exists("all.data")) rm(all.data)
  all.data <- xts()
  
  # detect cores with parallel() package
  nCores <- detectCores(logical = FALSE)
  # detect threads with parallel()
  nThreads<- detectCores(logical = TRUE)
  
  # Create doSNOW compute cluster
  cluster = makeCluster(nThreads, type = "SOCK")
  class(cluster);
  
  # register the cluster
  registerDoSNOW(cluster)
  
  # for(i in var.n){
  # # all.data <- foreach::foreach(i = var.n, .combine=cbind) %dopar% {
  #  
  #   data.holder <- xts()
  # 
  #   data.holder <- foreach::foreach(j = 1:nrow(start.and.end), 
  #                                   .combine = rbind, 
  #                                   # .errorhandling = 'pass',
  #                                   .packages=(c("xts", "piwebapi"))) %dopar% {
  #     tryCatch({
  #       data <- piWebApiService$data$getRecordedValues(path=as.character(pi.tags[i,2]), startTime = start.and.end[j,1], endTime = start.and.end[j,2])[,1:2]
  #       data <- fix.timestamps(data[nrow(data),1:2])
  #       x <- which(is.na(as.POSIXct(data[,1], format="%Y-%m-%d %H:%M:%S")))
  #       data <- xts(as.numeric(data[-x,2]), order.by = as.POSIXct(data[-x,1], format = "%Y-%m-%d %H:%M:%S", TZ="GMT"))
  #       return(data)
  #     }, error = function(e) {return(NULL)})
  #     
  #   }
  #   
  #   colnames(data.holder) <- as.character(pi.tags[i,1])
  #   all.data <- cbind(all.data, data.holder)
  # }
  start <- paste0("2019-01-01", "T06:00:00Z") # Make sure to convert time to GMT
  end <- paste0(as.character(as.Date(Sys.time()) - 1), "T00:00:00Z")
  all.data <- foreach::foreach(i=var.n, .combine=cbind) %dopar% {
    library(xts)
    library(piwebapi)
    holder <- piWebApiService$data$getInterpolatedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end, interval = "15m")[,1:2]
    # holder <- piWebApiService$data$getRecordedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end)[,1:2]
    holder <- fix.timestamps(holder)
    x <- which(is.na(as.POSIXct(holder[,1], format="%Y-%m-%d %H:%M:%S")))
    holder <- xts(as.numeric(holder[-x,2]), order.by = as.POSIXct(holder[-x,1], format="%Y-%m-%d %H:%M:%S"))
    keeper <- holder
    # while(nrow(holder) == 1000) { # Then there are more datapoints to pull
    #   new.start <- range(index(holder))[2]
    #   new.start <- paste0(substr(as.character(new.start),1,10), "T00:00:00Z")
    #   holder <- piWebApiService$data$getInterpolatedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end, interval = "15m")[,1:2]
    #   # holder <- piWebApiService$data$getRecordedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end)[,1:2]
    #   holder <- fix.timestamps(holder)
    #   holder <- xts(as.numeric(holder[,2]), order.by = as.POSIXct(holder[,1]))
    #   keeper <- rbind(keeper, holder)
    # }
    # if (nrow(keeper) != nrow(holder)) keeper <- rbind(keeper, holder)
    colnames(keeper) <- make.names(pi.tags[i,1])
    keeper
  }
  # stop cluster and remove clients
  stopCluster(cluster)
  
  # insert serial backend, otherwise error in repetetive tasks
  registerDoSEQ()
  
  # clean up a bit.
  invisible(gc); remove(nCores); remove(nThreads); remove(cluster); 
  
  
  all.data.ex <- cbind(ecoli, all.data)
  
  
  
  
  
  
  
# 
#   for(i in 1:nrow(start.and.end)) {
#   # for(i in 1:5) {
#     # data.holder <- piWebApiService$data$getMultipleRecordedValues(paths=c(unlist(pi.tags[var.n,2])), startTime = start.and.end[i,1], endTime=start.and.end[i,2])
#     data.holder <- xts()
#     for(j in var.n) {
#       tryCatch({
#         data <- piWebApiService$data$getRecordedValues(path=as.character(pi.tags[j,2]), startTime = start.and.end[i,1], endTime = start.and.end[i,2])[,1:2]
#         data <- fix.timestamps(data[nrow(data),1:2])
#         data <- xts(as.numeric(data[,2]), order.by = as.POSIXct(data[,1], format = "%Y-%m-%d %H:%M:%S", TZ="MST")-6*60*60)
#         colnames(data) <- as.character(pi.tags[j,1])
#         if(length(data.holder) > 0) data.holder <- merge(data.holder, data)
#         if(length(data.holder) == 0) data.holder <- data
#         rm(data)
#       }, error = function(e) {
#         print(paste(pi.tags[j,1], "not found"))
#         
#       })
#     }
#     
#     data.holder <- na.locf(data.holder)[nrow(data.holder)]
#     # data.holder <- lapply(data.holder, function(x) fix.timestamps(x[,1:2]))
#     # data.holder <- lapply(data.holder, function(x) xts(as.numeric(x[,2]), order.by = as.POSIXct(x[,1], format = "%Y-%m-%d %H:%M:%S", TZ="MST")-6*60*60))
#     # data.holder <- do.call(merge, data.holder)
#     
#     
#     if (!exists("all.data")) {
#       all.data <- data.holder
#     } else {
#       if(ncol(data.holder) > ncol(all.data)){
#         all.data <- rbind(all.data, data.holder[,(colnames(data.holder) %in% colnames(all.data))])
#         if(ncol(all.data) != ncol(data.holder)) all.data <- cbind(all.data, data.holder[,!(colnames(data.holder) %in% colnames(all.data))], fill=NA)
#       } else{
#         if((ncol(data.holder) < ncol(all.data))) {
#           x <- xts(NA, order.by=index(data.holder))
#           colnames(x) <- colnames(all.data)[which(!(colnames(all.data) %in% colnames(data.holder)))]
#           data.holder <- cbind(data.holder, x)
#           all.data <- rbind(all.data, data.holder)
#         } else{
#           if(ncol(data.holder) ==ncol(all.data)) all.data <- rbind(all.data, data.holder)
#         }
#         
#       }
#       
#     }
#   }
  




process <- all.data

all.data.ex <- merge(ecoli,process)
rm(all.data)
bottom.out <- which(!is.na(all.data.ex[,1]))
# clean.data <- xts()
# for(i in 1:length(bottom.out)){
#   if(length(clean.data) == 0) j <- c(1:bottom.out[1])
#   if(length(clean.data) > 0) j <- c(bottom.out[i-1]:bottom.out[i])
#   sub.data <- na.locf(all.data.ex[j,])
#   if(length(clean.data) > 0) clean.data <- rbind(clean.data, sub.data[nrow(sub.data),])
#   if(length(clean.data) == 0) clean.data <- sub.data[nrow(sub.data),]
# }
# save(file = "pre-dis-ecoli-clean-data.RData","clean.data")
load("pre-dis-ecoli-clean-data.RData")

source("C:\\Users\\KNewhart\\Documents\\GitHub\\MWRD\\Neural Networks\\NNopt.R")
NN.ecoli <- NNopt(all.data = clean.data, 
                  predict.col.name = "North.Pre.PAA.E..coli",
                  percent.train = 0.8, 
                  training.index = NULL)