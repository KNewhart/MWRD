rm(list=ls())

library(readxl)
library(xts)
library(xlsx)
library(readr)
library(doSNOW)
library(parallel)
library(neuralnet)
library(factoextra)

# Fix timestamps - this function creates an xts object from the piWebApiService function above
fix.timestamps <- function(pi.data) {
  ch.times <- pi.data[,2]
  ch.times <- sub("T", " ", ch.times)
  ch.times <- sub("Z", " ", ch.times)
  return(cbind(ch.times, pi.data[,1]))
}

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


pi.tags <- read_excel("C:/Users/KNewhart/Documents/GitHub/MWRD/pi-tags.xls")
pi.tags <- na.omit(pi.tags)
# Fix tags
for(i in 1:nrow(pi.tags)) {
  if(is.na(pi.tags[i,2])) next
  if(substr(pi.tags[i,2],1,12) == "\\\\APPLEPI_AF") pi.tags[i,2] <- paste0("af:", pi.tags[i,2])
  if(substr(pi.tags[i,2],1,9) == "\\\\applepi") pi.tags[i,2] <- paste0("pi:", pi.tags[i,2])
  if(substr(pi.tags[i,2],1,9) == "\\\\APPLEPI") pi.tags[i,2] <- paste0("pi:", pi.tags[i,2])
}

##### 2016-01-01 Daily #####
FullPlant.Daily.2016 <- c("North Primary Effluent Transfer Flow (MGD)",
                          "North AB1 MLR Flow",
                          "North AB2 MLR Flow",
                          "North AB3 MLR Flow",
                          "North AB4 MLR Flow",
                          "North AB5 MLR Flow",
                          "North AB6 MLR Flow",
                          "North AB7 MLR Flow",
                          "North AB8 MLR Flow",
                          "North AB9 MLR Flow",
                          "North AB10 MLR Flow",
                          "North AB11 MLR Flow",
                          "North AB12 MLR Flow",
                          "GVT Total Influent Flow (gpm)",
                          "South Clarifier 10 Blanket Level",
                          "South Clarifier 2 Blanket Level",
                          "South Clarifier 4 Blanket Level",
                          "South Clarifier 6 Blanket Level",
                          "South Clarifier 8 Blanket Level",
                          "South Clarifier 1 Blanket Level",
                          "South Clarifier 3 Blanket Level",
                          "South Clarifier 5 Blanket Level",
                          "South Clarifier 7 Blanket Level",
                          "South Clarifier 9 Blanket Level",
                          "DWD Reuse N Flow (MGD)",
                          "DWD Reuse S Flow (MGD)"
                          
                          )
FullPlant.Daily.2016.n <- sapply(FullPlant.Daily.2016, function(x) which(x == pi.tags[,1]))

start <- paste0("2016-01-01", "T00:00:00Z")
end <- paste0(as.character(as.Date(Sys.time()) - 1), "T00:00:00Z")

# Load daily data
rm(all.data)
t1 <- Sys.time()
for(i in FullPlant.Daily.2016.n) {
  # assign(make.names(pi.tags[i,1]), piWebApiService$data$getInterpolatedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end, interval = "1d")[,1:2])
  # assign(make.names(pi.tags[i,1]), fix.timestamps(get(make.names(pi.tags[i,1]))))
  # new.objects <- c(new.objects, list(make.names(pi.tags[i,1])))
  
  data.holder <- piWebApiService$data$getInterpolatedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end, interval = "1d")[,1:2]
  data.holder <- fix.timestamps(data.holder)
  data.holder <- xts(data.holder[,2], order.by = as.POSIXct(data.holder[,1], format = "%Y-%m-%d %H:%M:%S", TZ="MST"))
  colnames(data.holder) <- make.names(pi.tags[i,1])
  if (!exists("all.data")) {
    all.data <- data.holder
  } else {
    all.data <- merge(all.data, data.holder)
  }
}
t2 <- Sys.time()
t2-t1


write.xlsx(all.data, file="RWHTF_DataRequest_June2019_KN.xlsx", 
           sheetName="2016-01-01 Daily", append = FALSE)






##### 2018-06-01 Daily #####
FullPlant.Daily.2018 <- c("North Secondary Influent TKN",
                          "North AB9 A NH3",
                          "North AB9 A NO3",
                          "North AB9 C NH3",
                          "North AB9 C NO3",
                          "North AB10 A NH3",
                          "North AB10 A NO3",
                          "North AB10 C NH3",
                          "North AB10 C NO3",
                          "South AB3 RAS Flow",
                          "South AB4 RAS Flow",
                          "South AB4 TSS",
                          "South AB6 Zone 5 Air Flow",
                          "South AB6 Zone 6 Air Flow",
                          "South AB6 Zone 7 Air Flow",
                          "South AB6 Zone 8 Air Flow",
                          "South AB4 Zone 4 NH3",
                          "South AB4 Anoxic Zone NO3",
                          "South AB1 Zone 4 NH3",
                          "South AB2 Zone 4 NH3",
                          "GTE to South (gpm)",
                          "GTE to North (gpm)"
                          )

FullPlant.Daily.2018.n <- sapply(FullPlant.Daily.2018, function(x) which(x == pi.tags[,1]))

start <- paste0("2018-06-01", "T00:00:00Z")
end <- paste0(as.character(as.Date(Sys.time()) - 1), "T00:00:00Z")

# Load daily data
rm(all.data)
t1 <- Sys.time()
for(i in FullPlant.Daily.2018.n) {
  # assign(make.names(pi.tags[i,1]), piWebApiService$data$getInterpolatedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end, interval = "1d")[,1:2])
  # assign(make.names(pi.tags[i,1]), fix.timestamps(get(make.names(pi.tags[i,1]))))
  # new.objects <- c(new.objects, list(make.names(pi.tags[i,1])))
  
  data.holder <- piWebApiService$data$getInterpolatedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end, interval = "1d")[,1:2]
  data.holder <- fix.timestamps(data.holder)
  data.holder <- xts(data.holder[,2], order.by = as.POSIXct(data.holder[,1], format = "%Y-%m-%d %H:%M:%S", TZ="MST"))
  if(length(which(duplicated(index(data.holder)))) > 0) data.holder <- data.holder[-which(duplicated(index(data.holder))),]
  index(data.holder) <- lubridate::ceiling_date(index(data.holder), unit="day")
  colnames(data.holder) <- make.names(pi.tags[i,1])
  
  if (!exists("all.data")) {
    all.data <- data.holder
  } else {
    all.data <- merge(all.data, data.holder)
  }
}
t2 <- Sys.time()
t2-t1

write.xlsx(all.data, file="RWHTF_DataRequest_June2019_KN.xlsx", 
           sheetName="2018-06-01 Daily", append = TRUE)





##### 2018-06-01 15-min #####
FullPlant.15min <- c("North Primary Influent Flow", "North Primary Sludge Flow",
                     "South Primary Influent Flow", "South Primary Sludge Flow",
                     "North Primary Effluent Transfer Flow (MGD)",
                     "North Secondary Influent Flow",
                     "North Secondary Total RAS Flow (MGD)",
                     "North AB1 Flow",
                     "North AB1 A Air",
                     "North AB1 A DO",
                     "North AB1 B Air",
                     "North AB1 B DO",
                     "North AB1 C Air",
                     "North AB2 Flow",
                     "North AB2 A DO",
                     "North AB2 A Air",
                     "North AB2 B DO",
                     "North AB2 B Air",
                     "North AB2 C DO",
                     "North AB2 C Air",
                     "North AB3 Flow",
                     "North AB3 A Air",
                     "North AB3 A DO",
                     "North AB3 B Air",
                     "North AB3 B DO",
                     "North AB3 C Air",
                     "North AB3 C DO",
                     "North AB4 Flow",
                     "North AB4 Flow",
                     "North AB4 A Air",
                     "North AB4 A DO",
                     "North AB4 B Air",
                     "North AB4 B DO",
                     "North AB4 C Air",
                     "North AB4 C DO",
                     "North AB5 Flow",
                     "North AB5 A Air",
                     "North AB5 A DO",
                     "North AB5 B Air",
                     "North AB5 B DO",
                     "North AB5 C Air",
                     "North AB5 C DO",
                     "North RAS 1",
                     "North RAS 2",
                     "North RAS 3",
                     "North RAS 4",
                     "North RAS 5",
                     "North AB1 MLR Flow",
                     "North AB2 MLR Flow",
                     "North AB4 MLR Flow",
                     "North AB3 MLR Flow",
                     "North AB5 RAS Flow",
                     "North AB6 MLR Flow",
                     "North AB7 MLR Flow",
                     "North AB8 MLR Flow",
                     "North AB9 MLR Flow",
                     "North AB10 MLR Flow",
                     "North AB11 MLR Flow",
                     "North AB12 MLR Flow",
                     "North AB9 A NH3",
                     "North AB9 A NO3",
                     "North AB9 C NH3",
                     "North AB9 C NO3",
                     "North AB10 A NH3",
                     "North AB10 A NO3",
                     "North AB10 C NH3",
                     "North AB10 C NO3",
                     "South Secondary Influent Flow",
                     "South AB1 PE Flow",
                     "South AB1 MLR Flow",
                     "South AB1 Zone 5 Air Flow",
                     "South AB1 Zone 5 DO",
                     "South AB1 Zone 6 Air Flow",
                     "South AB1 Zone 6 DO",
                     "South AB1 Zone 7 Air Flow",
                     "South AB1 Zone 7 DO",
                     "South AB1 Zone 8 Air Flow",
                     "South AB1 Zone 8 DO",
                     "South AB1 CaRRB Flow",
                     "South AB1 RAS Flow",
                     "South AB2 MLR Flow",
                     "South AB2 Zone 5 Air Flow",
                     "South AB2 Zone 5 DO",
                     "South AB2 Zone 6 Air Flow",
                     "South AB2 Zone 6 DO",
                     "South AB2 Zone 7 Air Flow",
                     "South AB2 Zone 7 DO",
                     "South AB2 Zone 8 Air Flow",
                     "South AB2 Zone 8 DO",
                     "South AB2 CaRRB Flow",
                     "South AB2 RAS Flow",
                     "South AB3 PE Flow",
                     "South AB3 MLR Flow",
                     "South AB3 Zone 5 Air Flow",
                     "South AB3 Zone 5 DO",
                     "South AB3 Zone 6 Air Flow",
                     "South AB3 Zone 6 DO",
                     "South AB3 Zone 7 Air Flow",
                     "South AB3 Zone 7 DO",
                     "South AB3 Zone 8 Air Flow",
                     "South AB3 Zone 8 DO",
                     "South AB3 CaRRB Flow",
                     "South AB3 RAS Flow",
                     "South AB4 PE Flow",
                     "South AB4 MLR Flow",
                     "South AB4 Zone 5 Air Flow",
                     "South AB4 Zone 5 DO",
                     "South AB4 Zone 6 Air Flow",
                     "South AB4 Zone 6 DO",
                     "South AB4 Zone 7 Air Flow",
                     "South AB4 Zone 7 DO",
                     "South AB4 Zone 8 Air Flow",
                     "South AB4 Zone 8 DO",
                     "South AB4 CaRRB Flow",
                     "South AB4 RAS Flow",
                     "South AB5 PE Flow",
                     "South AB5 MLR Flow",
                     "South AB5 Zone 5 Air Flow",
                     "South AB5 Zone 5 DO",
                     "South AB5 Zone 6 Air Flow",
                     "South AB5 Zone 6 DO",
                     "South AB5 Zone 7 Air Flow",
                     "South AB5 Zone 7 DO",
                     "South AB5 Zone 8 Air Flow",
                     "South AB5 Zone 8 DO",
                     "South AB5 CaRRB Flow",
                     "South AB5 RAS Flow",
                     "South AB6 PE Flow",
                     "South AB6 MLR Flow",
                     "South AB6 Zone 5 Air Flow",
                     "South AB6 Zone 5 DO",
                     "South AB6 Zone 6 Air Flow",
                     "South AB6 Zone 6 DO",
                     "South AB6 Zone 7 Air Flow",
                     "South AB6 Zone 7 DO",
                     "South AB6 Zone 8 Air Flow",
                     "South AB6 Zone 8 DO",
                     "South AB6 CaRRB Flow",
                     "South AB6 RAS Flow",
                     "South AB4 Zone 4 NH3",
                     "South AB4 Anoxic Zone NO3",
                     "South AB1 Zone 4 NH3",
                     "South AB2 Zone 4 NH3",
                     "South Secondary Effluent Flow",
                     "South Secondary Effluent TSS",
                     "Total Effluent Flow",
                     "South CaRRB 1 SWAS Flow",
                     "South CaRRB 2 SWAS Flow",
                     "South CaRRB 3 SWAS Flow",
                     "North CaRRB 1 SWAS Flow",
                     "North CaRRB 2 SWAS Flow",
                     "GVT Total Influent Flow (gpm)",
                     "GVT Total Sludge Effluent Flow (gpm)",
                     "GVT Total Overflow (gpm)",
                     "GVT Total Sludge Influent Flow (gpm)",
                     "North WAS 1",
                     "North WAS 2",
                     "South WAS W",
                     "North CaRRB 1 Air",
                     "North CaRRB 1 Centrate Flow",
                     "North CaRRB DO",
                     "North CaRRB 1 RAS Flow",
                     "North CaRRB 3 Air",
                     "North CaRRB 3 Centrate Flow",
                     "North CaRRB DO",
                     "North CaRRB 3 RAS Flow",
                     "South CaRRB 1 DO A",
                     "South CaRRB 1 DO B",
                     "South CaRRB 1 Air A",
                     "South CaRRB 1 Air B",
                     "South CaRRB RAS Flow",
                     "South CaRRB 1 Splitter",
                     "South CaRRB 1 Centrate Flow",
                     "South CaRRB 2 DO A",
                     "South CaRRB 2 DO B",
                     "South CaRRB 2 Air A",
                     "South CaRRB 2 Air B",
                     "South CaRRB 2 Splitter",
                     "South CaRRB 2 Centrate Flow",
                     "South CaRRB 3 DO A",
                     "South CaRRB 3 DO B",
                     "South CaRRB 3 Air A",
                     "South CaRRB 3 Air B",
                     "South CaRRB 3 Splitter",
                     "South CaRRB 3 Centrate Flow",
                     "DAF Subnatant Flow (MGD)",
                     "DAF CONWAS Flow (gpm)",
                     "DAF IPW Flow (MGD)",
                     "DAF Polymer Feed 1 (gpm)",
                     "DAF Polymer Feed 2 (gpm)",
                     "DAF Polymer Feed 3 (gpm)",
                     "DAF Polymer Feed 4 (gpm)",
                     "DAF Polymer Feed 5 (gpm)",
                     "DAF Polymer Feed 6 (gpm)",
                     "DAF Polymer Feed 7 (gpm)",
                     "DAF Polymer Feed 8 (gpm)",
                     "DAF Polymer Feed 9 (gpm)",
                     "DAF WAS 1 (gpm)",
                     "DAF WAS 2 (gpm)",
                     "DAF WAS 3 (gpm)",
                     "DAF WAS 4 (gpm)",
                     "DAF WAS 5 (gpm)",
                     "DAF WAS 6 (gpm)",
                     "DAF WAS 7 (gpm)",
                     "DAF WAS 8 (gpm)",
                     "South SWAS Flow (gpm)",
                     "Digester Effluent Flow 1 (gpm)",
                     "Digester Supernatant Flow (gpm)",
                     "Digester Effluent Flow 2 (gpm)",
                     "Digester Sludge Feed Flow 1 (gpm)",
                     "Digester Sludge Feed Flow 2 (gpm)",
                     "Digester Acid Sludge Feed 1 (gpm)",
                     "Digester Acid Sludge Feed 2 (gpm)",
                     "GVT 1 Influent Flow (gpm)",
                     "GVT 1 Sludge Flow (gpm)",
                     "GVT 2 Influent Flow (gpm)",
                     "GVT 2 Sludge Flow (gpm)",
                     "GVT 3 Influent Flow (gpm)",
                     "GVT 3 Sludge Flow (gpm)",
                     "GVT 4 Influent Flow (gpm)",
                     "GVT 4 Sludge Flow (gpm)",
                     "GVT Total Sludge Effluent Flow (gpm)",
                     "GVT Total Overflow (gpm)",
                     "GVT Total Sludge Influent Flow (gpm)",
                     "Digester Total Gas",
                     "Centrifuge Flow Total (gpm)",
                     "Centrifuge Sludge Flow (gpm)",
                     
                     "DMX 1 pH",
                     "DMX 1 DO",
                     "DMX 1 Temperature (C)",
                     "DMX 1 Air",
                     "DMX 1 NH3",
                     "DMX 1 NO3",
                     "DMX 2 pH",
                     "DMX 2 DO",
                     "DMX 2 Temperature (C)",
                     "DMX 2 Air",
                     "DMX 2 NH3",
                     "DMX 2 NO3",
                     "DMX Effluent to NCaRRB",
                     "DMX Effluent to SCaRRB",
                     "DMX Centrate Flow"
                     
                     
                     
                     
)

FullPlant.15min.n <- sapply(FullPlant.15min, function(x) which(x == pi.tags[,1]))

start <- paste0("2018-06-01", "T00:00:00Z")
end <- paste0(as.character(as.Date(Sys.time()) - 1), "T00:00:00Z")

# Load daily data
rm(all.data)
t1 <- Sys.time()
for(i in FullPlant.15min.n) {
  # assign(make.names(pi.tags[i,1]), piWebApiService$data$getInterpolatedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end, interval = "1d")[,1:2])
  # assign(make.names(pi.tags[i,1]), fix.timestamps(get(make.names(pi.tags[i,1]))))
  # new.objects <- c(new.objects, list(make.names(pi.tags[i,1])))
  
  data.holder <- piWebApiService$data$getInterpolatedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end, interval = "15m")[,1:2]
  data.holder <- fix.timestamps(data.holder)
  data.holder <- data.holder[-which(is.na(as.POSIXct(data.holder[,1], format = "%Y-%m-%d %H:%M:%S", TZ="MST"))),]
  data.holder <- xts(data.holder[,2], order.by = as.POSIXct(data.holder[,1], format = "%Y-%m-%d %H:%M:%S", TZ="MST"))
  colnames(data.holder) <- make.names(pi.tags[i,1])
  if(length(which(duplicated(index(data.holder)))) > 0) data.holder <- data.holder[-which(duplicated(index(data.holder))),]
  if (!exists("all.data")) {
    all.data <- data.holder
  } else {
    all.data <- merge(all.data, data.holder)
  }
}
t2 <- Sys.time()
t2-t1

# write.xlsx(all.data, file="RWHTF_DataRequest_June2019_KN.xlsx", 
#            sheetName="2018-06-01 15-min", append = TRUE)
write.csv(all.data, file="RWHTF_DataRequest_June2019_Full_15min.csv", row.names = index(all.data))



##### AB2 2018-06-01 15-min #####
AB2.15min <- c("North AB2 Flow", "North AB2 MLR Flow","North AB2 RAS Flow",
               "North AB2 PE Flow", "North AB2 A RAS Flow", "North AB2 WAS Flow",
               "Cyclone overflow (gpm)", "Cyclone underflow (gpm)",
               "North AB2 A Air",
               "North AB2 A DO",
               "North AB2 B Air",
               "North AB2 B DO",
               "North AB2 B NH3",
               "North AB2 B NO3",
               "North AB2 C Air",
               "North AB2 C DO",
               "North AB2 C NH3",
               "North AB2 TSS",
               "North Clarifier 2 RAS Flow",
               "North Clarifier 2 Sludge Blanket",
               "North Clarifier 4 Sludge Blanket",
               "North AB4 A Air",
               "North AB4 A DO",
               "North AB4 B Air",
               "North AB4 B DO",
               "North AB4 C Air",
               "North AB4 C DO",
               "North AB4 Flow",
               "North AB4 MLR Flow",
               "North AB4 TSS",
               "North Secondary Influent NH3",
               "North Secondary Influent COD",
               "North Secondary Influent NO3",
               "North Secondary Influent TSS",
               "North Q1 RAS Flow",
               "North Q2 RAS Flow",
               "North Q3 RAS Flow",
               "North Q4 RAS Flow",
               "North Secondary Influent Flow",
               "North AB4 Influent",
               "North Clarifier 4 RAS Flow"
               
               
                     
                     
)

AB2.15min.n <- sapply(AB2.15min, function(x) which(x == pi.tags[,1]))

start <- paste0("2018-06-01", "T00:00:00Z")
end <- paste0(as.character(as.Date(Sys.time()) - 1), "T00:00:00Z")

# Load daily data
rm(all.data)
t1 <- Sys.time()
for(i in AB2.15min.n) {
  # assign(make.names(pi.tags[i,1]), piWebApiService$data$getInterpolatedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end, interval = "1d")[,1:2])
  # assign(make.names(pi.tags[i,1]), fix.timestamps(get(make.names(pi.tags[i,1]))))
  # new.objects <- c(new.objects, list(make.names(pi.tags[i,1])))
  
  data.holder <- piWebApiService$data$getInterpolatedValues(path=unlist(pi.tags[i,2]), startTime = start, endTime = end, interval = "15m")[,1:2]
  data.holder <- fix.timestamps(data.holder)
  data.holder <- data.holder[-which(is.na(as.POSIXct(data.holder[,1], format = "%Y-%m-%d %H:%M:%S", TZ="MST"))),]
  data.holder <- xts(data.holder[,2], order.by = as.POSIXct(data.holder[,1], format = "%Y-%m-%d %H:%M:%S", TZ="MST"))
  colnames(data.holder) <- make.names(pi.tags[i,1])
  if(length(which(duplicated(index(data.holder)))) > 0) data.holder <- data.holder[-which(duplicated(index(data.holder))),]
  if (!exists("all.data")) {
    all.data <- data.holder
  } else {
    all.data <- merge(all.data, data.holder)
  }
}
t2 <- Sys.time()
t2-t1

# write.xlsx(all.data, file="RWHTF_DataRequest_June2019_KN.xlsx", 
           # sheetName="AB2 2018-06-01 15-min", append = TRUE)
write.csv(all.data, file="RWHTF_DataRequest_June2019_AB2_15min.csv",
          row.names = index(all.data))
