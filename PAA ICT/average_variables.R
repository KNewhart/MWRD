# Initialize R-PI connection
library(piwebapi)

# Login information
useKerberos <- TRUE
username <- "knewhart"
password <- "Lunabear2@"
validateSSL <- TRUE
debug <- TRUE
piWebApiService <- piwebapi$new("https://pivision/piwebapi", useKerberos, username, password, validateSSL, debug)

pull.and.save <- function(tag, label=NULL, save=TRUE, start='*-365d', end='*') {
  pi.points <- piWebApiService$attribute$getByPath(path=tag)
  pi.data <- piWebApiService$stream$getRecorded(webId = pi.points$WebId, startTime=start, endTime=end)[[2]]
  pi.data <- do.call("rbind", lapply(pi.data, function(x) {
    time.obj <- paste(strsplit(as.character(x[1]), "T")[[1]][1], 
                      strsplit(strsplit(as.character(x[1]), "T")[[1]][2], "Z")[[1]][1])
    time.obj <- lubridate::with_tz(as.POSIXct(time.obj, tz="UTC"), tzone = Sys.timezone())
    value.obj <- x[[2]]
    if(length(value.obj) > 1) value.obj <- NA
    return(data.frame("Timestamp"=time.obj, 
                      "Value" = value.obj))
  })
  )
  pi.data <- na.omit(pi.data)
  all.pi.data <- pi.data
  if(is.null(all.pi.data)) return(NULL)
  
  # If not all of the data was pulled
  if(nrow(pi.data)==1000) {
    needsUpdating <- TRUE
    while(needsUpdating) {
      time.obj <- as.POSIXct(all.pi.data[nrow(all.pi.data), 1], tz=Sys.timezone())
      begin <- paste0(as.character(format(lubridate::with_tz(time.obj, tzone="UTC"),"%Y-%m-%d")),"T",
                              as.character(format(lubridate::with_tz(time.obj, tzone="UTC"),"%H:%M:%S")),"Z")
      pi.data <- piWebApiService$stream$getRecorded(webId = pi.points$WebId, startTime=begin, endTime=end)[[2]]
      pi.data <- do.call("rbind", lapply(pi.data, function(x) {
        time.obj <- paste(strsplit(as.character(x[1]), "T")[[1]][1], 
                          strsplit(strsplit(as.character(x[1]), "T")[[1]][2], "Z")[[1]][1])
        time.obj <- lubridate::with_tz(as.POSIXct(time.obj, tz="UTC"), tzone = Sys.timezone())
        value.obj <- x[[2]]
        if(length(value.obj) > 1) value.obj <- NA
        return(data.frame("Timestamp"=time.obj, 
                          "Value" = value.obj))
      })
      )
      pi.data <- na.omit(pi.data)
      all.pi.data <- rbind(all.pi.data, pi.data)
      if(nrow(pi.data) < 1000) needsUpdating <- FALSE
    }
  }
  
  
  file.name <- tail(strsplit(tail(strsplit(tag,"[\\]")[[1]],n=1),"[|]")[[1]],n=1)
  if(save && is.null(label)) write.csv(all.pi.data, file=paste0(file.name,".csv"), row.names = FALSE)
  if(save && !is.null(label)) write.csv(all.pi.data, file=paste0(label,".csv"), row.names = FALSE)
  if(!save) return(all.pi.data)
}


# Pull all data
# setwd("results/raw_data")
# data.parameters <- matrix(c("\\\\APPLEPI_AF\\MWRD_Production\\Hite Treatment Plant\\19-Permit_Compliance\\Permit Analytics|North Ecoli", "",
#                             "\\\\APPLEPI_AF\\MWRD_Production\\Hite Treatment Plant\\07-Disinfection\\Dis_PAA\\North_PAA\\North_PAA_Dose_Cntrl|DIS N PAA Total Flow (FY_K810)", "15m",
#                             "\\\\APPLEPI_AF\\MWRD_Production\\Hite Treatment Plant\\05-North_Secondary\\NSEC\\Main_Influent_1|Main Inf Channel NSEC (TI_N171)", "24h", 
#                           "\\\\applepi\\TI-R3003", "60m"
#                           ), ncol=2, byrow=TRUE)
# write.csv(data.parameters, file="data-parameters.csv")
data.parameters <- read.csv("data-parameters.csv", stringsAsFactors = FALSE)
all.tags <- as.list(data.parameters[,1])

# ## Start time
# time.obj <- as.POSIXct("2019-06-29 00:00:00", format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
# begin <- paste0(as.character(format(lubridate::with_tz(time.obj, tzone="UTC"),"%Y-%m-%d")),"T",
#                         as.character(format(lubridate::with_tz(time.obj, tzone="UTC"),"%H:%M:%S")),"Z")
# ## End time
# time.obj <- as.POSIXct("2019-08-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone())
# end <- paste0(as.character(format(lubridate::with_tz(time.obj, tzone="UTC"),"%Y-%m-%d")),"T",
#                 as.character(format(lubridate::with_tz(time.obj, tzone="UTC"),"%H:%M:%S")),"Z")

lapply(all.tags, function(tag) pull.and.save(tag=tag, start='*-370d', end='*-5d'))
master.file <- paste0(tail(strsplit(tail(strsplit(all.tags[[1]],"[\\]")[[1]],n=1),"[|]")[[1]],n=1),".csv")
master.obj <- read.csv(file=master.file, stringsAsFactors = FALSE)
master.obj <- master.obj[-1,] # Due to averaging, the first one will not be a fair average
master.timestamps <- master.obj[,1]
for(p in 2:nrow(data.parameters)) {
  file <- paste0(tail(strsplit(tail(strsplit(data.parameters[p,1],"[\\]")[[1]],n=1),"[|]")[[1]],n=1),".csv")
  if(!(file %in% list.files())) next
  obj <- read.csv(file=file, stringsAsFactors = FALSE)
  data <- xts(obj[,2], order.by=as.POSIXct(obj[,1]))
  avg <- data.parameters[p,2]
  if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="m") d <- lubridate::dminutes(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
  if(substr(x=avg,start=nchar(avg), stop=nchar(avg))=="h") d <- lubridate::dhours(x=as.numeric(substr(x=avg, start=1, stop=nchar(avg)-1)))
  all.data <- master.obj
  for(t in 1:length(master.timestamps)) {
    start <- as.POSIXct(master.timestamps[t]) - d
    end <- as.POSIXct(master.timestamps[t])
    weights <- as.numeric(difftime(index(na.omit(data[paste0(start,"/",end),])), start, units="mins"))
    for(i in length(weights):2) weights[i] <- weights[i]-weights[i-1]
    all.data[t,2] <- weighted.mean(na.omit(data[paste0(start,"/",end),]), w=weights)
  }
  master.obj <- cbind(master.obj, all.data[,2])
  colnames(master.obj)[ncol(master.obj)] <- tail(strsplit(tail(strsplit(data.parameters[p,1],"[\\]")[[1]],n=1),"[|]")[[1]],n=1)
}
