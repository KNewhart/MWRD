# Function to import pi data
piPush <- function(tag, time, val) {
  # Initialize R-PI connection
  if(!("piwebapi" %in% installed.packages()[,1])) {
    install.packages("devtools")
    library(devtools)
    install_github("rbechalany/PI-Web-API-Client-R")
  }
  require(piwebapi)

  
  # Login information
  useKerberos <- TRUE
  username <- "knewhart"
  password <- ""
  validateSSL <- TRUE
  debug <- TRUE
  piWebApiService <- piwebapi$new("https://pivision/piwebapi", useKerberos, username, password, validateSSL, debug)
  
  # If time is a POSIXct object, convert start and end time to character string that PI understands
  if(inherits(time, "POSIXct")) time <- paste0(as.character(format(lubridate::with_tz(time, tzone="UTC"),"%Y-%m-%d")),"T",
                                                 as.character(format(lubridate::with_tz(time, tzone="UTC"),"%H:%M:%S")),"Z")
  # Pull WebID associated with PI tag
  pi.points <- piWebApiService$point$getByPath(path=tag)
  
  timedValue <- PITimedValue(timestamp=time, value=as.numeric(round(val,2)))
  s <- PIStreamValue(webId = pi.points$WebId, value=timedValue)
  
  response <- piWebApiService$stream$updateValue(webId=pi.points$WebId, PITimedValue=timedValue,
                                                 bufferOption="Buffer")
  return(data.frame("Time"=time,
                    "Value"=as.numeric(round(val,2))))
}
