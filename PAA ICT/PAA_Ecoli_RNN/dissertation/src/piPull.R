# Function to import pi data
piPull <- function(tag, label=NULL, save=TRUE, obj.return = FALSE, start='*-365d', end='*') {
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
  if(inherits(start, "POSIXct")) start <- paste0(as.character(format(lubridate::with_tz(start, tzone="UTC"),"%Y-%m-%d")),"T",
                                                 as.character(format(lubridate::with_tz(start, tzone="UTC"),"%H:%M:%S")),"Z")
  if(inherits(end, "POSIXct")) end <- paste0(as.character(format(lubridate::with_tz(end, tzone="UTC"),"%Y-%m-%d")),"T",
                                             as.character(format(lubridate::with_tz(end, tzone="UTC"),"%H:%M:%S")),"Z")
  
  # Pull WebID associated with PI tag
  pi.points <- piWebApiService$attribute$getByPath(path=tag)
  
  # Pull PI tag/WebID from "start"
  pi.data <- piWebApiService$stream$getRecorded(webId = pi.points$WebId, startTime=start, endTime=end)[[2]]
  
  # Transform list of observations into dataframe with time and value
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
  
  # Remove NAs
  all.pi.data <- na.omit(pi.data)
  
  # If there are no observations, break now by returning a NULL object
  if(is.null(all.pi.data)) return(NULL)
  
  # "getRecorded" PI function is limited to 1000 observations per iteration
  # If the number of rows in "pi.data" is equal to 1000, 
  # then there are most likely more observations to pull before the "end"
  if(nrow(pi.data)==1000) {
    
    # Loop over the data pull until the "pi.data" object has fewer than 1000 observations
    needsUpdating <- TRUE
    while(needsUpdating) {
      
      # What time was the last observation pulled?
      time.obj <- as.POSIXct(all.pi.data[nrow(all.pi.data), 1], tz=Sys.timezone())
      
      # Make the last time the new start time
      begin <- paste0(as.character(format(lubridate::with_tz(time.obj, tzone="UTC"),"%Y-%m-%d")),"T",
                      as.character(format(lubridate::with_tz(time.obj, tzone="UTC"),"%H:%M:%S")),"Z")
      
      # Pull PI tag/WebID from "begin"
      pi.data <- piWebApiService$stream$getRecorded(webId = pi.points$WebId, startTime=begin, endTime=end)[[2]]
      
      # Transform list of observations into dataframe with time and value
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
      
      # Combine previous and most recent observations
      all.pi.data <- rbind(all.pi.data, na.omit(pi.data))
      
      # Check if there are more observations to pull
      # If "pi.data" = 1000 observations, there there are most likely more
      if(nrow(pi.data) < 1000) needsUpdating <- FALSE
    }
  }
  
  # Use the last character string from the PI tag as the variable name, unless another label has been provided
  if(is.null(label)) label <- gsub("[.]", "_", make.names(tail(strsplit(tag,"[\\]")[[1]],n=1)))
  
  # If we want to save the compiled data, save to CSV
  if(save) write.csv(all.pi.data, file=paste0(label,".csv"), row.names = FALSE)
  
  # If we want to return the compiled data, return the dataframe
  if(obj.return) return(all.pi.data)
}
