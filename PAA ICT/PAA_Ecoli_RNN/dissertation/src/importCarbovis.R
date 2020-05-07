importCarbovis <- function() {
  vis.data <- read.csv("data/paa/NNE Carbovis Data 2018-2019.csv", stringsAsFactors = FALSE)
  vis.data.ls <- list()
  for(i in seq(1,ncol(vis.data),by=8)) {
    valid <- vis.data[,i+1]
    valid <- which(valid=="Valid")
    data <- vis.data[valid,c(i,i+6,i+4)] # Using voltage, not calibrated data
    

    dates <- as.POSIXct(data[,1], format = "%m/%d/%Y %H:%M", tz="Etc/GMT+6")

    dates <- lubridate::with_tz(dates, tzone="UTC")

    
    keep <- intersect(which(!is.na(dates)), which(!duplicated(dates)))
    result <- xts(data[keep,2], order.by = dates[keep])
    # result <- result[paste0(range(paa[,1]),collapse="/")]
    colnames(result) <- data[1,3]
    
    vis.data.ls[[length(vis.data.ls)+1]] <- result
  }
  vis.data <- do.call("cbind",vis.data.ls)
  return(vis.data)
}