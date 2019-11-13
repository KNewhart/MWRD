spliceData <- function(x) {
  n.datasets <- ncol(x)/2
  data.names <- stringr::str_replace_all(colnames(x)[seq(2,ncol(x),by=2)], c(" " = "." , "-" = "" ))
  data.times <- x[,seq(1,ncol(x),by=2)]
  data.data <- x[,seq(2,ncol(x),by=2)]
  for(i in 1:n.datasets){
    rows.2.plot <- -which(is.na(data.data[i][[1]]))
    if(length(rows.2.plot) == 0){rows.2.plot <- seq(1,length(data.data[i][[1]]),by=1)}
    assign(data.names[i], xts::xts(data.data[rows.2.plot,i], order.by=data.times[i][[1]][rows.2.plot]), envir = .GlobalEnv)
  }
  
  return(sapply(data.names, function(x) get(x)))
}
mergeData <- function(list.x, sort.by = 1) {
  library(xts)
  
  all.data <- do.call(merge, list.x)
  all.data.index <- which(!is.na(all.data[,sort.by]))
  for(i in 1:(length(all.data.index)-1)) {
    row.start <- all.data.index[i]
    row.stop <- all.data.index[i+1]
    data.locf <- na.locf(all.data[(row.start+1):row.stop,])
    if (i == 1) {
      new.data <- data.frame(data.locf[nrow(data.locf),])
    }
    if (i != 1) {
      new.data <- rbind(new.data, data.frame(data.locf[nrow(data.locf),]))
    }
  }
  new.data <- na.omit(new.data)
  new.data.xts <- xts(new.data, order.by = as.POSIXct(rownames(new.data), format = "%Y-%m-%d %H:%M:%S"))
  
  return(new.data.xts)
}

# Disinfection_Process_Data <- readxl::read_excel("Disinfection Process Data_20190411_unlinked.xlsx", 
#                                                 sheet = "North PAA Lab Data", col_types = c("date", "numeric", rep("skip", 5),
#                                                                                             "date", "numeric", rep("skip", 5),
#                                                                                             "date", "numeric"))[-c(1:2),]
# Disinfection_Grab_Data <- mergeData(list.x = spliceData(x=Disinfection_Process_Data), sort.by =2)
# rm(Disinfection_Process_Data)
# 



#   reference_docx: MWRD_PAA_style.docx
# 