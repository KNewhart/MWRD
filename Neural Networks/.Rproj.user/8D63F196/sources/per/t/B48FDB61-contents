getPiData <- function(path, 
                      t,  # as POSIXct
                      i = 15, # interval in min
                      n = 1 # periods forward and back to collect data
                      ) {
  new.t <- stringr::str_c(stringr::str_sub(as.character(t),1,10), "T")
  new.t <- stringr::str_c(new.timestamps, stringr::str_sub(as.character(t),12,19),"Z")
  
  start.t <- t - n*i*60
  start.t <- stringr::str_c(stringr::str_sub(as.character(start.t),1,10), "T", stringr::str_sub(as.character(start.t),12,19),"Z")

  end.t <- t + n*i*60
  end.t <- stringr::str_c(stringr::str_sub(as.character(end.t),1,10), "T", stringr::str_sub(as.character(end.t),12,19),"Z")
  
  holder <- piWebApiService$data$getInterpolatedValues(path=path, startTime = start.t, endTime = end.t, interval = paste0(i,"m"))[,1:2]
  return(holder)
}




