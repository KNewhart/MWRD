import2018 <- function() {
  library(xts)
  library(readxl)
  oct.paa <- read_excel("data/paa/PAA PROFILE DATA_08-12-18.xlsx", 
                                sheet = "Oct 2 to 15, 2018", range = "A1:V170")[-1,]
  n.datetime <- which(colnames(oct.paa) == "Date and Time")
  oct.paa.index <- oct.paa[,n.datetime]
  oct.paa.index <- lubridate::with_tz(as.POSIXct(as.character(oct.paa.index[[1]]), tz="America/Denver"), "UTC")
  oct.paa <- sapply(oct.paa[,-n.datetime], function(x) as.numeric(x))
  colnames(oct.paa) <- stringr::str_replace_all(colnames(oct.paa), c(" " = "." , "-" = "" ))
  oct.paa <- xts(oct.paa, order.by = oct.paa.index)
  oct.paa <- oct.paa[,4:5]
  colnames(oct.paa) <- c("PAA.2018.1", "PAA.2018.2")
  
  # flow <- all.data$N..Basin.Outfall
  # hrt.1 <- 135.11*flow^-.959
  # hrt.half <- 1551.2*flow^-.971
  # all.data <- cbind(all.data, hrt.1, hrt.half)
  # colnames(all.data)[(ncol(all.data)-1):ncol(all.data)] <- c("HRT 1", "HRT half")
  # 
  return(oct.paa)
}