# Function to compile all data
compile.data <- function(path) {
  require(xts)
  master.timestamps <- read.csv(list.files(path, full.names = TRUE)[1], colClasses=c("NULL", NA, "NULL"))
  master.data <- lapply(list.files(path, full.names = TRUE), read.csv, colClasses=c("NULL", NA, NA), header=TRUE)
  master.data <- lapply(master.data, function(data) {data[which(data[,1] %in% unlist(master.timestamps)),2]})
  master.data <-  do.call("cbind", master.data)
  colnames(master.data) <- unlist(lapply(list.files(path), function(x) substr(x,1,nchar(x)-4)))
  
  # Create time object
  master.timestamps <- as.POSIXct(unlist(master.timestamps), origin="1970-01-01", tz="GMT")
  attributes(master.timestamps)$tzone <- "America/Denver"
  master.zoo <- zoo(master.data, master.timestamps)
  
  # Ammend column names
  cols <- colnames(master.zoo)
  cols <- gsub("-", ".", gsub("[)]", ".", gsub("[(]", ".", gsub(" ", ".", cols))))
  colnames(master.zoo) <- cols
  
  return(master.zoo)
}