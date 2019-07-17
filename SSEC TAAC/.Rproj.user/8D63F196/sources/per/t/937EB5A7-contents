# PI-to-R compiler
# Kate Newhart
# May 8, 2019
# See https://github.com/rbechalany/PI-Web-API-Client-R for more details

# Install and load piwebapi package from Github (or contact OSIsoft)
install.packages("devtools")
library(devtools)
install_github("rbechalany/PI-Web-API-Client-R")
library(piwebapi)

# Login information
useKerberos <- TRUE
username <- "CHANGE"
password <- "CHANGE"
validateSSL <- TRUE
debug <- TRUE
piWebApiService <- piwebapi$new("https://pivision/piwebapi", useKerberos, username, password, validateSSL, debug)




# Basic - this example function pulls the previous 30 days of data
var1 <- piWebApiService$data$getRecordedValues(path="pi:\\\\applepi\\AI_K826", startTime = "y-30d", endTime = "t")[,1:2]

# Function: Fix timestamps - this function creates an xts object from the piWebApiService function above
fix.timestamps <- function(pi.data) {
  ch.times <- pi.data[,2]
  ch.times <- sub("T", " ", ch.times)
  ch.times <- sub("Z", " ", ch.times)
  times <- as.POSIXct(ch.times)
  return(xts::xts(pi.data[,1], order.by = times))
}
var1 <- fix.timestamps(var1)




# Pro - this example creates variables from names and tags stored in a matrix
pi.tags <- matrix(c("DIS PAA N Upstream Residual", "pi:\\\\applepi\\AI_K826"), ncol = 2)
assign(make.names(pi.tags[1,1]), piWebApiService$data$getRecordedValues(path=pi.tags[1,2], startTime = "y-30d", endTime = "t")[,1:2])
assign(make.names(pi.tags[1,1]), fix.timestamps(get(make.names(pi.tags[1,1]))))

# Function: Pull more than 1000 observations of a variable
# The piwebapi package will not pull more than 1000 observations from the start time. 
# This wrapper will continue to pull data in batches of 1000until the entire window has been covered
pi.tags <- matrix(c("AB10 NH3 C" , "pi:\\\\applepi\\AI_N109B",
                    "AB10 NO3 C", "pi:\\\\applepi\\AI_N109A",
                    "AB10 Temp", "pi:\\\\applepi\\TI_N100"), ncol=2, byrow=TRUE)

## If times are Date or POSIXct objects, use these commands to transform them
## into strings that can be read by PI
# start <- paste0(substr(as.character(range(index(object))[1]),1,10), "T00:00:00Z")
# end <- paste0(substr(as.character(range(index(object))[1]),1,10), "T00:00:00Z")
## Otherwise, you can just use a character string
start <- paste0("2018-07-12", "T00:00:00Z")
start <- paste0("2019-07-12", "T00:00:00Z")

for(i in 1:nrow(pi.tags)) {
  holder <- piWebApiService$data$getRecordedValues(path=pi.tags[i,2], startTime = start, endTime = end)[,1:2]
  holder <- fix.timestamps(holder)
  keeper <- holder
  
  while(nrow(holder) == 1000) { # Then there are more datapoints to pull
    new.start <- range(index(holder))[2]
    new.start <- paste0(substr(as.character(new.start),1,10), "T00:00:00Z")
    holder <- fix.timestamps(piWebApiService$data$getRecordedValues(path=pi.tags[i,2], startTime = new.start, endTime = end)[,1:2])
    keeper <- rbind(keeper, holder)
  }
  if (nrow(keeper) != nrow(holder)) keeper <- rbind(keeper, holder)
  
  assign(make.names(pi.tags[i,1]), keeper)
}