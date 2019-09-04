###### Setup Pi #####
# Login information
useKerberos <- TRUE
username <- "knewhart"
password <- "Lunabear2@"
validateSSL <- TRUE
debug <- TRUE
piWebApiService <- piwebapi$new("https://pivision/piwebapi", useKerberos, username, password, validateSSL, debug)


# Write PI Point
library(piwebapi)

createdPoint <- piWebApiService$point$getByPath("\\\\APPLEPI\\Test_knewhart")

timedValue1 <- PITimedValue(timestamp = "2019-04-26T13:40:54Z", value = 25)  
timedValue2 <- PITimedValue(timestamp = "2017-04-27T13:40:54Z", value = 50)  

timedValue3 <- PITimedValue(timestamp = "2017-04-26T12:40:54Z", value = 75)  
timedValue4 <- PITimedValue(timestamp = "2017-04-27T12:40:54Z", value = 100)  

t1 <- list(timedValue1, timedValue2)  
t2 <- list(timedValue3, timedValue4)  

s1 <- PIStreamValues(webId = createdPoint$WebId, items = t1);  
s2 <- PIStreamValues(webId = createdPoint$WebId, items = t2);  

values <- list(s1, s2)  

# response12 <- piWebApiService$streamSet$updateValuesAdHoc(values, "Buffer", "Insert")

response11 <- piWebApiService$streamSet$updateValuesAdHoc(values, "Buffer", "Replace")



# data.holder <- piWebApiService$data$getRecordedValues(path="\\\\applepi\\Test_knewhart", startTime = "2019-04-26T17:40:54Z", endTime = "2017-04-27T17:40:54Z")




