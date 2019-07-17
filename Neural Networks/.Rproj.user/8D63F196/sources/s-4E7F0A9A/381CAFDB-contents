# Write PI Point
library(piwebapi)
newPoint <- PIPoint(NULL, NULL, "SINUSOIDR", NULL, "12 Hour Sine Wave", "classic", "Float32", NULL, NULL, NULL, NULL, NULL)  
response10 <- piWebApiService$dataServer$createPoint("s0TJVKOA0Ws0KihcA8rM1GogUElGSVRORVNTLVNSVjI", newPoint)  


timedValue1 <- PITimedValue(timestamp = "2017-04-26T17:40:54Z", value = 30)  
timedValue2 <- PITimedValue(timestamp = "2017-04-27T17:40:54Z", value = 31)  
timedValue3 <- PITimedValue(timestamp = "2017-04-26T17:40:54Z", value = 32)  
timedValue4 <- PITimedValue(timestamp = "2017-04-27T17:40:54Z", value = 33)  
t1 <- list(timedValue1, timedValue2)  
t2 <- list(timedValue3, timedValue4)  
s1 <- PIStreamValues(webId = webIds[1], items = t1);  
s2 <- PIStreamValues(webId = webIds[2], items = t2);  
values <- list(s1, s2)  
response11 <- piWebApiService$streamSet$updateValuesAdHoc(values, "BufferIfPossible", "Replace");  
  
createdPoint <- piWebApiService$point$getByPath("\\\\PIFITNESS-SRV2\\SINUSOIDR")  
updatePoint <- PIPoint()  
updatePoint$Descriptor <- "12 Hour Sine Wave for R"  
response12 <- piWebApiService$point$update(createdPoint$WebId, updatePoint)
