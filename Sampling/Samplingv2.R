###### Compile PAA #####
{
library(xts)
library(xlsx)
# Load raw data
PAA.PROFILE.DATA <- read.xlsx("data/PAA PROFILE DATA_08-08-19.xlsx", sheetIndex = 1)

##### PAA #####
# Subset PAA data
PAA.PROFILE.DATA <- PAA.PROFILE.DATA[which(PAA.PROFILE.DATA$ANALYSIS_CODE == "PAAR"),]

# Remove erroneous data
PAA.PROFILE.DATA <- PAA.PROFILE.DATA[which(PAA.PROFILE.DATA$COMBINATION_RESULT != "Scratched"),]
PAA.PROFILE.DATA <- PAA.PROFILE.DATA[which(!is.na(PAA.PROFILE.DATA$NUMERIC_RESULT)),]

# Fix timestamps
date.time <- strptime(paste(as.character(PAA.PROFILE.DATA$COLLECTION_DATE), as.character(PAA.PROFILE.DATA$COLLECTION_TIME)), format="%Y-%m-%d %H:%M")

# Clean data objects
data <- data.frame(date.time, stringsAsFactors = FALSE)
data <- cbind(data, as.data.frame(PAA.PROFILE.DATA$COMMON_NAME, stringsAsFactors = FALSE))
data <- cbind(data, as.data.frame(as.numeric(as.character(PAA.PROFILE.DATA$NUMERIC_RESULT)),
              stringsAsFactors = FALSE))
colnames(data) <- c("date.time", "COMMON_NAME", "NUMERIC_RESULT")
data <- data[order(data[,1]),]

# Load Flow and setpoint Data 
{
  # Install and load piwebapi package from Github
  # install.packages("devtools")
  # library(devtools)
  # install_github("rbechalany/PI-Web-API-Client-R")
  library(piwebapi)
  
  # Login information
  useKerberos <- TRUE
  username <- "knewhart"
  password <- "Lunabear2@"
  validateSSL <- TRUE
  debug <- TRUE
  piWebApiService <- piwebapi$new("https://pivision/piwebapi", useKerberos, username, password, validateSSL, debug)
  pi.times <- matrix(NA,nrow=length(date.time),ncol=1)
  for(i in 1:length(date.time)) {
    time.obj <- date.time[i]
    time.obj$hour <- time.obj$hour + 6
    pi.times[i,1] <- paste0(as.character(format(time.obj,"%Y-%m-%d")),"T",
                            as.character(format(time.obj,"%H:%M:%S")),"Z")
  }
  pi.tags <- matrix(c("DIS North Flow", "\\\\applepi\\PAA_North_Plant_Flow",
                      "PAA Setpoint", "\\\\applepi\\PAA_N_Target_Dose",
                      "DIS PAA N Upstream Residual", "\\\\applepi\\AI_K826"), ncol=2, byrow=TRUE)
  for(tag in 1:nrow(pi.tags)) {
    pi.points <- piWebApiService$point$getByPath(path=as.character(pi.tags[tag,2]))
    data.holder <- piWebApiService$data$stream$getInterpolatedAtTimes(webId = pi.points$WebId, 
                                                                      time = c(pi.times[,1]))[[2]]
    data.holder <- do.call("rbind", lapply(data.holder, function(x) c(x$Timestamp, x$Value)))
    colnames(data.holder) <- c("Datetime", make.names(pi.tags[tag,1]))
    if(tag==1) all.data <- data.holder
    if(tag>1) {
      all.data <- cbind(all.data, data.holder[,2])
      colnames(all.data)[ncol(all.data)] <- make.names(pi.tags[tag,1])
    }
  }
}

# Merge PI data and PAA data
data2 <- cbind(data, as.data.frame(all.data[,2:ncol(all.data)], stringsAsFactors = FALSE))

# Calculate hrt
hrt <- matrix(data=NA, nrow=nrow(data2), ncol = 1)
hrt[grep("NPAA1M", data2$COMMON_NAME)] <- 122.2683*as.numeric(as.vector(data2[c(grep("NPAA1M", data2$COMMON_NAME)),"DIS.North.Flow"]))^(-0.9312)
hrt[grep("NPAA10M", data2$COMMON_NAME)] <- 1044.6*as.numeric(as.vector(data2[c(grep("NPAA10M", data2$COMMON_NAME)),"DIS.North.Flow"]))^(-0.956)
hrt[grep("NPAA20M", data2$COMMON_NAME)] <- 1909.8*as.numeric(as.vector(data2[c(grep("NPAA20M", data2$COMMON_NAME)),"DIS.North.Flow"]))^(-0.958)
hrt[grep("NPAA30M", data2$COMMON_NAME)] <- 2775*as.numeric(as.vector(data2[c(grep("NPAA30M", data2$COMMON_NAME)),"DIS.North.Flow"]))^(-0.959)

data3 <- cbind(data2, as.data.frame(hrt, stringsAsFactors = FALSE))
colnames(data3)[ncol(data3)] <- "HRT (min)"
data3 <- na.omit(data3)

# Label sampling campaigns
sample.count <- vector()
for(i in 2:nrow(data3)){
  if(i == 2) {
    last.row <- 1
    sampling.campaign <- 1
  }
  
  if(difftime(data3[,"date.time"][i],data3[,"date.time"][i-1], units = "mins") > 60) {
    if(i == nrow(data3)) {
      sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:i))))
    } else {
      sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:(i-1)))))
      last.row <- i
      sampling.campaign <- sampling.campaign + 1
    }
  } else {
    if(i == nrow(data3)) {
      sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:i))))
    }
  }
}

data4 <- cbind(data3, as.data.frame(sample.count, stringsAsFactors=FALSE))
data.to.save <- data4
colnames(data.to.save) <- c("Datetime", "Sample Location", "PAA (mg/L)", "Flow (MGD)",
                            "PAA Setpoint (mg/L)", "Chemscan PAA (mg/L)", "HRT (min)", "Sampling event")
# write.csv(data.to.save, file="data/PAA PROFILE DATA_08-08-19_PAA.csv", row.names = FALSE)
}

###### Compile other components #####
{
##### E.coli #####
PAA.PROFILE.DATA <- read.xlsx("data/PAA PROFILE DATA_08-08-19.xlsx", sheetIndex = 1)
# Subset PAA data
PAA.PROFILE.DATA.ecoli <- PAA.PROFILE.DATA[which(PAA.PROFILE.DATA$ANALYSIS_CODE == "ECIDX"),]
# Remove erroneous data
PAA.PROFILE.DATA.ecoli <- PAA.PROFILE.DATA.ecoli[which(PAA.PROFILE.DATA.ecoli$COMBINATION_RESULT != "Scratched"),]
PAA.PROFILE.DATA.ecoli <- PAA.PROFILE.DATA.ecoli[which(PAA.PROFILE.DATA.ecoli$COMBINATION_RESULT != "PENDING"),]
PAA.PROFILE.DATA.ecoli <- PAA.PROFILE.DATA.ecoli[which(!is.na(PAA.PROFILE.DATA.ecoli$NUMERIC_RESULT)),]
PAA.PROFILE.DATA.ecoli <- PAA.PROFILE.DATA.ecoli[-which(substr(PAA.PROFILE.DATA.ecoli$LOCATION_DESCRIPTION,1,9) == "RWH South"),]

# Fix timestamps
date.time <- strptime(paste(as.character(PAA.PROFILE.DATA.ecoli$COLLECTION_DATE), as.character(PAA.PROFILE.DATA.ecoli$COLLECTION_TIME)), format="%Y-%m-%d %H:%M")

# Clean data objects
data <- data.frame(date.time, stringsAsFactors = FALSE)
data <- cbind(data, as.data.frame(PAA.PROFILE.DATA.ecoli$COMMON_NAME, stringsAsFactors = FALSE))
data <- cbind(data, as.data.frame(as.numeric(as.character(PAA.PROFILE.DATA.ecoli$NUMERIC_RESULT)),
                                  stringsAsFactors = FALSE))
colnames(data) <- c("date.time", "COMMON_NAME", "NUMERIC_RESULT")
data <- data[order(data[,1]),]

# Load Flow and setpoint Data 
{
  # Install and load piwebapi package from Github
  # install.packages("devtools")
  # library(devtools)
  # install_github("rbechalany/PI-Web-API-Client-R")
  library(piwebapi)
  
  # Login information
  useKerberos <- TRUE
  username <- "knewhart"
  password <- "Lunabear2@"
  validateSSL <- TRUE
  debug <- TRUE
  piWebApiService <- piwebapi$new("https://pivision/piwebapi", useKerberos, username, password, validateSSL, debug)
  pi.times <- matrix(NA,nrow=length(date.time),ncol=1)
  for(i in 1:length(date.time)) {
    time.obj <- date.time[i]
    time.obj$hour <- time.obj$hour + 6
    pi.times[i,1] <- paste0(as.character(format(time.obj,"%Y-%m-%d")),"T",
                            as.character(format(time.obj,"%H:%M:%S")),"Z")
  }
  pi.tags <- matrix(c("DIS North Flow", "\\\\applepi\\PAA_North_Plant_Flow",
                      "PAA Setpoint", "\\\\applepi\\PAA_N_Target_Dose",
                      "DIS PAA N Upstream Residual", "\\\\applepi\\AI_K826"), ncol=2, byrow=TRUE)
  for(tag in 1:nrow(pi.tags)) {
    pi.points <- piWebApiService$point$getByPath(path=as.character(pi.tags[tag,2]))
    data.holder <- piWebApiService$data$stream$getInterpolatedAtTimes(webId = pi.points$WebId, 
                                                                      time = c(pi.times[,1]))[[2]]
    data.holder <- do.call("rbind", lapply(data.holder, function(x) c(x$Timestamp, x$Value)))
    colnames(data.holder) <- c("Datetime", make.names(pi.tags[tag,1]))
    if(tag==1) all.data <- data.holder
    if(tag>1) {
      all.data <- cbind(all.data, data.holder[,2])
      colnames(all.data)[ncol(all.data)] <- make.names(pi.tags[tag,1])
    }
  }
}

# Merge PI data and PAA data
data2 <- cbind(data, as.data.frame(all.data[,2:ncol(all.data)], stringsAsFactors = FALSE))

# Calculate hrt
hrt <- matrix(data=NA, nrow=nrow(data2), ncol = 1)
hrt[grep("NPAA1M", data2$COMMON_NAME)] <- 122.2683*as.numeric(as.vector(data2[c(grep("NPAA1M", data2$COMMON_NAME)),"DIS.North.Flow"]))^(-0.9312)
hrt[grep("NPAA10M", data2$COMMON_NAME)] <- 1044.6*as.numeric(as.vector(data2[c(grep("NPAA10M", data2$COMMON_NAME)),"DIS.North.Flow"]))^(-0.956)
hrt[grep("NPAA20M", data2$COMMON_NAME)] <- 1909.8*as.numeric(as.vector(data2[c(grep("NPAA20M", data2$COMMON_NAME)),"DIS.North.Flow"]))^(-0.958)
hrt[grep("NPAA30M", data2$COMMON_NAME)] <- 2775*as.numeric(as.vector(data2[c(grep("NPAA30M", data2$COMMON_NAME)),"DIS.North.Flow"]))^(-0.959)

data3 <- cbind(data2, as.data.frame(hrt, stringsAsFactors = FALSE))
colnames(data3)[ncol(data3)] <- "HRT (min)"


# Label sampling campaigns
sample.count <- vector()
for(i in 2:nrow(data3)){
  if(i == 2) {
    last.row <- 1
    sampling.campaign <- 1
  }
  
  if(difftime(data3[,"date.time"][i],data3[,"date.time"][i-1], units = "mins") > 60) {
    if(i == nrow(data3)) {
      sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:i))))
    } else {
      sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:(i-1)))))
      last.row <- i
      sampling.campaign <- sampling.campaign + 1
    }
  } else {
    if(i == nrow(data3)) {
      sample.count <- c(sample.count, rep(sampling.campaign, length(c(last.row:i))))
    }
  }
}

data3 <- cbind(data3, as.data.frame(sample.count, stringsAsFactors = FALSE))
sampling.count <- sapply(unique(sample.count), function(x) length(which(sample.count == x)))
data4 <- data3[(sample.count %in% which(sampling.count > 1)),]
# Re number
unique.samples <- unique(which(sampling.count > 1))
for(i in 1:nrow(data4)) {
  data4[i,ncol(data4)] <- which(unique.samples == as.numeric(data4[i,ncol(data4)]))
}
data4[which(is.na(data4$`HRT (min)`)),"HRT (min)"] <- 0

# data4 <- cbind(data3, as.data.frame(sample.count, stringsAsFactors=FALSE))
write.csv(data4, file="data/PAA PROFILE DATA_08-08-19_Ecoli.csv")



##### Metals #####
PAA.PROFILE.DATA <- read.xlsx("data/PAA PROFILE DATA_08-08-19.xlsx", sheetIndex = 1)
# Subset data
PAA.PROFILE.DATA.icp <- PAA.PROFILE.DATA[which(PAA.PROFILE.DATA$ANALYSIS_CODE == "$ICPT1"),]
data.fe <- PAA.PROFILE.DATA.icp[which(substr(PAA.PROFILE.DATA.icp$ANALYTE_NAME,1,4) == "Iron"),]
data.k <- PAA.PROFILE.DATA.icp[which(substr(PAA.PROFILE.DATA.icp$ANALYTE_NAME,1,4) == "Pota"),]
data.ca <- PAA.PROFILE.DATA.icp[which(substr(PAA.PROFILE.DATA.icp$ANALYTE_NAME,1,4) == "Calc"),]
data.na <- PAA.PROFILE.DATA.icp[which(substr(PAA.PROFILE.DATA.icp$ANALYTE_NAME,1,4) == "Sodi"),]
data.mg <- PAA.PROFILE.DATA.icp[which(substr(PAA.PROFILE.DATA.icp$ANALYTE_NAME,1,4) == "Magn"),]
data <- list(data.fe, data.k, data.ca, data.na, data.mg)
data <- lapply(data, function(x) {
  x <- x[which(x$COMBINATION_RESULT != "Scratched"),]
  x <- x[which(x$COMBINATION_RESULT != "PENDING"),]
  x <- x[which(!is.na(x$NUMERIC_RESULT)),]
  date.time <- strptime(paste(as.character(x$COLLECTION_DATE), as.character(x$COLLECTION_TIME)), format="%Y-%m-%d %H:%M")
  return(xts(x$NUMERIC_RESULT, order.by=date.time))
})
data <- do.call(cbind,data)
colnames(data) <- c("Fe", "K", "Ca", "Na", "Mg")


write.csv(data, file="data/PAA PROFILE DATA_08-08-19_Metals.csv", row.names = index(data))

PAA.PROFILE.DATA.cod <- PAA.PROFILE.DATA[which(PAA.PROFILE.DATA$ANALYSIS_CODE == "CODLL"),]
data <- data.frame(strptime(paste(as.character(PAA.PROFILE.DATA.cod$COLLECTION_DATE), as.character(PAA.PROFILE.DATA.cod$COLLECTION_TIME)), format="%Y-%m-%d %H:%M"))
data <- cbind(data, data.frame(PAA.PROFILE.DATA.cod$NUMERIC_RESULT))
write.csv(data, file="data/PAA PROFILE DATA_08-08-19_COD.csv")

PAA.PROFILE.DATA.tss <- PAA.PROFILE.DATA[which(PAA.PROFILE.DATA$ANALYSIS_CODE == "TSS"),]
data <- data.frame(strptime(paste(as.character(PAA.PROFILE.DATA.tss$COLLECTION_DATE), as.character(PAA.PROFILE.DATA.tss$COLLECTION_TIME)), format="%Y-%m-%d %H:%M"))
data <- cbind(data, data.frame(PAA.PROFILE.DATA.tss$NUMERIC_RESULT))
write.csv(data, file="data/PAA PROFILE DATA_08-08-19_TSS.csv")
}

##### Calculate k & D #####
{
data <- data.to.save
exp.mod.vals <- matrix(data=NA, nrow=nrow(data), ncol=2)
mod.results <- matrix(data=NA, nrow=length(unique(data[,"Sampling event"])), ncol=3)
# Fit curve
for(i in unique(data[,"Sampling event"])) {
  s <- which(data[,"Sampling event"] %in% i)
  yy <- log(as.numeric(as.vector(data[s,"PAA (mg/L)"])))
  xx <- as.numeric(as.vector(data[s,"HRT (min)"]))
  if(any(is.infinite(yy))) {
    xx <- xx[!is.infinite(yy)]
    yy <- yy[!is.infinite(yy)]
  }
  mod <- lm(yy~xx)
  yy.predict <- predict(mod)
  k <- as.numeric(coef(mod)[2]*-1)
  C0 <- exp(as.numeric(coef(mod)[1]))
  D <- as.numeric(as.vector(data[s,"PAA Setpoint (mg/L)"])) - C0
  exp.mod.vals[s,1] <- D
  exp.mod.vals[s,2] <- k
  mod.results[i,1] <- D[1]
  mod.results[i,2] <- k[1]
  mod.results[i,3] <- as.POSIXct(data[s,"Datetime"])[1]
}



}

##### If k is constant #####
avg.k <- mean(mod.results[,2])


##### PLOTS AND MODELS #####
data <- data4
par(mfrow=c(round(length(unique(data$sample.count))/4),4))
pdf(file="plots/PAA_1storder_plots.pdf", width=8.5, height=11)
par(mfrow=c(4,3), oma=c(2,2,2,2))
exp.mod.vals <- matrix(data=NA, nrow=nrow(data), ncol=2)
mod.results <- matrix(data=NA, nrow=length(unique(data$sample.count)), ncol=3)
# Fit curve
for(i in unique(data$sample.count)) {
  yy <- log(as.numeric(as.vector(data[(data$sample.count %in% i),"NUMERIC_RESULT"])))
  xx <- as.numeric(as.vector(data[(data$sample.count %in% i),"hrt"]))
  if(any(is.infinite(yy))) {
    xx <- xx[!is.infinite(yy)]
    yy <- yy[!is.infinite(yy)]
  }
  mod <- lm(yy~xx)
  yy.predict <- predict(mod)
  k <- as.numeric(coef(mod)[2]*-1)
  C0 <- exp(as.numeric(coef(mod)[1]))
  D <- as.numeric(as.vector(data[(data$sample.count %in% i),"PAA.Setpoint"])) - C0
  exp.mod.vals[which(data$sample.count %in% i),1] <- D
  exp.mod.vals[which(data$sample.count %in% i),2] <- k
  mod.results[i,1] <- D[1]
  mod.results[i,2] <- k[1]
  mod.results[i,3] <- as.POSIXct(rownames(data[(data$sample.count %in% i),]))[1]
    
  plot(y=as.numeric(as.vector(data[(data$sample.count %in% i),"NUMERIC_RESULT"])),
       x=as.numeric(as.vector(data[(data$sample.count %in% i),"hrt"])),
       ylab="PAA Concentration", 
       xlab="HRT (min)",
       main=paste("Trial",i),
       ylim=c(0,1.44),
       xlim=c(0,101),
       pch=20)
  new.data <- data.frame(x=seq(min(na.omit(as.numeric(as.vector(data[(data$sample.count %in% i),"hrt"])))), max(na.omit(as.numeric(as.vector(data[(data$sample.count %in% i),"hrt"])))), length.out=100))
  new.data <- cbind(new.data, exp(-k*new.data+log(C0)))
  points(x=new.data[,1],y=new.data[,2], type="l", lty=2)
  labels <- list(
    paste("R-sq",round(cor(yy[1:length(yy.predict)],yy.predict)^2,4)),
    paste("D",round(D[1],4)),
    paste("k", round(k,4)))
  # mtext(labels[[1]], side=1, line=-3)
  mtext(labels[[1]], side=3, line=0.25, cex=0.75)
  text(x=90,y=1.45, paste(labels[[2]],"\n",labels[[3]]), pos=1)
}
dev.off()

# Boxplot of D & k
pdf(file="plots/PAA_1storder_boxplots.pdf", width=8.5, height=11)
par(mfrow=c(1,2), mar=c(1,2,3,1), oma=c(12,2,12,2))
boxplot(mod.results[,1])
        # , main="Instantaneous Demand (D)")
mtext(side=3, font=2, line = 1.5,"Instantaneous Demand (D)")
mtext(side=3, line=0.33, paste("Mean D =",round(mean(mod.results[,1]), 2)))
points(x=1,y=mean(mod.results[,1]), pch=17)
# text(x=1.5,y=min(mod.results[,1]), paste("Mean D =",round(mean(mod.results[,1]), 2)),pos=2)

boxplot(mod.results[,2])
        # , main= "1st order Decay (k)")
mtext(side=3, font=2, line = 1.5,"1st order Decay (k)")
mtext(side=3, line=0.33, paste("Mean k =",round(mean(mod.results[,2]), 2)))
points(x=1,y=mean(mod.results[,2]), pch=17)
# text(x=1.5,y=min(mod.results[,2]), paste("Mean k =",round(mean(mod.results[,2]), 2)),pos=2)
dev.off()


# Plot D & k vs time
pdf(file="plots/PAA_1storder_parameter_timeseries.pdf", width=8.5, height=11)
par(mfrow=c(1,1))
mod.results.df <- as.data.frame(mod.results)
mod.results.xts <- xts(mod.results.df[,1:2], order.by=as.POSIXct(mod.results.df[,3], origin = lubridate::origin))
plot(as.zoo(mod.results.xts[,1:2]), plot.type="multiple", type="p", main="1st order model parameters for individual sampling campaigns",
     ylab=c("D","k"),xlab="", oma = c(3, 0, 3, 0),mar=c(1,4,0,3), pch=20)
dev.off()



#### Diurnal trend
yy.D <- mod.results.df[,1]
yy.k <- mod.results.df[,2]
xx.time <- as.numeric(difftime(as.POSIXct(format(index(mod.results.xts), "%H:%M:%S"), format="%H:%M:%S"),
                               as.POSIXct(paste(Sys.Date(), "00:00:00 MDT")))) # Time in hours
pdf(file="plots/PAA_1storder_parameter_hour.pdf", width=8.5, height=11)
par(mfrow=c(2,1),mar=c(4,4,1,1), oma=c(0,0,2,0))
plot(y=yy.D, x=xx.time, pch=20, xlab="Hour of Day", ylab="Demand (D)")
mtext(side=3,font=2, line=1, "1st order model parameters for individual sampling campaigns")
plot(y=yy.k, x=xx.time, pch=20, xlab="Hour of Day", ylab="Decay (k)")
dev.off()



xx.time.rad <- xx.time/24*(2*pi) # Time in radians
diurnal.1 <- cos(xx.time.rad)
diurnal.2 <- sin(xx.time.rad)
diurnal.3 <- cos(2*xx.time.rad)
diurnal.4 <- sin(2*xx.time.rad)

diurnal.train=lm(yy.k~diurnal.1+diurnal.2+diurnal.3+diurnal.4, na.action=na.exclude)
Rsq.diurnal=summary(diurnal.train)$r.squared
points(y=predict(diurnal.train), x=xx.time, pch=20, col="blue")


xx.test <- seq(0,2*pi,0.1)
yy.k.predicted <- diurnal.train$coef[1]+
  diurnal.train$coef[2]*cos(xx.test)+
  diurnal.train$coef[3]*sin(xx.test)+
  diurnal.train$coef[4]*cos(2*xx.test)+
  diurnal.train$coef[5]*sin(2*xx.test)
yy.k.predicted[which(yy.k.predicted < min(predict(diurnal.train)))] <- min(predict(diurnal.train))
plot(xx.test, yy.k.predicted, pch=20, col="blue")
points(y=yy.k, x=xx.time.rad, pch=20)


### Fit Chemscan to data
chemscan.fit <- merge(mod.results.xts, 
                      as.numeric(as.vector(data[which(as.character(date.time) %in% as.character(index(mod.results.xts))),"DIS.PAA.N.Upstream.Residual"])),
                      as.numeric(as.vector(data[which(as.character(date.time) %in% as.character(index(mod.results.xts))),"PAA.Setpoint"])))
chemscan.fit <- chemscan.fit["2019-07-13/2019-07-21"]
colnames(chemscan.fit) <- c("D", "k", "Chemscan", "Setpoint")
chemscan.hrt <- (log(chemscan.fit$Chemscan)-log(chemscan.fit$Setpoint-chemscan.fit$D))/chemscan.fit$k*-1
write.csv(chemscan.hrt, row.names=index(chemscan.hrt), file="data/chemscan_hrt.csv")
plot(as.zoo(chemscan.fit$Chemscan))






# Save data
PAA.PROFILE.DATA <- xts(PAA.PROFILE.DATA, order.by = as.POSIXct(PAA.PROFILE.DATA$date.time, format="%m/%d%/%Y %H:%M"), origin = lubridate::origin)



plotting.data <- as.zoo(PAA.PROFILE.DATA)
xx <- index(plotting.data)
yy <- plotting.data$NUMERIC_RESULT
library(RColorBrewer)

plot(xx, as.numeric(yy), pch=20,
     col=brewer.pal(unique(sampling.campaign),"Set1")[as.numeric(plotting.data[,ncol(plotting.data)])])
# Add labels
# text(xx,as.numeric(yy),labels=xx, cex=.5, pos=2)

