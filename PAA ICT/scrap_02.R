
# par(mfrow=c(round(length(unique(data$sample.count))/4),4))
# pdf(file="plots/PAA_1storder_plots.pdf", width=8.5, height=11)
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
# dev.off()

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
