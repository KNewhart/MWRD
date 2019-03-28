

```

## Predictive model comparison
```{r echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
df.model.compare <- data.frame("Model" = "Multiple Lienar Regression", "R-squared" = summary.lm(lm.new.data)$r.squared)

df.model.compare <- rbind(df.model.compare, data.frame("Model" = "Random Forest", "R-squared" = mean(rf.new.data$rsq)))

df.model.compare <- rbind(df.model.compare, data.frame("Model" = "Generalized Addative Model", "R-squared" = summary.gam(gam.new.data)$r.sq))

knitr::kable(df.model.compare, digits = 2)
```


## Principal Component Analysis
```{r message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE}
# PCA

windowsFonts(A = windowsFont("Times New Roman"))
pca.new.data <- prcomp(new.data, scale=TRUE)

png("MWRD_Ecoli_nseconline_pca.png", units = "in", res = 200, width = 6.5, height = 6.5, family="A", pointsize = 24)

fviz_pca_var(pca.new.data, labelsize = 5,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
             # ,ggtheme = theme(text=element_text(family="A", size=16))
)
dev.off()

pca.new.data.summary <- factoextra::get_pca_var(pca.new.data)
```
![](MWRD_Ecoli_nseconline_pca.png)
**Figure .** PCA variable loading for effluent of north secondary

## Partial Least Squares
```{r message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE}
# PLS

pls.new.data <- plsr(Pre.disinf.E..coli..LOG.MPN.100mL. ~ NSEC.Aerobic.SRT + NSEC.Effluent.OP + NSEC.Effluent.NO3 + NSEC.Effluent.NO5 + NSEC.Effluent.Flow, data=new.data, scale = TRUE, validation = "CV")
summary(pls.new.data)
plot(pls.new.data)
validationplot(pls.new.data, val.type = "MSEP")


pls.reg.fit <- plsreg1(predictors = new.data[,2:8], response = new.data[,1], comps = 3, crosval = TRUE)
plot(pls.reg.fit)

png("MWRD_Ecoli_nseconline_pls.png", units = "in", res = 200, width = 2*3, height = 2*3, family="A", pointsize = 16)
par(mar=c(4,4,.5,.5))
plot(x = new.data[,1], y = pls.reg.fit$y.pred, xlim=c(7,12), ylim=c(7,12), xlab="Actal Log(N)", ylab="Predicted Log(N)")
abline(a=0,b=1,col="blue", lwd=2)
dev.off()
```
![](MWRD_Ecoli_nseconline_pls.png)
**Figure .** PLS model fit for pre-disinfection *E. coli*. Black circles represent actual observations (x-axis) plotted against the prediction (y-axis). Blue line represents perfect model fit.

# Plot
```{r echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}
# data1 <- n.paa.online[,1] 
data1 <- log(n.paa.grab[,6]) 
data2 <- nsec.online[,which(colnames(nsec.online) == "NSEC Effluent NO5")]

label1 <- colnames(data1)
label2 <- colnames(data2)

if (range(index(data1)[which(!is.na(data1))])[1] < range(index(data2)[which(!is.na(data2))])[1]) {
  r1 <- range(index(data2)[which(!is.na(data2))])[1]
} else {
  r1 <- range(index(data1)[which(!is.na(data1))])[1]
}
if (range(index(data1)[which(!is.na(data1))])[2] > range(index(data2)[which(!is.na(data2))])[2]) {
  r2 <- range(index(data2)[which(!is.na(data2))])[2]
} else {
  r2 <- range(index(data1)[which(!is.na(data1))])[2]
}

data2plot <- na.omit(data1)[paste0(r1,"/",r2)]
data2plot <- merge(data2plot, data2[paste0(r1,"/",r2)])
data2plot <- data2plot[-which(duplicated(index(data2plot))),]
data2plot <- data.frame(data2plot, row.names = as.character(index(data2plot)))
data2plot <- cbind(data2plot, as.numeric(difftime(as.POSIXct(rownames(data2plot)), as.POSIXct(rownames(data2plot)[1]),units = "days")))

par(mar=c(5.1,4.1,2.1,4.1))
plot(x = data2plot[,3], y = data2plot[,1], type = "p", pch = 20, col = "black", xaxt = "n", xlab = "", ylab = "", yaxt="n")
axis(side = 2)
mtext(side = 2, label1, line = 2.5)

par(new = TRUE)
plot(x = data2plot[,3], y = data2plot[,2], type = "p", pch = 20, col = "red", xaxt = "n", xlab = "", yaxt="n", ylab = "")
axis(side = 4, col.axis = "red")
mtext(side = 4, label2, line = 2.5, col = "red")
# x-axis
axis.ticks <- seq(0,round(data2plot[nrow(data2plot),3]), by = 10)
axis.labels <- sapply(axis.ticks, function(x) which(x > data2plot[,3]))
axis.labels <- sapply(axis.labels, function(x) x[length(x)])
axis.labels[[1]] <- 1
axis.labels <- as.numeric(unlist(axis.labels))
axis(side = 1, at = axis.ticks, labels = format(as.POSIXct(rownames(data2plot)[axis.labels]), "%m/%d"))

```



```{r}
data1 <- n.paa.online[,1][which( n.paa.online[,1] < 2.00001)]
data2 <- n.paa.grab[,12]

label1 <- colnames(data1)
label2 <- colnames(data2)

if (range(index(data1)[which(!is.na(data1))])[1] < range(index(data2)[which(!is.na(data2))])[1]) {
  r1 <- range(index(data2)[which(!is.na(data2))])[1]
} else {
  r1 <- range(index(data1)[which(!is.na(data1))])[1]
}
if (range(index(data1)[which(!is.na(data1))])[2] > range(index(data2)[which(!is.na(data2))])[2]) {
  r2 <- range(index(data2)[which(!is.na(data2))])[2]
} else {
  r2 <- range(index(data1)[which(!is.na(data1))])[2]
}

data2plot <- na.omit(data1)[paste0(r1,"/",r2)]
data2plot <- merge(data2plot, data2[paste0(r1,"/",r2)])
data2plot <- data.frame(data2plot)
data2plot <- cbind(data2plot, as.numeric(difftime(as.POSIXct(rownames(data2plot)), as.POSIXct(rownames(data2plot)[1]),units = "days")))

par(mar=c(5.1,4.1,2.1,4.1))
plot(x = data2plot[,3], y = data2plot[,1], type = "p", pch = 20, col = "black", xaxt = "n", xlab = "", ylab = "", yaxt="n")
axis(side = 2)
mtext(side = 2, label1, line = 2.5)

par(new = TRUE)
plot(x = data2plot[,3], y = data2plot[,2], type = "p", pch = 20, col = "red", xaxt = "n", xlab = "", yaxt="n", ylab = "", log = 'y')
axis(side = 4, col.axis = "red")
mtext(side = 4, label2, line = 2.5, col = "red")
# x-axis
axis.ticks <- seq(0,round(data2plot[nrow(data2plot),3]), by = 10)
axis.labels <- sapply(axis.ticks, function(x) which(x > data2plot[,3]))
axis.labels <- sapply(axis.labels, function(x) x[length(x)])
axis.labels[[1]] <- 1
axis.labels <- as.numeric(unlist(axis.labels))
axis(side = 1, at = axis.ticks, labels = format(as.POSIXct(rownames(data2plot)[axis.labels]), "%m/%d"))
```


```{r}
data1 <- n.paa.online[,7]
data2 <- n.paa.grab[,12]

label1 <- colnames(data1)
label2 <- colnames(data2)

if (range(index(data1)[which(!is.na(data1))])[1] < range(index(data2)[which(!is.na(data2))])[1]) {
  r1 <- range(index(data2)[which(!is.na(data2))])[1]
} else {
  r1 <- range(index(data1)[which(!is.na(data1))])[1]
}
if (range(index(data1)[which(!is.na(data1))])[2] > range(index(data2)[which(!is.na(data2))])[2]) {
  r2 <- range(index(data2)[which(!is.na(data2))])[2]
} else {
  r2 <- range(index(data1)[which(!is.na(data1))])[2]
}

data2plot <- na.omit(data1)[paste0(r1,"/",r2)]
data2plot <- merge(data2plot, data2[paste0(r1,"/",r2)])
data2plot <- data.frame(data2plot)
data2plot <- cbind(data2plot, as.numeric(difftime(as.POSIXct(rownames(data2plot)), as.POSIXct(rownames(data2plot)[1]),units = "days")))

par(mar=c(5.1,4.1,2.1,4.1))
plot(x = data2plot[,3], y = data2plot[,1], type = "p", pch = 20, col = "black", xaxt = "n", xlab = "", ylab = "", yaxt="n")
axis(side = 2)
mtext(side = 2, label1, line = 2.5)

par(new = TRUE)
plot(x = data2plot[,3], y = data2plot[,2], type = "p", pch = 20, col = "red", xaxt = "n", xlab = "", yaxt="n", ylab = "", log = 'y')
axis(side = 4, col.axis = "red")
mtext(side = 4, label2, line = 2.5, col = "red")
# x-axis
axis.ticks <- seq(0,round(data2plot[nrow(data2plot),3]), by = 10)
axis.labels <- sapply(axis.ticks, function(x) which(x > data2plot[,3]))
axis.labels <- sapply(axis.labels, function(x) x[length(x)])
axis.labels[[1]] <- 1
axis.labels <- as.numeric(unlist(axis.labels))
axis(side = 1, at = axis.ticks, labels = format(as.POSIXct(rownames(data2plot)[axis.labels]), "%m/%d"))
```

```{r}
## North secondary effluent lab data
nsec.eff.lab.fc24 <- as.data.frame(read_excel("North Secondary and Disinfection Process Data_20190215.xlsx", 
                                              sheet = "NSEC Eff Lab Data (FC24)", col_names = FALSE, 
                                              col_types = c("date", "numeric", "skip", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric"), skip = 3))
nsec.eff.lab.fc24 <- xts(nsec.eff.lab.fc24[,-1], order.by = nsec.eff.lab.fc24[,1])
colnames(nsec.eff.lab.fc24) <- as.vector(sapply(c("ALK","CBOD","COD","NH3-N","NO5-N","OP","TP","TIN","TKN","TN","TSS"), function(x) paste("NSE", x, "(fc24)")))
sapply(nsec.eff.lab.fc24, function(x) plot.xts(x, type = "p", pch = 20, main = colnames(x), grid.col = NA))

sapply(nsec.eff.lab.fc24, function(data1) {
  # data1 <- n.paa.online[,7]
  data2 <- n.paa.grab[,12]
  
  label1 <- colnames(data1)
  label2 <- colnames(data2)
  
  if (range(index(data1)[which(!is.na(data1))])[1] < range(index(data2)[which(!is.na(data2))])[1]) {
    r1 <- range(index(data2)[which(!is.na(data2))])[1]
  } else {
    r1 <- range(index(data1)[which(!is.na(data1))])[1]
  }
  if (range(index(data1)[which(!is.na(data1))])[2] > range(index(data2)[which(!is.na(data2))])[2]) {
    r2 <- range(index(data2)[which(!is.na(data2))])[2]
  } else {
    r2 <- range(index(data1)[which(!is.na(data1))])[2]
  }
  
  data2plot <- na.omit(data1)[paste0(r1,"/",r2)]
  data2plot <- merge(data2plot, data2[paste0(r1,"/",r2)])
  data2plot <- data.frame(data2plot)
  data2plot <- cbind(data2plot, as.numeric(difftime(as.POSIXct(rownames(data2plot)), as.POSIXct(rownames(data2plot)[1]),units = "days")))
  
  par(mar=c(5.1,4.1,2.1,4.1))
  plot(x = data2plot[,3], y = data2plot[,1], type = "p", pch = 20, col = "black", xaxt = "n", xlab = "", ylab = "", yaxt="n")
  axis(side = 2)
  mtext(side = 2, label1, line = 2.5)
  
  par(new = TRUE)
  plot(x = data2plot[,3], y = data2plot[,2], type = "p", pch = 20, col = "red", xaxt = "n", xlab = "", yaxt="n", ylab = "", log = 'y')
  axis(side = 4, col.axis = "red")
  mtext(side = 4, label2, line = 2.5, col = "red")
  # x-axis
  axis.ticks <- seq(0,round(data2plot[nrow(data2plot),3]), by = 10)
  axis.labels <- sapply(axis.ticks, function(x) which(x > data2plot[,3]))
  axis.labels <- sapply(axis.labels, function(x) x[length(x)])
  axis.labels[[1]] <- 1
  axis.labels <- as.numeric(unlist(axis.labels))
  axis(side = 1, at = axis.ticks, labels = format(as.POSIXct(rownames(data2plot)[axis.labels]), "%m/%d"))
})
```

```{r}
sapply(nsec.eff.lab.fc24, function(data1) {
  # data1 <- n.paa.online[,7]
  data2 <- n.paa.grab[,6]
  
  label1 <- colnames(data1)
  label2 <- colnames(data2)
  
  if (range(index(data1)[which(!is.na(data1))])[1] < range(index(data2)[which(!is.na(data2))])[1]) {
    r1 <- range(index(data2)[which(!is.na(data2))])[1]
  } else {
    r1 <- range(index(data1)[which(!is.na(data1))])[1]
  }
  if (range(index(data1)[which(!is.na(data1))])[2] > range(index(data2)[which(!is.na(data2))])[2]) {
    r2 <- range(index(data2)[which(!is.na(data2))])[2]
  } else {
    r2 <- range(index(data1)[which(!is.na(data1))])[2]
  }
  
  data2plot <- na.omit(data1)[paste0(r1,"/",r2)]
  data2plot <- merge(data2plot, data2[paste0(r1,"/",r2)])
  data2plot <- data.frame(data2plot)
  data2plot <- cbind(data2plot, as.numeric(difftime(as.POSIXct(rownames(data2plot)), as.POSIXct(rownames(data2plot)[1]),units = "days")))
  
  par(mar=c(5.1,4.1,2.1,4.1))
  plot(x = data2plot[,3], y = data2plot[,1], type = "p", pch = 20, col = "black", xaxt = "n", xlab = "", ylab = "", yaxt="n")
  axis(side = 2)
  mtext(side = 2, label1, line = 2.5)
  
  par(new = TRUE)
  plot(x = data2plot[,3], y = data2plot[,2], type = "p", pch = 20, col = "red", xaxt = "n", xlab = "", yaxt="n", ylab = "", log = 'y')
  axis(side = 4, col.axis = "red")
  mtext(side = 4, label2, line = 2.5, col = "red")
  # x-axis
  axis.ticks <- seq(0,round(data2plot[nrow(data2plot),3]), by = 10)
  axis.labels <- sapply(axis.ticks, function(x) which(x > data2plot[,3]))
  axis.labels <- sapply(axis.labels, function(x) x[length(x)])
  axis.labels[[1]] <- 1
  axis.labels <- as.numeric(unlist(axis.labels))
  axis(side = 1, at = axis.ticks, labels = format(as.POSIXct(rownames(data2plot)[axis.labels]), "%m/%d"))
})
```

```{r}
## North secondary influent lab
nsec.inf.lab.fc24 <- as.data.frame(read_excel("North Secondary and Disinfection Process Data_20190215.xlsx", 
                                              sheet = "NSEC Inf Lab Data (FC24)", col_names = FALSE, 
                                              col_types = c("date", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric"), skip = 3))
nsec.inf.lab.fc24 <- xts(nsec.inf.lab.fc24[,-1], order.by = nsec.inf.lab.fc24[,1])
colnames(nsec.inf.lab.fc24) <- as.vector(sapply(c("NSI ALK","NSI BOD", "NSI COD", "NSI NH3-N", "NSI NO5-N", "NSI C:N", "NSI C:P", "NSI OP", "NSI TP", "NSI TIN", "NSI TKN", "NSI TN", "NSI TSS"), function(x) paste(x, "(fc24)")))

sapply(nsec.inf.lab.fc24, function(data1) {
  # data1 <- n.paa.online[,7]
  data2 <- n.paa.grab[,6]
  
  label1 <- colnames(data1)
  label2 <- colnames(data2)
  
  if (range(index(data1)[which(!is.na(data1))])[1] < range(index(data2)[which(!is.na(data2))])[1]) {
    r1 <- range(index(data2)[which(!is.na(data2))])[1]
  } else {
    r1 <- range(index(data1)[which(!is.na(data1))])[1]
  }
  if (range(index(data1)[which(!is.na(data1))])[2] > range(index(data2)[which(!is.na(data2))])[2]) {
    r2 <- range(index(data2)[which(!is.na(data2))])[2]
  } else {
    r2 <- range(index(data1)[which(!is.na(data1))])[2]
  }
  
  data2plot <- na.omit(data1)[paste0(r1,"/",r2)]
  data2plot <- merge(data2plot, data2[paste0(r1,"/",r2)])
  data2plot <- data.frame(data2plot)
  data2plot <- cbind(data2plot, as.numeric(difftime(as.POSIXct(rownames(data2plot)), as.POSIXct(rownames(data2plot)[1]),units = "days")))
  
  par(mar=c(5.1,4.1,2.1,4.1))
  plot(x = data2plot[,3], y = data2plot[,1], type = "p", pch = 20, col = "black", xaxt = "n", xlab = "", ylab = "", yaxt="n")
  axis(side = 2)
  mtext(side = 2, label1, line = 2.5)
  
  par(new = TRUE)
  plot(x = data2plot[,3], y = data2plot[,2], type = "p", pch = 20, col = "red", xaxt = "n", xlab = "", yaxt="n", ylab = "", log = 'y')
  axis(side = 4, col.axis = "red")
  mtext(side = 4, label2, line = 2.5, col = "red")
  # x-axis
  axis.ticks <- seq(0,round(data2plot[nrow(data2plot),3]), by = 10)
  axis.labels <- sapply(axis.ticks, function(x) which(x > data2plot[,3]))
  axis.labels <- sapply(axis.labels, function(x) x[length(x)])
  axis.labels[[1]] <- 1
  axis.labels <- as.numeric(unlist(axis.labels))
  axis(side = 1, at = axis.ticks, labels = format(as.POSIXct(rownames(data2plot)[axis.labels]), "%m/%d"))
})
```

```{r}
nsec.inf.lab.grab <- as.data.frame(read_excel("North Secondary and Disinfection Process Data_20190215.xlsx",
                                              sheet = "NSEC Inf Lab Data (Grab)", col_names = FALSE,
                                              col_types = c("date", "numeric", "skip",
                                                            "skip", "skip", "skip", "skip", "date",
                                                            "numeric", "skip", "skip", "skip",
                                                            "skip", "skip", "date", "numeric",
                                                            "skip", "skip", "skip",
                                                            "skip", "skip", "date", "numeric",
                                                            "skip", "skip", "skip",
                                                            "skip", "skip", "date",
                                                            "numeric", "skip", "skip",
                                                            "skip", "skip", "skip",
                                                            "date", "numeric"), skip = 4))
colnames(nsec.inf.lab.grab) <- c("Time", "NSI COD (grab)",
                                 "Time", "NSI MCOD (grab)",
                                 "Time", "NSI NO5-N (grab)",
                                 "Time", "NSI NO5M-N (grab)",
                                 "Time", "NSI TSS (grab)",
                                 "Time", "NSI TSSM (grab)")
for (i in c(1,3,5,7,9,11)) {
  blah <- nsec.inf.lab.grab[,c(i,(i+1))]
  blah <- na.omit(blah)
  blah <- xts(blah[,2], order.by = blah[,1])
  colnames(blah) <- colnames(nsec.inf.lab.grab)[i+1]
  if (i == 1) {
    nsec.inf.lab.grab.merged <- blah
  } else {
    nsec.inf.lab.grab.merged <- merge(nsec.inf.lab.grab.merged, blah)
  }
}


sapply(nsec.inf.lab.grab.merged, function(data1) {
  # data1 <- n.paa.online[,7]
  data2 <- n.paa.grab[,6]
  
  label1 <- colnames(data1)
  label2 <- colnames(data2)
  
  if (range(index(data1)[which(!is.na(data1))])[1] < range(index(data2)[which(!is.na(data2))])[1]) {
    r1 <- range(index(data2)[which(!is.na(data2))])[1]
  } else {
    r1 <- range(index(data1)[which(!is.na(data1))])[1]
  }
  if (range(index(data1)[which(!is.na(data1))])[2] > range(index(data2)[which(!is.na(data2))])[2]) {
    r2 <- range(index(data2)[which(!is.na(data2))])[2]
  } else {
    r2 <- range(index(data1)[which(!is.na(data1))])[2]
  }
  
  data2plot <- na.omit(data1)[paste0(r1,"/",r2)]
  data2plot <- merge(data2plot, data2[paste0(r1,"/",r2)])
  data2plot <- data.frame(data2plot)
  data2plot <- cbind(data2plot, as.numeric(difftime(as.POSIXct(rownames(data2plot)), as.POSIXct(rownames(data2plot)[1]),units = "days")))
  
  par(mar=c(5.1,4.1,2.1,4.1))
  plot(x = data2plot[,3], y = data2plot[,1], type = "p", pch = 20, col = "black", xaxt = "n", xlab = "", ylab = "", yaxt="n")
  axis(side = 2)
  mtext(side = 2, label1, line = 2.5)
  
  par(new = TRUE)
  plot(x = data2plot[,3], y = data2plot[,2], type = "p", pch = 20, col = "red", xaxt = "n", xlab = "", yaxt="n", ylab = "", log = 'y')
  axis(side = 4, col.axis = "red")
  mtext(side = 4, label2, line = 2.5, col = "red")
  # x-axis
  axis.ticks <- seq(0,round(data2plot[nrow(data2plot),3]), by = 10)
  axis.labels <- sapply(axis.ticks, function(x) which(x > data2plot[,3]))
  axis.labels <- sapply(axis.labels, function(x) x[length(x)])
  axis.labels[[1]] <- 1
  axis.labels <- as.numeric(unlist(axis.labels))
  axis(side = 1, at = axis.ticks, labels = format(as.POSIXct(rownames(data2plot)[axis.labels]), "%m/%d"))
})
```


Time plots reveal few correlations between *E. coli* and process variables. TSSM in the north secondary influent and 

## North Secondary Effluent Flow Composite
```{r}
data1 <- nsec.eff.lab.fc24
data2 <- n.paa.grab[,6]

label1 <- colnames(data1)
label2 <- colnames(data2)

all.data <- merge(data2, data1)
all.data.index <- which(!is.na(all.data[,1]))
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

apply(new.data, 2, function(x) length(which(!is.na(x)))/nrow(new.data))[order(-apply(new.data, 2, function(x) length(which(!is.na(x)))/nrow(new.data)))]
new.data <- new.data[,order(-apply(new.data, 2, function(x) length(which(!is.na(x)))/nrow(new.data)))[1:10]]
new.data <- na.omit(new.data)


# pls.fit <- plsr(new.data[,1] ~ new.data[,2] + new.data[,3] + new.data[,4], scale = TRUE, validation = "CV")
# summary(pls.fit)
# plot(pls.fit)
# validationplot(pls.fit, val.type = "MSEP")
# 

# pls.reg.fit <- plsreg1(predictors = new.data[,2:4], response = new.data[,1], comps = 3, crosval = TRUE)
# plot(pls.reg.fit)
# plot(new.data[,1], pls.reg.fit$y.pred, log = 'yx')


mod_gam_nsec_eff <- gam(new.data[,1] ~ new.data[,2] + new.data[,5]  + new.data[,6] + new.data[,9])
summary(mod_gam_nsec_eff)
```

## North Secondary Influent Grab
```{r}
data1 <- nsec.inf.lab.grab.merged
data2 <- n.paa.grab[,6]

label1 <- colnames(data1)
label2 <- colnames(data2)

all.data <- merge(data2, data1)
all.data.index <- which(!is.na(all.data[,1]))
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

apply(new.data, 2, function(x) length(which(!is.na(x)))/nrow(new.data))
new.data <- new.data[,order(-apply(new.data, 2, function(x) length(which(!is.na(x)))/nrow(new.data)))[1:3]]
new.data <- na.omit(new.data)

library(pls)
pls.fit <- plsr(new.data[,1] ~ new.data[,2] + new.data[,3], scale = TRUE, validation = "CV")
summary(pls.fit)
plot(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

library(plsdepot)
pls.reg.fit <- plsreg1(predictors = new.data[,2:3], response = new.data[,1], comps = 3, crosval = TRUE)
plot(pls.reg.fit)
plot(new.data[,1], pls.reg.fit$y.pred, log = 'yx')

library(mgcv)
mod_gam_nsec_inf_grab <- gam(new.data[,1] ~ new.data[,2] + new.data[,3])
summary(mod_gam_nsec_inf_grab)
```
## North Secondary Influent Flow Composite
```{r}
data1 <- nsec.inf.lab.fc24
data2 <- n.paa.grab[,6]

label1 <- colnames(data1)
label2 <- colnames(data2)

all.data <- merge(data2, data1)
all.data.index <- which(!is.na(all.data[,1]))
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

apply(new.data, 2, function(x) length(which(!is.na(x)))/nrow(new.data))[order(-apply(new.data, 2, function(x) length(which(!is.na(x)))/nrow(new.data)))]
new.data <- new.data[,order(-apply(new.data, 2, function(x) length(which(!is.na(x)))/nrow(new.data)))[1:10]]
new.data <- na.omit(new.data)

# library(pls)
# pls.fit <- plsr(new.data[,1] ~ new.data[,2] + new.data[,3] + new.data[,4], scale = TRUE, validation = "CV")
# summary(pls.fit)
# plot(pls.fit)
# validationplot(pls.fit, val.type = "MSEP")
# 
# library(plsdepot)
# pls.reg.fit <- plsreg1(predictors = new.data[,2:4], response = new.data[,1], comps = 3, crosval = TRUE)
# plot(pls.reg.fit)
# plot(new.data[,1], pls.reg.fit$y.pred, log = 'yx')

library(mgcv)
mod_gam_nsec_inf_fc24 <- gam(new.data[,1] ~ new.data[,2] + new.data[,3] + new.data[,4] + new.data[,5]  + new.data[,6]  + new.data[,7] + new.data[,8] + new.data[,9] + new.data[,10])
summary(mod_gam_nsec_inf_fc24)
```


```{r eval=FALSE, include=FALSE}
# Import data
library(readxl)
library(xts)
## North secondary lab
nsec.ml.lab <- as.data.frame(read_excel("North Secondary and Disinfection Process Data_20190215.xlsx", 
                                        sheet = "NSEC Mixed Liquor Lab Data", 
                                        col_names = FALSE, col_types = c("date", 
                                                                         "numeric", "date", "numeric", 
                                                                         "date", "numeric"), skip = 5))
colnames(nsec.ml.lab) <- c("Time", 
                           "NS SVI", 
                           "Time", 
                           "NS TSS", 
                           "Time",
                           "NS VSS")
for (i in c(1,3,5)) {
  blah <- nsec.ml.lab[,c(i,(i+1))]
  blah <- na.omit(blah)
  blah <- xts(blah[,2], order.by = blah[,1])
  colnames(blah) <- colnames(nsec.ml.lab)[i+1]
  if (i == 1) {
    north.daily <- blah
  } else {
    north.daily <- merge(north.daily, blah)
  }
  
}
rm(blah)
# rm(nsec.ml.lab)

## North secondary influent lab
nsec.inf.lab.fc24 <- as.data.frame(read_excel("North Secondary and Disinfection Process Data_20190215.xlsx", 
                                              sheet = "NSEC Inf Lab Data (FC24)", col_names = FALSE, 
                                              col_types = c("date", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric"), skip = 3))
nsec.inf.lab.fc24 <- xts(nsec.inf.lab.fc24[,-1], order.by = nsec.inf.lab.fc24[,1])
colnames(nsec.inf.lab.fc24) <- as.vector(sapply(c("NSI ALK","NSI BOD", "NSI COD", "NSI NH3-N", "NSI NO5-N", "NSI C:N", "NSI C:P", "NSI OP", "NSI TP", "NSI TIN", "NSI TKN", "NSI TN", "NSI TSS"), function(x) paste(x, "(fc24)")))
north.daily <- merge(north.daily, nsec.inf.lab.fc24)
# rm(nsec.inf.lab.fc24)



## North secondary effluent lab data
nsec.eff.lab.fc24 <- as.data.frame(read_excel("North Secondary and Disinfection Process Data_20190215.xlsx", 
                                              sheet = "NSEC Eff Lab Data (FC24)", col_names = FALSE, 
                                              col_types = c("date", "numeric", "skip", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric"), skip = 3))
nsec.eff.lab.fc24 <- xts(nsec.eff.lab.fc24[,-1], order.by = nsec.eff.lab.fc24[,1])
colnames(nsec.eff.lab.fc24) <- as.vector(sapply(c("ALK","CBOD","COD","NH3-N","NO5-N","OP","TP","TIN","TKN","TN","TSS"), function(x) paste("NSE", x, "(fc24)")))
north.daily <- merge(north.daily, nsec.eff.lab.fc24)








# 
# ## North secondary influent grab
# nsec.inf.lab.grab <- as.data.frame(read_excel("North Secondary and Disinfection Process Data_20190215.xlsx", 
#     sheet = "NSEC Inf Lab Data (Grab)", col_names = FALSE, 
#     col_types = c("date", "numeric", "skip", 
#         "skip", "skip", "skip", "skip", "date", 
#         "numeric", "skip", "skip", "skip", 
#         "skip", "skip", "date", "numeric", 
#         "skip", "skip", "skip", 
#         "skip", "skip", "date", "numeric", 
#         "skip", "skip", "skip", 
#         "skip", "skip", "date", 
#         "numeric", "skip", "skip", 
#         "skip", "skip", "skip", 
#         "date", "numeric"), skip = 4))
# colnames(nsec.inf.lab.grab) <- c("Time", "NSI COD (grab)",
#                                  "Time", "NSI MCOD (grab)",
#                                  "Time", "NSI NO5-N (grab)",
#                                  "Time", "NSI NO5M-N (grab)",
#                                  "Time", "NSI TSS (grab)",
#                                  "Time", "NSI TSSM (grab)")
# for (i in c(1,3,5,7,9,11)) {
#     blah <- nsec.inf.lab.grab[,c(i,(i+1))]
#     blah <- na.omit(blah)
#     blah <- xts(blah[,2], order.by = blah[,1])
#     colnames(blah) <- colnames(nsec.inf.lab.grab)[i+1]
#     north.daily <- merge(north.daily, blah)
# }
# rm(blah)
# rm(nsec.inf.lab.grab)
# 
# n.paa.lab <- as.data.frame(read_excel("North Secondary and Disinfection Process Data_20190215.xlsx", 
#     sheet = "North PAA Lab Data", col_types = c("date", 
#         "numeric", "skip", "skip", 
#         "skip", "skip", "skip", "date", 
#         "numeric", "skip", "skip", 
#         "skip", "skip", "skip", 
#         "date", "numeric"), skip = 4))
# colnames(n.paa.lab) <- c("Time", "Pre-Disinfection E. coli",
#                          "Time", "Final Effluent E. coli",
#                          "Time", "Effluent PAA Residual")
# for (i in c(1,3,5)) {
#     blah <- n.paa.lab[,c(i,(i+1))]
#     blah <- na.omit(blah)
#     blah <- xts(blah[,2], order.by = blah[,1])
#     colnames(blah) <- colnames(n.paa.lab)[i+1]
#     north.daily <- merge(north.daily, blah)
# }
# rm(blah)
# rm(n.paa.lab)
# 
# ## Reduce time to days
# day1 <- range(index(north.daily))[1]
# ndays <- as.numeric(difftime(range(index(north.daily))[2], day1, units = "days"))
# for (i in 0:round(ndays-1)) {
#     day <- as.Date(day1)+i
#     day.data <- north.daily[paste0(day,"/",day),]
#     day.data.locf <- na.locf(day.data)
#     day.data.filtered <- day.data.locf[nrow(day.data.locf),]
#     index(day.data.filtered) <- lubridate::floor_date(index(day.data.filtered),"day")
#     if (i == 0) {
#         north.daily.rounded <- day.data.filtered
#     } 
#     if (i > 0) {
#         north.daily.rounded <- rbind(north.daily.rounded, day.data.filtered)
#     }
# }
```


```{r eval=FALSE, include=FALSE}
north.row.index <- which(!is.na(north.daily.rounded$Effluent.PAA.Residual))[which(!is.na(north.daily.rounded$Effluent.PAA.Residual)) %in% which(!is.na(north.daily.rounded$Log.Removal..N0.N.))]
north.col.index <- which(!is.na(last(north.daily.rounded[north.row.index,])))
remove.these.cols <- c("PAA.Dosing.Pump.Total.Flow..gpm.", "PAA.Setpoint..mg.L.", 
                       # "X.U.0394.PAA..mg.L.", 
                       "Effluent.Discharge..MGD.", "Contact.Tank.Volume..MG.", "Time.to.Upstream.Sample.Point..min.","NSI.OP..fc24.", "NSE.OP..fc24.","NSI.C.P..fc24.")
plot.this.data <- as.data.frame(north.daily.rounded)[north.row.index,north.col.index]
plot.this.data <- plot.this.data[,-which(colnames(plot.this.data) %in% remove.these.cols)]
# colnames(plot.this.data)[order(-apply(plot.this.data, 2, function(x) length(which(is.na(x)))))]
pca.results <- princomp(na.omit(plot.this.data), cor = TRUE, scores = TRUE)
# plot(pca.results)
factoextra::fviz_eig(pca.results)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  
  test <- cor.test(x,y)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  
  text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex=cex, col=2)
}
# pdf(file="north_daily_pairs.pdf",width=100,height=100)
# pairs(scale(plot.this.data), lower.panel=panel.smooth, upper.panel=panel.cor)
# dev.off()

# pdf(file="north_daily_pca.pdf",width=10,height=10)
factoextra::fviz_pca_var(pca.results,
                         col.var = "contrib", # Color by contributions to the PC
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE     # Avoid text overlapping
)
# dev.off()



# 
# library(ggfortify)
# file <-"north_daily_pca_autoplot.pdf"
# pdf(file=file,width=10,height=10)
# autoplot(princomp(na.omit(plot.this.data)), data = scale(na.omit(plot.this.data)), loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, loadings.label.vjust = 1.5, loadings.label.hjust = .5) +
#   theme(plot.background=element_blank(),
#   panel.background=element_rect(fill='transparent',color='black',size=1))
# dev.off()
# Biobase::openPDF(file)

# 
# load <- as.data.frame(abs(with(pca.results, unclass(loadings))))
# # names(load.sum)[order(-load.sum)][1:5]
# 
# std_dev <- pca.results$sdev
# pr_var <- std_dev^2
# prop_varex <- pr_var/sum(pr_var)
# plot(prop_varex[1:10])
# sum(prop_varex[1:5])
# 
# # Plot of first 5 components divided by variable
# library(ggplot2)
# data1 <- data.frame(rownames(load), load[,1], rep("PC1", nrow(load)))
# data2 <- data.frame(rownames(load), load[,2], rep("PC2", nrow(load)))
# data3 <- data.frame(rownames(load), load[,3], rep("PC3", nrow(load)))
# data4 <- data.frame(rownames(load), load[,4], rep("PC4", nrow(load)))
# data5 <- data.frame(rownames(load), load[,5], rep("PC5", nrow(load)))
# colnames(data1) <- c("Variable", "Loading", "PrincipalComponent")
# colnames(data2) <- c("Variable", "Loading", "PrincipalComponent")
# colnames(data3) <- c("Variable", "Loading", "PrincipalComponent")
# colnames(data4) <- c("Variable", "Loading", "PrincipalComponent")
# colnames(data5) <- c("Variable", "Loading", "PrincipalComponent")
# data <- rbind(data1,data2,data3,data4,data5)
# base_size <- 100
# file <- "north_daily_pca_ggplot.pdf"
# pdf(file,width = 110, height = 85)
# ggplot(data, aes(fill = Variable, y = Loading, x = PrincipalComponent)) + 
#   geom_bar( stat="identity") + 
#   theme(panel.background = element_blank(),
#         legend.text=element_text(size=base_size),
#         axis.text.x = element_text(size = base_size*1.75),
#         axis.text.y = element_text(size = base_size*1.75),
#         axis.title.y = element_text(size = base_size*1.75, 
#                        margin = margin(t = 0, r = 75*1.75, b = 0, l = 0)),
#         axis.title.x = element_blank(),
#         legend.key.height = unit(15, "line"),
#         legend.key.width = unit(15, "line")) +
#   guides(fill=guide_legend(ncol=1)) +
#   labs(fill="", y= "Component loadings")
# 
# dev.off()
# # library("Biobase")
# Biobase::openPDF(file)

```
# GAM
```{r eval=FALSE, include=FALSE}
library(mgcv)
# colnames(plot.this.data)
for (i in 1:ncol(plot.this.data)) {
  predictor <- 7
  if (i != predictor) {
    mod_gam <- gam(plot.this.data[,7] ~ s(plot.this.data[,i]))
    print(paste(colnames(plot.this.data)[i],summary(mod_gam)$dev.expl))
  }
}
# [1] "PAA.Dose..mg.L. 0.00168359943201252"
# [1] "Upstream..Residual..mg.L. 0.000606810663177951"
# [1] "X.U.0394.PAA..mg.L. 0.0305720444328727"
# [1] "Pre.Disinfection.E..coli..MPN.100.mL. 0.00688618129058839"
# [1] "Detention.Time..min. 3.08265256773682e-05"
# [1] "Log.Removal..N0.N. 0.826446782956113"
# [1] "CT..mg.L.min. 0.000699687993667349"
# [1] "CuT..mg.L.min. 0.000536581551228759"
# [1] "Ambient.Temperature 0.00818534327523203"
# [1] "NSI.BOD..fc24. 0.00148950649862333"
# [1] "NSI.NH3.N..fc24. 0.00787259527494445"
# [1] "NSI.NO5.N..fc24. 0.0946997298831912"
# [1] "NSI.C.N..fc24. 0.000403781053740269"
# [1] "NSI.TP..fc24. 0.00486940226314001"
# [1] "NSI.TIN..fc24. 0.00470051217262278"
# [1] "NSI.TKN..fc24. 0.00231804581323724"
# [1] "NSI.TSS..fc24. 0.000104961831023688"

mod_gam <- gam(plot.this.data[,7] ~ s(plot.this.data[,13]))
summary(mod_gam)
plot(mod_gam, residuals = T, shade = T, pch = 19, cex=0.25, xlab = colnames(plot.this.data)[13], ylab = colnames(plot.this.data)[7], select = 1)


for (i in 1:ncol(plot.this.data)) {
  predictor <- c(7,13)
  if (i != predictor) {
    mod_gam <- gam(plot.this.data[,7] ~ s(plot.this.data[,13]) + s(plot.this.data[,i]))
    print(paste(i,colnames(plot.this.data)[i],summary(mod_gam)$dev.expl))
  }
}

mod_gam <- gam(plot.this.data[,7] ~ s(plot.this.data[,13]) + s(plot.this.data[,15]))
summary(mod_gam)
plot(mod_gam, residuals = T, shade = T, pch = 19, cex=0.25, xlab = paste(colnames(plot.this.data)[13],colnames(plot.this.data)[15],sep=' & ') , ylab = colnames(plot.this.data)[7], select = 1)

```


```{r eval=FALSE, include=FALSE}
# 15 minute interval data
library(readxl)
library(xts)
nsec.online <- as.data.frame(read_excel("North Secondary and Disinfection Process Data_20190215.xlsx", 
                                        col_types = c("date", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric", 
                                                      "numeric", "numeric", "numeric"), 
                                        skip = 3))
nsec.online <- xts(nsec.online[,-1], order.by = nsec.online[,1])
colnames(nsec.online) <- c("NSEC Influent Flow", "NSEC Influent Temp","NSEC Influent NH3","NSEC Influent TSS","NSEC Influent COD","NSEC CaRRB-1 Centrate Flow","NSEC CaRRB-1 NH3","NSEC CaRRB-3 Centrate Flow","NSEC CaRRB-3 NH3","GTE Flow","GTE to SSEC Flow","GTE to NSEC Flow","AB-10 Influent Flow","AB-10 A-Pass Temp","AB-10 A-Pass pH","AB-10 A-Pass DO","AB-10 A-Pass NH3","AB-10 A-Pass NO3","AB-10 B-Pass DO","AB-10 C-Pass pH	AB-10","C-Pass DO","AB-10 C-Pass NH3","AB-10 C-Pass NO3","AB-10 MLSS","AB-10 MLR Flow","Quad 4 RAS Flow","Quad 4 Basins in Service","AB-10 RAS Flow","NSEC Aerobic SRT","NSEC Effluent NH3","NSEC Effluent NO3","NSEC Effluent OP","NSEC Effluent TSS","NSEC Effluent NO5","NSEC Effluent Flow")

n.paa.online <- suppressWarnings(as.data.frame(read_excel("North Secondary and Disinfection Process Data_20190215.xlsx", 
                                                          sheet = "North PAA Online Data", col_names = FALSE, 
                                                          col_types = c("date", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric", 
                                                                        "numeric", "numeric", "numeric"), 
                                                          skip = 3)))
n.paa.online <- suppressWarnings(xts(n.paa.online[,-1], order.by = n.paa.online[,1]))
colnames(n.paa.online) <- c("North PAA Upstream Residual (CL17-1)","North PAA Upstream Residual (CL17-2)","North Effluent PAA Upstream Residual","North PAA Dosing Pump Flow","North PAA Dose Setpoint","North PAA HRT","North PAA CT Target Dose","North Disinfection Flow")

north.15min <- merge(nsec.online, n.paa.online)
rm(nsec.online)
rm(n.paa.online)



north.15min.avg <- apply.daily(north.15min, colMeans)
index(north.15min.avg) <- lubridate::floor_date(index(north.15min.avg),"day")
north.daily.merged <- merge(north.15min.avg,north.daily.rounded)
```















































```{r eval=FALSE, include=FALSE}
rm(list = ls())
setwd("C:/Users/Kate Newhart/odrive/Mines/Data/MWRD")
delta <- intToUtf8(0x0394)
library(readxl)
library(xts)

# Daily data
process.data <- read_excel("Copy of PAA Process Data Clean KN.xlsx", 
                           sheet = "Process Data", skip = 1)
process.data <- process.data[-1,]
```


```{r eval=FALSE, include=FALSE}
## South process data
south.process.data <- xts(apply(process.data[,c(1:4,6:11,27)], 2, function(x) as.numeric(x)), order.by =  as.POSIXct(as.data.frame(process.data[,5])[,1], format = "%Y-%m-%d %H:%M:%S"))
colnames(south.process.data) <- c("PAA Dosing Pump Flow (gpm)", #1
                                  "PAA Dose (mg/L)", #2
                                  "PAA Setpoint (mg/L)", #3
                                  "Pre-Disinfection E. coli (MPN/100 mL)",  #4
                                  "Effluent Discharge (MGD)", #5
                                  "Contact Tank Volume (MG)", #6
                                  "Detention Time (min)", #7
                                  "Log Removal (N0/N)", #8
                                  "Effluent E. coli (MPN/100 mL)", #9
                                  "CT (mg/L*min)", #10
                                  "Ambient Temperature (F)") #11
south.daily <- south.process.data
```










# South process GAM
```{r eval=FALSE, include=FALSE}
library(mgcv)
plot.this.data <- south.process.data
colnames(south.process.data)[c(9,2,4,11)]
mod_gam_south <- gam(plot.this.data[,9] ~ s(plot.this.data[,2]) # Predict effluent E. coli from C0
                     + s(plot.this.data[,4]) # N0
                     + s(plot.this.data[,11]) #T
)
summary(mod_gam_south)
```

# North process GAM
```{r eval=FALSE, include=FALSE}
library(mgcv)
plot.this.data <- n.paa.grab
colnames(n.paa.grab)[c(12,2,4,6,7,8,13,14,15)]
mod_gam_north <- gam(plot.this.data[,12] ~ s(plot.this.data[,2]) # Predict effluent E. coli from C0
                     + s(plot.this.data[,4]) # C1
                     + s(plot.this.data[,6]) # N0
                     + s(plot.this.data[,7]) # Qe
                     + s(plot.this.data[,8]) # V
                     + s(plot.this.data[,13]) #CT
                     + s(plot.this.data[,14]) #CuT
                     + s(plot.this.data[,15]) #T
)
summary(mod_gam_north)
```

```{r eval=FALSE, include=FALSE}
# North process data - October
process.data.oct <- suppressWarnings(read_excel("Copy of PAA Process Data Clean KN.xlsx",
                                                sheet = "Oct 2 to 15, 2018", range = "A1:W170",
                                                col_types = c(rep("numeric", 12),"date", rep("numeric", 10))))
process.data.oct <- process.data.oct[-1,]
process.data.oct <- xts(process.data.oct[,-13], order.by = as.POSIXct(as.data.frame(process.data.oct[,13])[,1], format = "%Y-%m-%d %H-%M-%S"))

cols <- c( "PAA Pump Total Flow", # 1
           "Pump Flow Based PAA Dose", # 2
           "PAA @ 1 min. Sample", # 3
           "PAA @ 1/2 Basin Sampling", # 4
           "N. Eff. TSS Conc.", # 5
           "Temp. of NSEC Main Ch.", # 6
           "Detention Time") # 7
plot.this.data <- as.data.frame(process.data.oct)
plot.this.data <- plot.this.data[,which(colnames(plot.this.data) %in% cols)]

# GAM
library(mgcv)
mod_gam_all <- gam(plot.this.data[,4] ~ s(plot.this.data[,1]) + 
                     s(plot.this.data[,2]) + 
                     s(plot.this.data[,3])+ s(plot.this.data[,5])+ s(plot.this.data[,6]) + s(plot.this.data[,7]))
summary(mod_gam_all)
for (i in 1:ncol(plot.this.data)) {
  if (i < 4) {
    plot(mod_gam_all, residuals = T, shade = T, pch = 19, cex=0.25, xlab = colnames(plot.this.data)[i], ylab = colnames(plot.this.data)[4], select = i)
  }
  if (i > 4) {
    plot(mod_gam_all, residuals = T, shade = T, pch = 19, cex=0.25, xlab = colnames(plot.this.data)[i-1], ylab = colnames(plot.this.data)[4], select = (i-1))
  }
}

# obs <- plot.this.data[1,c(1,2,3,5,6,7)]
# predict(mod_gam_all, obs)
```


```{r eval=FALSE, include=FALSE}
# Add Vis data?
vis.data <- read_excel("NNE Carbovis Data2.xlsx",
                       sheet = "Inst DL Data", col_types = c("date",
                                                             "text", "numeric", "skip", "skip",
                                                             "skip", "numeric", "skip", "skip",
                                                             "skip", "numeric", "skip", "skip",
                                                             "skip", "numeric", "skip", "skip",
                                                             "skip", "numeric", "skip", "skip",
                                                             "skip", "numeric", "skip", "skip",
                                                             "skip", "skip", "numeric", "skip",
                                                             "skip", "skip", "numeric", "skip",
                                                             "skip", "skip", "skip", "numeric",
                                                             "skip", "skip", "skip", "numeric",
                                                             "skip"), skip = 6)
vis.data <- vis.data[which(vis.data[,2] == "Valid"),-2]
colnames(vis.data) <- c("Time", "CODto (mg/L)", "CODto (V)",
                        "TSS (mg/L)", "TSS (V)",
                        "UVT (%)", "UVT (V)",
                        "CODds (mg/L)", "CODds (V)",
                        "SACto (1/m)", "SACto (V)")
vis.data <- xts(vis.data[,-1], order.by = as.POSIXct(as.data.frame(vis.data[,1])[,1], format = "%Y-%m-%d %H-%M-%S"))
vis.data <- vis.data[paste0(range(index(process.data.oct))[1],"/",range(index(process.data.oct))[2])]

# Merge Vis and PAA data
all.data <- merge(xts(plot.this.data, order.by = as.POSIXct(rownames(plot.this.data), tz="UTC")), vis.data)
all.data.index <- which(!is.na(all.data[,1]))
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

# GAM
library(mgcv)
mod_gam_all <- gam(new.data[,4] ~ s(new.data[,1]) + s(new.data[,2]) + s(new.data[,3])+ s(new.data[,5])+ s(new.data[,6]) + s(new.data[,7]) + s(new.data[,16]))
summary(mod_gam_all)
for (i in 1:ncol(plot.this.data)) {
  if (i < 4) {
    plot(mod_gam_all, residuals = T, shade = T, pch = 19, cex=0.25, xlab = colnames(plot.this.data)[i], ylab = colnames(plot.this.data)[4], select = i)
  }
  if (i > 4) {
    plot(mod_gam_all, residuals = T, shade = T, pch = 19, cex=0.25, xlab = colnames(plot.this.data)[i-1], ylab = colnames(plot.this.data)[4], select = (i-1))
  }
}
# GAM does no better with vis data
```



```{r eval=FALSE, include=FALSE}
cols.2.remove <- labels(apply(north.daily.merged, 2, function(x) length(which(is.na(x))))[order(-apply(north.daily.merged, 2, function(x) length(which(is.na(x)))))])[1:28]
north.daily.merged.clean <- north.daily.merged[,-which(colnames(north.daily.merged) %in% cols.2.remove)]
north.daily.merged.clean <- na.omit(north.daily.merged.clean)
```


```{r eval=FALSE, include=FALSE}
apply(north.15min, 2, function(x) length(which(is.na(x))))[order(-apply(north.15min, 2, function(x) length(which(is.na(x)))))]
# colnames(north.15min)[order(-apply(north.15min, 2, function(x) length(which(is.na(x)))))]
remove.these.cols <- c("North.Effluent.PAA.Upstream.Residual","GTE.to.SSEC.Flow","GTE.to.NSEC.Flow","North.PAA.HRT","NSEC.CaRRB.1.NH3","Quad.4.Basins.in.Service","North.PAA.Dose.Setpoint")
plot.this.data <- north.15min
plot.this.data <- plot.this.data[,-which(colnames(plot.this.data) %in% remove.these.cols)]
plot.this.data <- na.omit(plot.this.data)

# panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
# {
#     usr <- par("usr"); on.exit(par(usr))
#     par(usr = c(0, 1, 0, 1))
#     r <- abs(cor(x, y))
#     txt <- format(c(r, 0.123456789), digits=digits)[1]
#     txt <- paste(prefix, txt, sep="")
#     if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
# 
#     test <- cor.test(x,y)
#     # borrowed from printCoefmat
#     Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
#                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
#                   symbols = c("***", "**", "*", ".", " "))
# 
#     text(0.5, 0.5, txt, cex = cex * r)
#     text(.8, .8, Signif, cex=cex, col=2)
# }
# pairs(plot.this.data, lower.panel=panel.smooth, upper.panel=panel.cor)
apply(plot.this.data, 2, function(x) length(unique(x)))[order(apply(plot.this.data, 2, function(x) length(unique(x))))]
# [order(-apply(north.15min, 2, function(x) length(which(is.na(x)))))]
pca.results <- princomp(plot.this.data, 
                        cor = TRUE,
                        scores = TRUE)
# plot(pca.results)
factoextra::fviz_eig(pca.results)
# pdf(file="north_15min_pca.pdf",width=10,height=10)
factoextra::fviz_pca_var(pca.results,
                         col.var = "contrib", # Color by contributions to the PC
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE     # Avoid text overlapping
)
# dev.off()

```

# Merge sensor and daily data
```{r eval=FALSE, include=FALSE}
# Where is the visual spectrum sensor? 
# What is the time difference between influent and effluent E. coli?
find.timestamps <- function(data1, data2) {
  # data1 = sensor data
  # data1 <- vis.data
  # data2 = daily data
  # data2 <- comb.data
  
  for(i in 1:nrow(data2)) {
    # What is the closest timestamp from the sensor data to the daily grab sample data?
    blah <- suppressWarnings(which(index(data1) <= index(data2)[i]))
    # Subtract detention time from timestamp of grab sample to match E.coli with sensor?
    # blah <- suppressWarnings(which(index(data1) <= lubridate::round_date(index(data2)[i] - data2[i,"Detention Time (min)"]*60, "1 minute")))
    if (i == 1) {
      sensor.index <- blah[length(blah)]
    } else {
      sensor.index <- c(sensor.index, blah[length(blah)])
    }
  }
  
  data3 <- xts(as.data.frame(data1[sensor.index,]), order.by = index(data2))
  data3 <- cbind(data2,data3)
  return(data3)
}
comb.data <- south.process.data[paste0(range(index(vis.data))[1],"/", range(index(vis.data))[2])]
comb.data <- find.timestamps(vis.data[,c(1,3,5,7,9)], comb.data)
```


# Pairwise plots
```{r eval=FALSE, include=FALSE}
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}

pairs(as.data.frame(vis.data[,c(1,3,5,7,9)]), lower.panel=panel.smooth, upper.panel=panel.cor)
pairs(as.data.frame(south.process.data), lower.panel=panel.smooth, upper.panel=panel.cor)
pairs(as.data.frame(comb.data[,-c(2,3,6,7,8)]), lower.panel=panel.smooth, upper.panel=panel.cor)
```

```{r eval=FALSE, include=FALSE}
comb.data.clean <- na.omit(comb.data[,-c(2,3,6,7,8)])

pca.results <- princomp(as.data.frame(comb.data.clean), cor = TRUE, scores = TRUE)
# plot(pca.results)
factoextra::fviz_eig(pca.results)
factoextra::fviz_pca_var(pca.results,
                         col.var = "contrib", # Color by contributions to the PC
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE     # Avoid text overlapping
)
```

**Table S1.** PCA variable contributions
```{r echo=FALSE}
knitr::kable(pca.new.data.summary$contrib, digits = 2)
# factoextra::fviz_eig(pca.new.data)
```

