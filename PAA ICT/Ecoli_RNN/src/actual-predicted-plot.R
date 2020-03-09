actual.predicted.plot <- function(actual, predicted, label) {
  par(mar=c(3, 3, 3, 1) + 0.1)
  r <- c(min(c(actual[,2], predicted[,2])), max(c(actual[,2], predicted[,2])))
  plot(x=actual[,1], y=actual[,2], main=label, pch=20,xlab="", ylab="", ylim=c(r[1], r[2]))
  points(x=predicted[,1], y=predicted[,2], pch=20, col="purple")
  line.matrix <- matrix(data=c(actual[,1], actual[,2], predicted[,1], predicted[,2]), ncol=4)
  sapply(1:nrow(actual), function(i) lines(x=line.matrix[i,c(1,3)], y=line.matrix[i,c(2,4)]))
  legend("topleft", legend=c("Actual", "Predicted"), pch=20, col=c("black", "purple"))
}