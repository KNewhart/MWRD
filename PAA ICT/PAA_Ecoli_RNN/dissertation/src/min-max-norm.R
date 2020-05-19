min.max.norm <- function(train, test=NULL) {
  if(is.vector(train)) train <- data.frame(train)
  if(is.null(test)) test <- train
  ma <- apply(train,2,max)
  mi <- apply(train,2,min)
  results <- do.call("cbind", lapply(names(ma), function(var) {
    (test[,var]-mi[var])/(ma[var]-mi[var])
  }))
  if(length(rownames(test))>0) rownames(results) <- rownames(test)
  return(results)
}