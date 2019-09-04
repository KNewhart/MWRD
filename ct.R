calcCt <- function(C1, # D = initial decay (C1=C0-D)
                   t1,
                   C2, # C2 = C at time t
                   t2, # t = total time to integrate (min)
                   C3 = NULL, 
                   t3 = NULL
) {
  # SO I know the initial dosing concentration and I know the concentraiton down some exponential curve
  # SO I need to integrate in R...
  # k <- # Fit from C1 & C2 predictions 
  # C1 <- C0-D
  if(is.null(C3)) {
    C <- c(C1, C2)
    t <- c(t1, t2)
  } else {
    C <- c(C1,C2,C3)
    t <- c(t1,t2,t3)
  }
  exponential.model <- lm(log(C) ~ t)
  k <- -1*exponential.model$coefficients[2]
  Ct <- (C1-C1*exp(-k*t2))/k # Ct under single exponential curve
  return(Ct)
}

