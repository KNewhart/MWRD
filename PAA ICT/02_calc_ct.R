#######################################
# From predicted PAA concentrations, 
# we can estimate ICT. ICT /should/
# relate directly to log removal of 
# E.coli
#######################################

# This function will calculate the area under a 
# first order exponential curve given 2 or 3 sample points
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

CT <- vector()
for(i in unique(paa.wq.ecoli$sample.count)) {
  experiment <- paa.wq.ecoli[which(paa.wq.ecoli$sample.count == i),]
  if(experiment[3,"NUMERIC_RESULT"] == 0) {
    CT <- c(CT, calcCt(C1=experiment[1,"NUMERIC_RESULT"],
                       t1=experiment[1,"HRT (min)"],
                       C2=experiment[2,"NUMERIC_RESULT"],
                       t2=experiment[2,"HRT (min)"]))
  } else {
    CT <- c(CT, calcCt(C1=experiment[1,"NUMERIC_RESULT"],
                       t1=experiment[1,"HRT (min)"],
                       C2=experiment[2,"NUMERIC_RESULT"],
                       t2=experiment[2,"HRT (min)"],
                       C3=experiment[3,"NUMERIC_RESULT"],
                       t3=experiment[3,"HRT (min)"])
    )
  }
}
names(CT) <- rep("CT", length(CT))

ecoli.CT <- paa.wq.ecoli[which(!is.na(paa.wq.ecoli[,"Log Removal"])),]
ecoli.CT <- ecoli.CT[which(ecoli.CT[,"Log Removal"] > 0),]
