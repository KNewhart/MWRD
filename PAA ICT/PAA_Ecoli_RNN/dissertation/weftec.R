library(xts)
sapply(list.files("src", full.names = TRUE), source)

if("ct-all-results-ls.RData" %in% list.files(path="results/")) {
  load("results/ct-all-results-ls.RData")
} else {
  load("results/paa-all-results-ls-lablag.RData")
  load("data/paa/process-data-ls-lablag.RData")
  i <- 1;j <- 1
  all.results.ls <- list()
  # 5. Merge data for testing
  year <- c("2018", "2019")
  for(i in 1:length(paa.data.ls)) {
    paa <- paa.data.ls[[i]] # 2018 or 2019
    hrt <- hrt.ls[[i]][[1]] # Use only real-time HRT
    dose <- process.data.ls[[1]]$PAA_N_FlowRate*0.15*1.16/(process.data.ls[[1]]$`North Flow to Dis`)
    flow <- process.data.ls[[1]]$`North Flow to Dis`
    t_f <- 2690.3*flow^-0.959
    data <- array(data=NA, 
                  dim=c(ncol(paa), # 2 or 4 rows (sampling location)
                        2, # 2 columns (C and t)
                        nrow(paa))) # matrices (each sampling event)
    results <- matrix(data=NA,
                      nrow=nrow(paa),ncol=5)
    for(j in 1:dim(data)[3]) {
      data[,,j] <- cbind(as.numeric(paa[j,]),as.numeric(hrt[j,]))
      if(any(data[,,j]==0)) {
        data[which(data[,,j]==0),,j] <- NA 
      }
    }
    # layout(mat=matrix(1:dim(results)[1]/4, ncol=4, byrow = TRUE))
    par(mfrow=c(2,2))
    for(j in 1:dim(results)[1]) {
      C <- data[,1,j]
      t <- data[,2,j]
      exponential.mod <- lm(log(C)~ t)
      r2 <- cor(x=na.omit(C),y=na.omit(exp(t*exponential.mod$coefficients[2]+exponential.mod$coefficients[1])))
      C0 <- dose[j]
      D <- C0-exp(as.numeric(exponential.mod$coefficients)[1])
      k <- -as.numeric(exponential.mod$coefficients)[2]
      n <- tail(which(process.data.ls[[1]][,1] <= index(paa)[j]),n=1)
      CT <- (C0-D)/k-(C0-D)/k*exp(-k*t_f[n])
      results[j,] <- c(as.numeric(exponential.mod$coefficients), C0, CT, r2)
      
    }
    results <- xts(results, order.by=index(paa))
    all.results.ls[[length(all.results.ls)+1]] <- results
  }
  save(all.results.ls,file="results/ct-all-results-ls.RData")
}

D <- all.results.ls[[2]][,1]
k <- -all.results.ls[[2]][,2]
# 
# # Build Demand Variable Selction model
# load("data/paa/process-data-ls.RData")
# all.results.ls <- list()
# 
# # for(i in 1:length(all.results.ls)) {
#   for(i in 2) {
#   process <- process.data.ls[[1]] # Instantaneous process data
#   process <- process[,-2] # remove PAA analyzer
#   # vis <- vis.data
#   # hrt <- hrt.ls[[i]][[1]] # Use only real-time HRT
#   
#   # 1. Combine paa and process data
#   merged.data <- cbind(D,
#                        xts(process[which(process[,1] %in% index(D)),-1], order.by=process[which(process[,1] %in% index(D)),1]))
#   merged.data <- merged.data[,which(apply(merged.data,2,function(x) !anyNA(x)))]
#   
#   # 2. Add HRT
#   # merged.data <- cbind(merged.data, hrt)
#   
#   # 3. Add vis
#   # vis <- na.locf(vis)
#   # vis <- xts(vis[sapply(index(merged.data), function(t) tail(which(index(vis)<t), n=1)),], order.by=index(merged.data))
#   # merged.data <- cbind(merged.data, vis)
#   
#   # 4. Remove vars that are constant
#   merged.data <- merged.data[,apply(merged.data, 2, function(x) length(unique(x))>3)]
#   
#   predict.var <- colnames(merged.data)[1]
#   
#   # Initialize loop
#   vars <- which(!(colnames(merged.data) %in% predict.var))
#   vars.key <- data.frame("Name"=colnames(merged.data)[vars],
#                          "Number"=vars)
#   combos <- combn(vars,1) # Initialize
#   previous.rmse <- 100 # Dummy
#   current.rmse <- 90 # Dummy
#   results.ls <- list() # Store all combination results
#   
#   # Loop over RMSE
#   while(previous.rmse >= current.rmse) {
#     combo.results.ls <- list()
#     # Train all combinations n times
#     for(j in 1:ncol(combos)) {
#       t1 <- Sys.time()
#       all.data <- merged.data[,c(which(colnames(merged.data) %in% predict.var), combos[,j])] # Create training data
#       predict.col <- which(colnames(all.data)==predict.var)
#       resultsANN.n <- list()
#       for(n in 1:10) {
#         resultsANN.n[[length(resultsANN.n)+1]] <- randomANN(all.data=all.data,
#                                                             predict.col=predict.col, 
#                                                             act.function="softsign", 
#                                                             n_epoch=250,
#                                                             # n_nodes=NULL, 
#                                                             scale=TRUE)
#       }
#       combo.results.ls[[length(combo.results.ls)+1]] <- resultsANN.n # n iterations of a combo for a sampling location
#       save(combo.results.ls, file="results-temp-combo-results-ls-d.RData")
#       print(paste(Sys.time(),"Combo", j, "of", ncol(combos), "completed"))
#       t2 <- Sys.time()
#       print(paste("Estimated time to combo completion:", Sys.time()+as.numeric(difftime(t2,t1, units="secs"))*(ncol(combos)-j)))
#     }
#     # Calcaulte performance of each combo
#     rsq <- unlist(lapply(combo.results.ls, function(combo) mean(unlist(lapply(combo, function(trial) trial[,1])) )))
#     rmse <- unlist(lapply(combo.results.ls, function(combo) mean(unlist(lapply(combo, function(trial) (trial[,2]-trial[,3])^2)) )^0.5))
#     
#     # Save results
#     results.ls[[length(results.ls)+1]] <- list(combos, rsq, rmse)
#     save(results.ls, file="results/temp-results-ls-d.RData")
#     # Compare to previous
#     previous.rmse <- current.rmse
#     current.rmse <- min(rmse)
#     
#     # Set new combo
#     combos <- matrix(data=rep(combos[,which(rmse==current.rmse)], ncol(combos)-1), nrow=nrow(combos))
#     combos <- rbind(combos, vars[-which(vars %in% combos)])
#   }
#   
#   all.results.ls[[length(all.results.ls)+1]] <- results.ls
#   save(all.results.ls, file="results/temp-all-results-ls-d.RData")
# }
# 
# 
# save(all.results.ls, file="results/D-var-results-ls-n.RData")








# Build Demand Variable Selction model
load("data/paa/process-data-ls.RData")
all.results.ls <- list()

# for(i in 1:length(all.results.ls)) {
for(i in 2) {
  process <- process.data.ls[[1]] # Instantaneous process data
  process <- process[,-2] # remove PAA analyzer
  # vis <- vis.data
  # hrt <- hrt.ls[[i]][[1]] # Use only real-time HRT
  
  # 1. Combine paa and process data
  merged.data <- cbind(k,
                       xts(process[which(process[,1] %in% index(k)),-1], order.by=process[which(process[,1] %in% index(k)),1]))
  merged.data <- merged.data[,which(apply(merged.data,2,function(x) !anyNA(x)))]
  
  # 2. Add HRT
  # merged.data <- cbind(merged.data, hrt)
  
  # 3. Add vis
  # vis <- na.locf(vis)
  # vis <- xts(vis[sapply(index(merged.data), function(t) tail(which(index(vis)<t), n=1)),], order.by=index(merged.data))
  # merged.data <- cbind(merged.data, vis)
  
  # 4. Remove vars that are constant
  merged.data <- merged.data[,apply(merged.data, 2, function(x) length(unique(x))>3)]
  
  predict.var <- colnames(merged.data)[1]
  
  # Initialize loop
  vars <- which(!(colnames(merged.data) %in% predict.var))
  vars.key <- data.frame("Name"=colnames(merged.data)[vars],
                         "Number"=vars)
  combos <- combn(vars,1) # Initialize
  previous.rmse <- 100 # Dummy
  current.rmse <- 90 # Dummy
  results.ls <- list() # Store all combination results
  
  # Loop over RMSE
  while(previous.rmse >= current.rmse) {
    combo.results.ls <- list()
    # Train all combinations n times
    for(j in 1:ncol(combos)) {
      t1 <- Sys.time()
      all.data <- merged.data[,c(which(colnames(merged.data) %in% predict.var), combos[,j])] # Create training data
      predict.col <- which(colnames(all.data)==predict.var)
      resultsANN.n <- list()
      for(n in 1:10) {
        resultsANN.n[[length(resultsANN.n)+1]] <- randomANN(all.data=all.data,
                                                            predict.col=predict.col, 
                                                            act.function="softsign", 
                                                            n_epoch=250,
                                                            # n_nodes=NULL, 
                                                            scale=TRUE)
      }
      combo.results.ls[[length(combo.results.ls)+1]] <- resultsANN.n # n iterations of a combo for a sampling location
      save(combo.results.ls, file="results-temp-combo-results-ls-k.RData")
      print(paste(Sys.time(),"Combo", j, "of", ncol(combos), "completed"))
      t2 <- Sys.time()
      print(paste("Estimated time to combo completion:", Sys.time()+as.numeric(difftime(t2,t1, units="secs"))*(ncol(combos)-j)))
    }
    # Calcaulte performance of each combo
    rsq <- unlist(lapply(combo.results.ls, function(combo) mean(unlist(lapply(combo, function(trial) trial[,1])) )))
    rmse <- unlist(lapply(combo.results.ls, function(combo) mean(unlist(lapply(combo, function(trial) (trial[,2]-trial[,3])^2)) )^0.5))
    
    # Save results
    results.ls[[length(results.ls)+1]] <- list(combos, rsq, rmse)
    save(results.ls, file="results/temp-results-ls-k.RData")
    # Compare to previous
    previous.rmse <- current.rmse
    current.rmse <- min(rmse)
    
    # Set new combo
    combos <- matrix(data=rep(combos[,which(rmse==current.rmse)], ncol(combos)-1), nrow=nrow(combos))
    combos <- rbind(combos, vars[-which(vars %in% combos)])
  }
  
  all.results.ls[[length(all.results.ls)+1]] <- results.ls
  save(all.results.ls, file="results/temp-all-results-ls-k.RData")
}


save(all.results.ls, file="results/k-var-results-ls-n.RData")


var.names <- colnames(merged.data)[combos]
var.df.ls <- list()
i <- 1
optimum.run <- length(all.results.ls[[i]])-1

var.df <- data.frame(Variable=NA, RMSE=NA, Label=NA)
for(j in 1:(optimum.run+1)) {
  optimum.rmse <- min(all.results.ls[[i]][[j]][[3]])
  optimum.combo <- which(all.results.ls[[i]][[j]][[3]]==optimum.rmse) # Min RMSE
  
  n <- all.results.ls[[i]][[j]][[1]][nrow(all.results.ls[[i]][[j]][[1]]),optimum.combo] # New variable that was added
  # var.df[j,1] <- var.names[n]
  var.df[j,1] <- vars.key[which(vars.key[,2]==n),1]
  # var.df[i,2] <- all.results.ls[[ecoli.i]][[1]][[3]][which(all.results.ls[[ecoli.i]][[1]][[1]]==n)]
  var.df[j,2] <- optimum.rmse
  var.df[j,3] <- i
}

print(var.df)

ct <- var.df[,2]
ct.lab <- var.df[,1]

lab.fix <- function(lab) {
  name.mat <- matrix(data=c("RAS %","Sec RAS (%)",
                            "BOD","Final BOD",
                            "TSS", "Sec Eff TSS",
                            "ASRT", "Sec ASRT", 
                            "AI_N92C", "Sec NO2",
                            "COD", "Sec Eff COD",
                            "AI_N99C", "Sec TSS",
                            "TN_FC24", "Sec Eff TN",
                            "AI_N93D", "Sec pH",
                            "TI-R3003", "Air Temperature",
                            "FY-F25", "Sec Inf Flow",
                            "FY-F225", "Sec Eff Flow",
                            "FI_T632", "GTE to SAR Flow",
                            "SVI", "Sec SVI",
                            "TI_N171", "Sec Temperature",
                            "AC_N94C", "Sec DO",
                            "HRT", "HRT",
                            "AI_N92A", "Sec NH3",
                            "TIN_FC24", "Eff TIN",
                            "FC_N231", "WAS Flow",
                            "NO5_FC24", "Eff NO3+NO2"), 
                     ncol=2, byrow=TRUE)
  for(i in 1:nrow(name.mat)) {
    name <- name.mat[i,1]
    new.name <- name.mat[i,2]
    if(length(grep(name,lab))>0) {
      lab <- new.name
      break
    }
  }
  
  return(lab)
}

ct.lab <- sapply(ct.lab, function(lab) lab.fix(lab))

# Plot RMSE as variables are added

# pdf(file="figures/var-select-ct.pdf", width=6.5, height=3)
par(mar=c(3.5,3.5,1,1))
plot(x=c(1:length(ct)), y=ct, 
     ylim=c(min(ct-.0001), max(ct)),
     pch=20, type="b", xaxt="n", ylab="", xlab="", xlim=c(0.5,length(ct)+.5))
mtext("RMSE", side=2, line=2.25)
mtext("Iteration", side=1, line=2.25)
axis(side=1, at = c(1:length(ct)), labels = c(1:length(ct)))

for(x in 1:length(ct)) {
  y <- ct[x]
  text(ct.lab[x],x=x,y=y,pos=1)
}

# dev.off()