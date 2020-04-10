# Install JAGS executible
# Install R2JAGS package
install.packages("R2jags",dependencies=TRUE,repos="http://cran.us.r-project.org")
install.packages("runjags",dependencies=TRUE,repos="http://cran.us.r-project.org")
install.packages("MCMCpack",dependencies=TRUE,repos="http://cran.us.r-project.org")

# Bayesian Belief networks

library(R2jags)
library(coda)

#Initial values
list("taubeta"=0.01,"taugamma"=0.01,"tau.lrv"=0.01,"taubias"=0.01,"beta0"=0)


#Import data
data<- read.csv("data.csv")

#Starting values
inits.mlp=function(){
  list("mu.mubeta"=0,
       "taubeta"=0.01,
       "taugamma"=0.01,
       "tau.lrv"=0.01,
       "mubias"=0,
       "taubias"=0.01,
       "beta0"=0)
}

#Input data
x=as.matrix(data[,c(1,2,4)])
y=data$ct


#Number of neurons in hidden layer
k=3

#Number of input variables
p=3

#Input parameters for the model
datalist <- list(
  k=k,
  p=p,
  n=dim(data)[1],
  x=x,
  y=y,
  min.tu=min(data$turbidity),
  max.tu=max(data$turbidity),
  min.ph=min(data$ph),
  max.ph=max(data$ph),
  min.lrv=min(data$lrv),
  max.lrv=max(data$lrv)
  
)

jags.params = c("gamma","beta","beta0","tau.y","taubeta","taugamma","taubias") # The parameters to be monitored

#Run model, "model.txt" is the model which is saved as a separate file
# Have to create model.txt first...
jagsfit.p <-jags(data=datalist, 
                 parameters.to.save=jags.params,
                 inits=inits.mlp,
                 # n.iter=2.2E6, 
                 model.file = "model.txt",
                 # n.chains=1,
                 # n.burnin = 2E5,
                 # n.thin=1000
                 )

#Visualise traceplots                
traceplot(jagsfit.p)

#Show summary of the results
print(jagsfit.p)


#Geweke diagnostics
jagsfit <- as.mcmc(jagsfit.p)
autocorr.diag(jagsfit)
gewe=geweke.diag(jagsfit)
gewe2=sapply(gewe, "[[", "z")

pnorm(abs(gewe2),lower.tail=FALSE)*2

#Plot autocorrelation plots
autocorr.plot(jagsfit)



