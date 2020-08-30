############################################
## SP18_STAT 6570_Final Project
## Author: Shuang Liu
## pink salmon data
############################################

# set directory to where the current script is saved
# note: this line of command requires the package "rstudioapi"
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(rjags)

data<-read.csv('pinkdata.csv')
data[['Y']]<-log(data$R/data$S)
data[['asd']]<-data$AlongShore_Distance

# lable even and odd species
# 0 (even), 1(odd)

for(i in 1:length(data[['t']])){
  if((data[['t']][i] %% 2) == 0) {
    data[['spe']][i]<-0
  } else {
    data[['spe']][i]<-1
  }
}

myvars = c("Y","asd","t","spe")
new.data<-data[myvars]
new.data = new.data[complete.cases(new.data),]

### attach Log(R/S) from one year ago, from the same site

nrow = dim(new.data)[1]
Y.t.1 = numeric(length = nrow)
for (i in 1:nrow){
  asd.index = new.data[['asd']][i] # get along shore distance of the current stock
  t.index = new.data[['t']][i]-1 # get previous year number
  t = new.data$t[new.data$asd == asd.index] # get years available for the current stock
  # evaluate if previous year does not exist
  if (t.index < min(t)){
    Y.t.1[i] = NA} 
  else if (t.index %in% t == FALSE){
    Y.t.1[i] = NA} 
  else {
    Y.t.1[i] = new.data$Y[new.data$asd == asd.index & new.data$t == t.index] # grab exist data
  }
}

new.data[['Y.t.1']] = Y.t.1


### attach Log(R/S) from two years ago, from the same site

nrow = dim(new.data)[1]
Y.t.2 = numeric(length = nrow)
for (i in 1:nrow){
  asd.index = new.data[['asd']][i]
  t.index = new.data[['t']][i]-2
  t = new.data$t[new.data$asd == asd.index]
  if (t.index < min(t)){
    Y.t.2[i] = NA} 
  else if (t.index %in% t == FALSE){
    Y.t.2[i] = NA} 
  else {
    Y.t.2[i] = new.data$Y[new.data$asd == asd.index & new.data$t == t.index]
  }
}

new.data[['Y.t.2']] = Y.t.2

### attach Log(R/S) from nearby sites

nrow = dim(new.data)[1]
sorted.asd = sort(unique(new.data$asd))
range.index = seq(1,length(sorted.asd),by=1)

### attach data from the next site ( north west )

Y.s.w1 = numeric(length = nrow)
for(i in 1:nrow){
  t.index = new.data[['t']][i] # get current year
  asd = new.data$asd[new.data$t == t.index] # get available stocks for the current year
  current.asd = new.data[['asd']][i] # get current stock along shore distance
  index = match(current.asd,sorted.asd)+1 # get the rank of the next stock
  asd.index = sorted.asd[index] # get the along shore distance of the next stock
  # evaluate if the rank of the next stock exist in the available stocks for the year
  if (index %in% range.index == FALSE){
    Y.s.w1[i] = NA}
  else if (asd.index %in% asd == FALSE){
    Y.s.w1[i] = NA}
  else {
    Y.s.w1[i] = new.data$Y[new.data$asd == asd.index & new.data$t == t.index]
  }
}


### attach data from the next site ( south east )

Y.s.e1 = numeric(length = nrow)
for(i in 1:nrow){
  t.index = new.data[['t']][i]
  asd = new.data$asd[new.data$t == t.index]
  current.asd = new.data[['asd']][i]
  index = match(current.asd,sorted.asd)-1
  asd.index = sorted.asd[index]
  
  if (index %in% range.index == FALSE){
    Y.s.e1[i] = NA}
  else if (asd.index %in% asd == FALSE){
    Y.s.e1[i] = NA}
  else {
    Y.s.e1[i] = new.data$Y[new.data$asd == asd.index & new.data$t == t.index]
  }
}

new.data[['Y.s.w1']] = Y.s.w1
new.data[['Y.s.e1']] = Y.s.e1

### attach log(R/S) from far-away sites.
### let's go 10 sites away from the current site.

# 10 site away north west

Y.s.w10 = numeric(length = nrow)
for(i in 1:nrow){
  t.index = new.data[['t']][i]
  asd = new.data$asd[new.data$t == t.index]
  current.asd = new.data[['asd']][i]
  index = match(current.asd,sorted.asd)+10
  asd.index = sorted.asd[index]
  
  if (index %in% range.index == FALSE){
    Y.s.w10[i] = NA}
  else if (asd.index %in% asd == FALSE){
    Y.s.w10[i]=NA}
  else {
    Y.s.w10[i] = new.data$Y[new.data$asd == asd.index & new.data$t == t.index]
  }
  
}


# 10 site away south east

Y.s.e10 = numeric(length = nrow)
for(i in 1:nrow){
  t.index = new.data[['t']][i]
  asd = new.data$asd[new.data$t == t.index]
  current.asd = new.data[['asd']][i]
  index = match(current.asd,sorted.asd)-10
  asd.index = sorted.asd[index]
  
  if (index %in% range.index == FALSE){
    Y.s.e10[i] = NA}
  else if (asd.index %in% asd == FALSE){
    Y.s.e10[i]=NA}
  else {
    Y.s.e10[i] = new.data$Y[new.data$asd == asd.index & new.data$t == t.index]
  }
  
}

new.data[['Y.s.w10']] = Y.s.w10
new.data[['Y.s.e10']] = Y.s.e10

# remove rows with NA values
new.data = new.data[complete.cases(new.data),]


#############################
##### initial model: M1 #####
#############################

N.salmon = dim(new.data)[1] # number of observations
P.y = 8 # number of beta parameters
  
# Create a datalist
dataList <- list(
  "y" = new.data$Y,
  "spe" = new.data$spe,
  "y.t.1" = new.data$Y.t.1,
  "y.t.2" = new.data$Y.t.2,
  "y.s.w1" = new.data$Y.s.w1,
  "y.s.e1" = new.data$Y.s.e1,
  "y.s.w10" = new.data$Y.s.w10,
  "y.s.e10" = new.data$Y.s.e10,
  "N.salmon" = N.salmon,
  "P.y" = P.y
)

# List of parameters to be monitored  
parameters <- c(
  "beta.y",
  "sigma2"
)

# Set initial values
initsValues <- list(
  "beta.y" = rep(0, P.y),
  "sigma2"= 1
)

# JAGS Set-up
adaptSteps <- 10000              #number of steps to "tune" the samplers
burnInSteps <- 10000             #number of steps to "burn-in" the samplers
nChains <- 2                    #number of chains to run
numSavedSteps <- 10000           #total number of steps in chains to save
thinSteps <- 1                  #number of steps to "thin" (1=keep every step)
nIter <- ceiling((numSavedSteps*thinSteps )/nChains) 	#steps per chain

# Create, initialize, and adapt the model
jagsModel1 <- jags.model("model-1.txt", 
                        data=dataList, 
                        inits=initsValues, 
                        n.chains=nChains, 
                        n.adapt=adaptSteps)

# Burn-in the algorithm
cat( "Burning in the MCMC chain...\n")
update(jagsModel1, n.iter=burnInSteps)

# Run MCMC algorithm
cat("Sampling final MCMC chain...\n" )
codaSamples <- coda.samples(jagsModel1, 
                            variable.names=parameters, 
                            n.iter=nIter, 
                            thin=thinSteps)

summary(codaSamples)
plot(codaSamples)

mcmcChain = as.matrix(codaSamples)
boxplot(mcmcChain,names = c("intercept","species","t-1","t-2","w1","e1","w10","e10","sigma^2"))

dic1 = dic.samples(jagsModel1, nIter)

# plot(codaSamples[[1]][,9],col = "black",main = "sigma^2",xlab ="",ylab="")
# 
# for (j in 1:8){
#   plot(codaSamples[[1]][,j],col = "black",main = paste("beta",j),xlab ="",ylab="")
# }

####################################################
##### M2: remove t-2, west.10, east.10 from M1 #####
####################################################

N.salmon = dim(new.data)[1]
P.y = 5

# Create a datalist
dataList <- list(
  "y" = new.data$Y,
  "spe" = new.data$spe,
  "y.t.1" = new.data$Y.t.1,
  "y.s.w1" = new.data$Y.s.w1,
  "y.s.e1" = new.data$Y.s.e1,
  "N.salmon" = N.salmon,
  "P.y" = P.y
)

# List of parameters to be monitored  
parameters <- c(
  "beta.y",
  "sigma2"
)

# Set initial values
initsValues <- list(
  "beta.y" = rep(0, P.y),
  "sigma2"= 1
)

# JAGS Set-up
adaptSteps <- 10000              #number of steps to "tune" the samplers
burnInSteps <- 10000             #number of steps to "burn-in" the samplers
nChains <- 2                    #number of chains to run
numSavedSteps <- 10000           #total number of steps in chains to save
thinSteps <- 1                  #number of steps to "thin" (1=keep every step)
nIter <- ceiling((numSavedSteps*thinSteps )/nChains) 	#steps per chain

# Create, initialize, and adapt the model
jagsModel2 <- jags.model("model-2.txt", 
                        data=dataList, 
                        inits=initsValues, 
                        n.chains=nChains, 
                        n.adapt=adaptSteps)

# Burn-in the algorithm
cat( "Burning in the MCMC chain...\n")
update(jagsModel2, n.iter=burnInSteps)

# Run MCMC algorithm
cat("Sampling final MCMC chain...\n" )
codaSamples <- coda.samples(jagsModel2, 
                            variable.names=parameters, 
                            n.iter=nIter, 
                            thin=thinSteps)

summary(codaSamples)
plot(codaSamples)

mcmcChain = as.matrix(codaSamples)
boxplot(mcmcChain,names = c("intercept","species","t-1","w1","e1","sigma^2"))

dic2 = dic.samples(jagsModel2, nIter)

# plot(codaSamples[[1]][,6],col = "black",main = "sigma^2",xlab ="",ylab="")
# 
# for (j in 1:5){
#   plot(codaSamples[[1]][,j],col = "black",main = paste("beta",j),xlab ="",ylab="")
# }

#################################
### M3: remove east.1 from M2 ###
#################################

N.salmon = dim(new.data)[1]
P.y = 4

# Create a datalist
dataList <- list(
  "y" = new.data$Y,
  "spe" = new.data$spe,
  "y.t.1" = new.data$Y.t.1,
  "y.s.w1" = new.data$Y.s.w1,
  "N.salmon" = N.salmon,
  "P.y" = P.y
)

# List of parameters to be monitored  
parameters <- c(
  "beta.y",
  "sigma2"
)

# Set initial values
initsValues <- list(
  "beta.y" = rep(0, P.y),
  "sigma2"= 1
)

# JAGS Set-up
adaptSteps <- 10000              #number of steps to "tune" the samplers
burnInSteps <- 10000             #number of steps to "burn-in" the samplers
nChains <- 2                    #number of chains to run
numSavedSteps <- 10000           #total number of steps in chains to save
thinSteps <- 1                  #number of steps to "thin" (1=keep every step)
nIter <- ceiling((numSavedSteps*thinSteps )/nChains) 	#steps per chain

# Create, initialize, and adapt the model
jagsModel3 <- jags.model("model-3.txt", 
                         data=dataList, 
                         inits=initsValues, 
                         n.chains=nChains, 
                         n.adapt=adaptSteps)

# Burn-in the algorithm
cat( "Burning in the MCMC chain...\n")
update(jagsModel3, n.iter=burnInSteps)

# Run MCMC algorithm
cat("Sampling final MCMC chain...\n" )
codaSamples <- coda.samples(jagsModel3, 
                            variable.names=parameters, 
                            n.iter=nIter, 
                            thin=thinSteps)

summary(codaSamples)
plot(codaSamples)

mcmcChain = as.matrix(codaSamples)
boxplot(mcmcChain,names = c("intercept","species","t-1","w1","sigma^2"))

dic3 = dic.samples(jagsModel3, nIter)


# plot(codaSamples[[1]][,5],col = "black",main = "sigma^2",xlab ="",ylab="")
# 
# for (j in 1:4){
#   plot(codaSamples[[1]][,j],col = "black",main = paste("beta",j),xlab ="",ylab="")
# }


##################################################
### M4: recover east.1, remove species from M3 ###
##################################################

N.salmon = dim(new.data)[1]
P.y = 4

# Create a datalist
dataList <- list(
  "y" = new.data$Y,
  "y.t.1" = new.data$Y.t.1,
  "y.s.w1" = new.data$Y.s.w1,
  "y.s.e1" = new.data$Y.s.e1,
  "N.salmon" = N.salmon,
  "P.y" = P.y
)

# List of parameters to be monitored  
parameters <- c(
  "beta.y",
  "sigma2"
)

# Set initial values
initsValues <- list(
  "beta.y" = rep(0, P.y),
  "sigma2"= 1
)

# JAGS Set-up
adaptSteps <- 10000              #number of steps to "tune" the samplers
burnInSteps <- 10000             #number of steps to "burn-in" the samplers
nChains <- 2                    #number of chains to run
numSavedSteps <- 10000           #total number of steps in chains to save
thinSteps <- 1                  #number of steps to "thin" (1=keep every step)
nIter <- ceiling((numSavedSteps*thinSteps )/nChains) 	#steps per chain

# Create, initialize, and adapt the model
jagsModel4 <- jags.model("model-4.txt", 
                         data=dataList, 
                         inits=initsValues, 
                         n.chains=nChains, 
                         n.adapt=adaptSteps)

# Burn-in the algorithm
cat( "Burning in the MCMC chain...\n")
update(jagsModel4, n.iter=burnInSteps)

# Run MCMC algorithm
cat("Sampling final MCMC chain...\n" )
codaSamples <- coda.samples(jagsModel4, 
                            variable.names=parameters, 
                            n.iter=nIter, 
                            thin=thinSteps)

summary(codaSamples)
plot(codaSamples)

mcmcChain = as.matrix(codaSamples)
boxplot(mcmcChain,names = c("intercept","t-1","w1","e1","sigma^2"))

dic4 = dic.samples(jagsModel4, nIter)

# plot(codaSamples[[1]][,5],col = "black",main = "sigma^2",xlab ="",ylab="")
# 
# for (j in 1:4){
#   plot(codaSamples[[1]][,j],col = "black",main = paste("beta",j),xlab ="",ylab="")
# }

###############################################
# DIC for model comparison
###############################################

dic1
dic2
dic3
dic4