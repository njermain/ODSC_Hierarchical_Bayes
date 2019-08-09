### Nate Jermain
### 8/9/19
### Hierarchical Bayesian Model 

library(R2OpenBUGS)
library(rjags)
library(coda)
library(MCMCvis)

# inspect the data
setwd('C:/Users/w10007346/Dropbox/Employment/ODSC/Hierarchical Bayes')
df <- read.csv("Bayes_dat.csv")
head(df)
length(df$Time)
df = df[-c(1)]

# Setup Model
mod = function(){
  #priors
  b0~dnorm(0,.001)
  mu.Z~dnorm(0,.001)

  sigma~dunif(0,50)
  tau<-1/(sigma*sigma)
  
  varsigma~dunif(0,50)
  tau.g<-1/(varsigma*varsigma)
  
  #likelihood
  for(i in 1:length(Time)){
    mu[i]<-b0+3000*exp(-Z[Country[i]]*Time[i])
    Price[i]~dnorm(mu[i], tau)
    Price_pred[i]~dnorm(mu[i], tau)
  }
  for(j in 1:8){
    Z[j]~dnorm(mu.Z, tau.g)
  }
}




# write model
model.file="model.txt"
write.model(mod,model.file)

# no initial values
inits<-NULL

# what parameters we want to track
params = c("tau","mu.Z", "b0", "Price_pred", "tau.g")

## hyperparameters
# number of iterations
ni = 10000
# burn in interval
nb = 1000
# thinning interval
nt = 1
# number of chains
nc = 3


# compile model
jmod = jags.model(file = model.file, data = df, n.chains = nc, inits = inits, n.adapt = 1000)

# iterate through jmod for the extent of the burn-in
update(jmod, n.iter=nb, by=1)

# draw samples from the posterior for params, given MCMC hyperparameters
post = coda.samples(jmod, params, n.iter = ni, thin = nt)

# diagnostic evaluation of posterior samples
MCMCtrace(post, params = c('tau.g','mu.Z'), pdf=F)

# objectively assess convergence with gelmans diagnostic
gelman.diag(post)

# get summary of posterior samples for two parameters
MCMCsummary(post, params = c('tau.g','mu.Z'), digits=2)

# get samples from posteriors
samples = jags.samples(jmod,c('Price_pred'), length(df$Time))

# take the mean of each group of samples
posterior_means = apply(samples$Price_pred,1,mean)

# plot posterior means versus observed values
plot(df$Price, posterior_means, ylab='Predicted Price', xlab='Observed Price', cex.axis=1.1, cex.lab=1.2)
lines(seq(1,3500),seq(1,3500), col='red', lwd=2, cex=3)




