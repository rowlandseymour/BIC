#The likelihood function is
# \pi(p \mid x) \propto p^5(1-p)^5(0.8p)^3(1-0.8p)^7
#Removing constants and combining terms gives
# \pi(p \mid x) \propto p^8(1-p)^5(1-0.8p)^7, and
# log\pi(p \mid x) \propto 8log(p) + 5log(1-p) + 7log(1-0.8p)


# Function to evaluate loglikelihood --------------------------------------

log.likelihood <- function(x, p){

  log.value <- 8*log(p) + 5*log(1-p) + 7*log(1-0.8*p)

  return(log.value)

}

# MCMC Sampler ------------------------------------------------------------

#Initialise Values
x <- c(1.019844, 1.043574, 1.360953, 1.049228, 1.491926, 1.192943, 1.323738, 1.262572, 2.034768, 1.451654)
n.iter <- 10000 #number of iterations
p.current <- 0.5 #initial value for p
p.store <- numeric(n.iter) #empty vecotr to store p at each iteration

#Run MCMC For Loop
for(i in 1:n.iter){

  #Propose prop value for p
  p.prop <- rnorm(1, p.current, 0.1)

  if(p.prop > 0 & p.prop < 1){
  #Compute current and prop loglikelihood
  loglike.prop     <- log.likelihood(x, p.prop)
  loglike.current <- log.likelihood(x, p.current)

  #Compute Log acceptance probability
  log.p.acc <- loglike.prop - loglike.current #No prior ratio as it is uniform

  #Accept/Reject
  u <- runif(1)
  if(log(u) < log.p.acc){
    p.current <- p.prop
  }
  }

  #Store Current Value
  p.store[i] <- p.current


}

#Plot trace plots
plot(p.store, type = 'l')

#Investigate posterior
hist(p.store, freq = FALSE, main = "", xlab = expression(p))
mean(p.store)
quantile(p.store, c(0.025, 0.975))
