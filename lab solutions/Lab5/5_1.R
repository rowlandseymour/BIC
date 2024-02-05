#Function that evaluates Pareto loglikelihood
log.likelihood <- function(x, beta){

  log.value <- length(x)*log(beta) - (beta + 1)*sum(log(x))

  return(log.value)

}

# MCMC Sampler ------------------------------------------------------------

#Initialise Values
x <- c(1.019844, 1.043574, 1.360953, 1.049228, 1.491926, 1.192943, 1.323738, 1.262572, 2.034768, 1.451654)
n.iter <- 10000 #number of iterations
beta.current <- 2 #initial value for beta
beta.store <- numeric(n.iter) #empty vecotr to store beta at each iteration

#Run MCMC For Loop
for(i in 1:n.iter){

  #Propose prop value for beta
  beta.prop <- rnorm(1, beta.current, 0.1)

  #Compute current and prop loglikelihood
  loglike.prop     <- log.likelihood(x, beta.prop)
  loglike.current <- log.likelihood(x, beta.current)

  #Compute Log acceptance probability
  log.p.acc <- loglike.prop - loglike.current +
    dgamma(beta.prop, 1, 0.01, log = TRUE) - dgamma(beta.current, 1, 0.01, log = TRUE)

  #Accept/Reject
  u <- runif(1)
  if(log(u) < log.p.acc){
    beta.current <- beta.prop
  }

  #Store Current Value
  beta.store[i] <- beta.current


}

#Plot trace plots
plot(beta.store, type = 'l')

#Investigate posterior
hist(beta.store, freq = FALSE, main = "", xlab = expression(beta))
quantile(beta.store, c(0.025, 0.975))
