#The Posterior Distribution
# \pi(alpha, beta \mid x) \propto \beta^(N*alpha)/Gamma(alpha)*
#            prod(x)^(-alpha -1)*e^(-beta(sum(1/x)))*
#            alpha^{a-1}e^(-b*alpha)beta^{c-1}*e^(-d*beta)
# The full conditionals are
# \pi(\alpha \mid \beta , x) \propto \beta^(N*alpha)/Gamma(alpha)*
#            prod(x)^(-alpha -1)*alpha^{a-1}e^(-b*alpha)
# \pi(\beta \mid \alpha , x) \propto\beta^(N*alpha)e^(-beta(sum(1/x)))beta^{c-1}*e^(-d*beta)
# This has a nice closed form \beta \mid \alpha , x \sim Gamma(N*\alpha + c, sum(1/x) + d)


# Function to evaluate loglikelihood --------------------------------------

log.likelihood <- function(x, alpha, beta){

  log.value <- (length(x)*alpha)*log(beta)- length(x)*lgamma(alpha) + (-alpha -1)*sum(log(x)) - beta*(sum(1/x))

  return(log.value)

}

# MCMC Sampler ------------------------------------------------------------

#Initialise Values
x <- 1/rgamma(100, 4, 6) #Generate some data
n.iter <- 100000 #number of iterations
alpha.current <- 4 #initial value for alpha
beta.current <- 6 #initial value for beta
alpha.store <- numeric(n.iter) #empty vector to store alpha at each iteration
beta.store <- numeric(n.iter) #empty vecotr to store beta at each iteration


#Run MCMC For Loop
for(i in 1:n.iter){

  #Update alpha using MHRW
  alpha.prop <- rnorm(1, alpha.current, 0.1)

  if(alpha.prop > 0){
    #Compute current and prop loglikelihood
    loglike.prop      <- log.likelihood(x, alpha.prop, beta.current)
    loglike.current   <- log.likelihood(x, alpha.current, beta.current)

    #Compute Log acceptance probability
    log.p.acc <- loglike.prop - loglike.current +
      dgamma(alpha.prop, 1, 0.01, log = TRUE) - dgamma(alpha.current, 1, 0.01, log = TRUE)


    #Accept/Reject
    u <- runif(1)
    if(log(u) < log.p.acc){
      alpha.current <- alpha.prop
    }
  }

  #Update beta using a Gibbs Sampler
  beta.current <- rgamma(1, length(x)*alpha.current + 1, sum(1/x) + 0.01)


  #Store Current Value
  alpha.store[i] <- alpha.current
  beta.store[i] <- beta.current

}

#Plot trace plots
plot(alpha.store, type = 'l')
plot(beta.store, type = 'l')

#Investigate correlation
plot(alpha.store, beta.store)
#Yes, highly correlated!
