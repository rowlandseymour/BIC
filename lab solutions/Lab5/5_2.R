
# MCMC Sampler ------------------------------------------------------------

#Initialise Values
y <- c(20, 16, 20, 17, 18, 19, 19, 18, 21, 20, 19, 22, 23, 19, 20, 19, 21, 20, 25, 15)
n.iter <- 10000 #number of iterations
p.current <- 0.5 #initial value for p
p.store <- numeric(n.iter) #empty vecotr to store p at each iteration

#Run MCMC For Loop
for(i in 1:n.iter){

  #Propose prop value for p
  p.prop <- rnorm(1, p.current, 0.1)

  if(p.prop > 0 & p.prop < 1){ #Automatically reject if p is outside of [0, 1]
  #Compute current and prop loglikelihood
  loglike.prop    <- sum(dbinom(y, 25, p.prop, log = TRUE))
  loglike.current <- sum(dbinom(y, 25, p.current, log = TRUE))

  #Compute Log acceptance probability
  log.p.acc <- loglike.prop - loglike.current +
    dnorm(p.prop, 0.5, 0.1, log = TRUE) - dnorm(p.current, 0.5, 0.1, log = TRUE)

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
quantile(p.store, c(0.025, 0.975))


#Plot Prior on Top
x <- seq(0, 1, 0.01)
prior.density <- dnorm(x, 0.5, 0.1)
lines(x, prior.density, type = 'l')

