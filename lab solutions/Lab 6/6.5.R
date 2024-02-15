
# Input Data and Set Up MCMC ----------------------------------------------
x <- c(33,  17, 218,   3,  39,   3,  43,  14,  20)


n.iter <- 10000
lamdba <- 1
x10 <- 100

lambda.store <- numeric(n.iter)
x10.store <- numeric(n.iter)


# Run MCMC ----------------------------------------------------------------

for(i in 1:n.iter){
  
  #Update lambda
  lambda <- rgamma(1, 11, sum(x) + x10 + 0.01)
  
  #Update x_10
  x10 <- sample(50:200, 1, prob = dexp(50:200, lambda)/(exp(-50*lambda) - exp(-200*lambda)))
  
  #Store values
  lambda.store[i] <- lambda
  x10.store[i] <- x10
  
}

#Plot Posteriors
hist(lambda.store)
mean(lambda.store)
quantile(lambda.store, c(0.025, 0.975))

hist(x10.store)
mean(x10.store)
ds(x10.store)
