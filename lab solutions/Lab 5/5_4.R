lambda.store <- numeric(n.iter)
gamma.store <- numeric(n.iter)

lambda.current <- 5
gamma.current <- 1

for(i in 1:n.iter){

  lambda.current <- rgamma(1, 11, 95 + gamma.current)
  gamma.current  <- rexp(1, lambda.current + 0.01)


  lambda.store[i] <- lambda.current
  gamma.store[i] <- gamma.current
}


# Plot Trace Plots --------------------------------------------------------
plot(gamma.store, type = 'l')
plot(lambda.store, type = 'l')


# Plot Posterior Distibrutions --------------------------------------------
hist(gamma.store, main = "", freq = FALSE, xlab = expression(gamma))
median(gamma.store)
quantile(gamma.store, c(0.025, 0.975))

hist(lambda.store, main = "", freq = FALSE, xlab = expression(lambda))
median(lambda.store)
quantile(lambda.store, c(0.025, 0.975))



