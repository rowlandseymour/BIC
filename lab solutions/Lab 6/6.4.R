
# Input Data and Set up MCMC ----------------------------------------------
y <- c(4, 4, 5, 2)

p <- 0.001
y5 <- 2

n.iter <- 10000
p.store <- numeric(n.iter)
y5.store <- numeric(n.iter)


# Run MCMC Algorithm ------------------------------------------------------

for(i in 1:n.iter){

  #Update p
  p <- rbeta(1, 15 + y5 + 1, 5001 - 15 - y5)

  #Update y5

  numerator <- choose(1000, 0:5)*p^(0:5)*(1-p)^(1000 - 0:5)
  denominator <- sum(numerator)
  y5 <- sample(0:5, 1, prob = numerator/denominator)


  #Save Output
  p.store[i] <- p
  y5.store[i] <- y5
}


# Plot Posteriors ---------------------------------------------------------
p.sample = p.store[-(1:1000)] # discard first 1000 samples as burn-in
y5.sample = y5.store[-(1:1000)]
plot(p.sample, type = 'l')
hist(p.sample)
mean(p.sample)
quantile(p.sample, c(0.025, 0.975))

plot(y5.sample, type = 'l')
mean(y5.sample)
table(y5.sample)/(n.iter-1000)
