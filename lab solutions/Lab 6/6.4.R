
# Input Data and Set up MCMC ----------------------------------------------
y <- c(4, 4, 5, 2)

p <- 0.001
y5 <- 3

n.iter <- 10000
p.store <- numeric(n.iter)
y5.store <- numeric(n.iter)


# Run MCMC Algorithm ------------------------------------------------------

for(i in 1:n.iter){

  #Update p
  p <- rbeta(1, 15 + y5 + 1, 1001*5 - 15 - y5)

  #Update y5

  numerator <- choose(1000, 0:5)*p^(0:5)*(1-p)^(1000 - 0:5)
  denominator <- sum(numerator)
  y5 <- sample(0:5, 1, prob = numerator/denominator)


  #Save Output
  p.store[i] <- p
  y5.store[i] <- y5
}


# Plot Posteriors ---------------------------------------------------------
plot(p.store, type = 'l')
hist(p.store)
mean(p.store)
quantile(p.store, c(0.025, 0.975))

plot(y5.store, type = 'l')
mean(y5.store)
table(y5.store)/n.iter
