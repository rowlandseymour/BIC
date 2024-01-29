#It helps to plot the distribution so we know that our value of c is correct
X <- seq(0, 5, 0.01)
plot(X, 2/(sqrt(2*pi))*exp(-X^2/2), type = 'l')
lines(X, 2/sqrt(2*pi)*exp(1/2)*dexp(X, 1), col = 2)


#Generate from Q (uniform)
y <- rexp(10000, 1)

# Calculate the probability density function values
f <- 2/(sqrt(2*pi))*exp(-y^2/2)

#Generate acceptance probabilities 
c <- 2/sqrt(2*pi)*exp(1/2)
k <- f/(c*dexp(y, 1)) #q(y) = 1

# Generate random samples from a uniform distribution for acceptance/rejection
u <- runif(10000)

# Accept samples based on the acceptance/rejection criterion
x <- y[u < k]

#Plot results
hist(x, freq = FALSE, main = "", breaks = 20)
lines(X, 2/(sqrt(2*pi))*exp(-X^2/2))
