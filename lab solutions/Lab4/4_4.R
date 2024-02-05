#It helps to plot the distribution so we can get an idea of what value c to use. 
#It looks like a u[0, 1] distribution with c >=3 will work here. I'll use c=4, but 3 is the most efficient. 
X <- seq(0, 1, 0.01)
plot(X, 3*X^2, type = 'l')


#Generate from Q (uniform)
y <- runif(10000)


# Calculate the probability density function values
f <- 3*y^2

#Generate acceptance probabilities 
k <- f/(4*1) #q(y) = 1

# Generate random samples from a uniform distribution for acceptance/rejection
u <- runif(10000)

# Accept samples based on the acceptance/rejection criterion
x <- y[u < k]

#Plot results
hist(x, freq = FALSE, main = "", breaks = 20)
lines(X, 3*X^2)