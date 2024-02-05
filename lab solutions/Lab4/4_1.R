#Integrating over [0, 1] and setting the result equal to 1, gives a = 3. 
#The CDF is 0 for x < 0, x^3 for 0 <= x <= 1 and 1 for x > 1. 
#The inverse function for the inverse transform method is u^1/3. 


#Generate random uniform samples
u <- runif(10000)

#Generate samples from required distribution
x <- u^(1/3)

#Plot results
X <- seq(0, 1, 0.01)
hist(x, freq = FALSE, main = "", breaks = 20)
lines(X, 3*X^2)
