#The inverse function is F^-1(x) = x^\theta for x \in [0, 1]. 


# Inverse Transform Function ----------------------------------------------

#This function take in a vector of U[0, 1] numbers and a value for theta
#and returns samples from the distributions 1/theta x^{(1-theta)/theta}. 

inverse.transform.function <- function(u, theta){
  
  #Transform using inverse distribution function
  x <- u^theta

  return(x)  
}



#Generate random uniform samples
u <- runif(10000)

#Generate samples from required distribution
x1 <- inverse.transform.function(u, 1)
x5 <- inverse.transform.function(u, 5)
x10 <- inverse.transform.function(u, 10)

#Plot results
X <- seq(0, 1, 0.01)
hist(x1, freq = FALSE, main = "", breaks = 20)
lines(X, rep(1, length(X)))

#Plot results
X <- seq(0, 1, 0.01)
hist(x5, freq = FALSE, main = "", breaks = 20)
lines(X, 0.2*X^-0.8)

#Plot results
X <- seq(0, 1, 0.01)
hist(x10, freq = FALSE, main = "", breaks = 20)
lines(X, 0.1*X^-0.9)
