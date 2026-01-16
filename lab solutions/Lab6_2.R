
#define potential V and unnormalised density
V <- function(x){
  abs(x)^3
}

exp_V <- function(x){
  exp(-V(x))
}

#calculate normalisation constant
C <- integrate(exp_V,-Inf,Inf)$value

#normalised density
pi <- function(x){
  exp(-V(x))/C
}

nabla_V <- function(x){
  sign(x)*3*x^2
}

#set step size h (experiment with a few different step sizes to see how the algorithm behaves)
h <- 0.03


#number of iterations
n <- 10000

#noise vector
xi <- rnorm(n,0,1)

#simulate process y with initialization y(0) = 0
y <- rep(0,n+1)

for (k in 1:n){
  y[k+1] = y[k] - h*nabla_V(y[k]) + sqrt(2*h)*xi[k]
}

hist(y, main = "", xlab = "y", breaks = 30, freq=FALSE)

x <- seq(-3,3,by=0.05)
lines(x,pi(x), col = "blue")

