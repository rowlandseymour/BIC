x <- -5:5
sigma <- 0.2
y <- cos(0.5*x) + log(x + 6)
plot(x, y, xlab = "x", ylab = "f(x)", ylim = c(0, 4))

sq.exp <- function(x,
                   y,
                   alpha,
                   ell) {
  #Calculate the Covariance Matrix with the covariance function of squared exponential
  #INPUTS: x, y -- data points, alpha -- vertical scale parameter, ell -- horizontal scale
  ##OUTPUTS: covar -- length(x)*length(y) covariance matrix

  covar <- matrix(rep(0, length(x) * length(y)), nrow = length(x)) # Initialise Empty Covariance Matrix
  for (i in 1:length(x)) {
    for (j in 1:length(y))
      covar[i, j] <- alpha ^ 2 * exp(-(x[i] - y[j]) ^ 2 / ell ^ 2)
  }
  return(covar)
}

mvnorm.chol <- function(mu, chol){
  #Multivariate Normal Sampler with Cholesky Input
  #Inputs: mu -- mean, chol -- cholesky decomposition of variance matrix (may need transposing)
  return(mu + t(chol)%*%rnorm(length(mu)))
}


x.star <- seq(-5, 5, 0.01)

k.x.x      <- sq.exp(x, x, 1, 1)
k.x.star.x <- sq.exp(x.star, x, 1, 1)
k.x.x.star <- sq.exp(x, x.star, 1, 1)
k.x.star.x.star <- sq.exp(x.star, x.star, 1, 1)

mu.star <- k.x.star.x%*%solve(k.x.x)%*%y
k.star  <- k.x.star.x.star - k.x.star.x%*%solve(k.x.x)%*%t(k.x.star.x)
k.star.chol <- chol(k.star + 1e-13*diag(length(x.star)))


f.matrix <- matrix(NA, length(x.star), 1000)
for(i in 1:1000)
  f.matrix[, i] <- mvnorm.chol(mu.star, k.star.chol)
f.upper <- apply(f.matrix, 1, quantile, 0.975)
f.lower <- apply(f.matrix, 1, quantile, 0.025)

plot(x, y, xlab = "x", ylab = "f(x)", ylim = c(0, 5), xlim = c(-5, 5))
lines(x.star, mu.star)
polygon(c(x.star, rev(x.star)), c(f.lower, rev(f.upper)), col = rgb(0,0, 1, 0.25), border = FALSE)

