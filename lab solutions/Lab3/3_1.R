# View Data ---------------------------------------------------------------

y <- c(2, 6, 2, 3, 4, 3, 4, 3, 1, 2, 3, 2, 6, 6, 2, 3, 5, 1, 2, 2, 4, 2, 5, 3,
       6, 4, 1, 2, 7, 8, 4, 3, 7, 3, 3, 5, 2, 6, 1, 3, 7, 4, 2, 6, 8, 8, 4, 5,
       7, 4)
hist(y, main = "", xlab = "Reaction time (ms)")


# Set Up Likelihood Function ----------------------------------------------
lambda <- seq(0, 10, 0.01) #grid of lambda values
likelihood.function <- function(lambda, y) prod(dpois(y, lambda)) #compute likelihood
log.likelihood.function  <- function(lambda, y) sum(dpois(y, lambda, log = TRUE)) #compute loglikelihood
likelihood <- sapply(lambda,  likelihood.function, y) #evaluate at grid of points
log.likelihood <- sapply(lambda,  log.likelihood.function, y) #evaluate at grid of points


# Set Up Prior Computationally  ------------------------------------------------------------
lambda   <- seq(0, 10, 0.01) #grid of lambda values
prior    <- dexp(lambda, 0.1)
log.prior <- dexp(lambda, 0.1, log = TRUE)
plot(lambda, prior, type = 'l', xlab = expression(lambda), ylab = "density")


# Construct Posterior Distribution Computationally ----------------------------------------
posterior <- prior*likelihood
integrating.factor <- 0.5*0.01*(posterior[1] + posterior[1001] + 2*sum(posterior[-c(1, 1001)])) #Using trapezium rule
posterior <- posterior/integrating.factor #normalise
plot(lambda, posterior, type = 'l', xlab = expression(lambda), 
     ylab = "posterior density")


# Construct Posterior Distribution Analytically ---------------------------
#The analytical distirbution is a Gamma(sum(y) +1, 50.1) distirbution
posteiror.analytical <- dgamma(lambda, sum(y) + 1, 50.01)
plot(lambda, posterior, type = 'l', xlab = expression(lambda), 
     ylab = "posterior density")
lines(lambda, posteiror.analytical, col = 2)
max(abs(posterior - posteiror.analytical)) #maximum absolute error
