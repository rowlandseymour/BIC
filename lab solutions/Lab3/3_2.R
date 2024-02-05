#The posterior distribution is proportion to
# \lambda^{N + \alpha - 1}*(1-\lambda)^{\beta - 1}*exp{-lambda*sum(y)}


# View Data ---------------------------------------------------------------

y <- c(1.95, 1.46, 4.81, 1.52, 4.24, 3.00, 0.46, 2.27, 1.76, 0.41)
hist(y, main = "", xlab = "y")
N <- length(y)


# Set Up Likelihood Function ----------------------------------------------
lambda <- seq(0, 1, 0.01) #grid of lambda values
likelihood.function <- function(lambda, y) prod(dexp(y, lambda)) #compute likelihood
log.likelihood.function  <- function(lambda, y) sum(dexp(y, lambda, log = TRUE)) #compute loglikelihood
likelihood <- sapply(lambda,  likelihood.function, y) #evaluate at grid of points
log.likelihood <- sapply(lambda,  log.likelihood.function, y) #evaluate at grid of points


# Set Up Prior Computationally  ------------------------------------------------------------
prior    <- dbeta(lambda, 1, 1)
log.prior <- dbeta(lambda, 1, 1, log = TRUE)
plot(lambda, prior, type = 'l', xlab = expression(lambda), ylab = "density")


# Construct Posterior Distribution Computationally ----------------------------------------
posterior <- prior*likelihood
integrating.factor <- 0.5*0.01*(posterior[1] + posterior[101] + 2*sum(posterior[-c(1, 101)])) #Using trapezium rule
posterior <- posterior/integrating.factor #normalise
plot(lambda, posterior, type = 'l', xlab = expression(lambda),
     ylab = "posterior density")
