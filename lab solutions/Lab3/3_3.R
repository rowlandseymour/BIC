#The posterior distribution is Beta(sum(x) + \alpha, 100*N - sum(x) + \beta )


# Function for posterior distribution -------------------------------------
#This function computes the posterior distribution
#It has four inputs: sum.x and N describing the data, and alpha and beta (both numerics correpsonding to the prior distribution)
#It outputs a vector evaluating the posterior dstirbution at [0, 0.01, 0.02, ..., 10]
evaluate.posterior <- function(sum.x, N, alpha, beta){
  
  
  #Create grid
  p <- seq(0, 1, 0.01)
  
  #Evaluate posterior
  posterior <- dbeta(p, sum.x + alpha, 100*N - sum.x + beta)
  
  #Return posterior
  return(posterior)
}



# Large data scenario -----------------------------------------------------
p <- seq(0, 1, 0.01) #Create grid of p values
beta <- seq(0.01, 10, 0.01) #Create grid of beta values
all.posteriors <- sapply(beta, evaluate.posterior, sum.x = 2971, N = 150, alpha = 2)


#Plot Posterior and Prior
plot(p, all.posteriors[, 100], type = 'l', xlab = "p", ylab = "density")
lines(p, dbeta(p, 2, beta[100]), col = 2)

#The prior mean is alpha/ alpha + beta. The posterior mean is sum(x) + alpha / alpha + beta + 100N
prior.mean <- 2/(2 + beta)
posterior.mean <- (2971 + 2)/(2 + 100*150 + beta)

#Create Plots
plot(beta, posterior.mean, type = 'l', ylim = c(0, 1), xlab = expression(beta), ylab = "posterior mean")
plot(prior.mean, posterior.mean, type = 'l', ylim = c(0, 1), xlab = "prior mean", ylab = "posterior mean")



# Low data scenario -----------------------------------------------------
p <- seq(0, 1, 0.01) #Create grid of p values
beta <- seq(0.01, 10, 0.01) #Create grid of beta values
all.posteriors <- sapply(beta, evaluate.posterior, sum.x = 101, N = 5, alpha = 2)


#Plot Posterior and Prior
plot(p, all.posteriors[, 1000], type = 'l', xlab = "p", ylab = "density")
lines(p, dbeta(p, 2, beta[1000]), col = 2)

#The prior mean is alpha/ alpha + beta. The posterior mean is sum(x) + alpha / alpha + beta + 100N
prior.mean <- 2/(2 + beta)
posterior.mean <- (101 + 2)/(2 + 100*5 + beta)

#Create Plots
plot(beta, posterior.mean, type = 'l', ylim = c(0, 1), xlab = expression(beta), ylab = "posterior mean")
plot(prior.mean, posterior.mean, type = 'l', ylim = c(0, 1), xlab = "prior mean", ylab = "posterior mean")


#This one is pretty invariant to the choice of prior parameter for beta in low data settings. If you start changing alpha too, it becomes much less invariant.


