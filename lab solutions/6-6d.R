x <- c(-5.93,  33.12, -21.41, -12.42, -17.64,  -5.47, -27.95, -22.25, -20.40, -26.28, -24.57, 3.06,  44.28, 6.02, -21.14,  14.79, -15.10, 53.18,  38.61,   5.71)

n.iter <- 50000
sigma.sq.store <- numeric(n.iter)
epsilon <- 30
for(i in 1:n.iter){

  sigma.sq <- rexp(1, 1)

  x.star <- rnorm(20, 5, sqrt(sigma.sq))

  d <- sum((mean(x)-mean(x.star))^2)

  if(d < epsilon){
    sigma.sq.store[i] <- sigma.sq
  } else{
    sigma.sq.store[i] <- NA
  }

}

#Get number of reject samples
sum(is.na(sigma.sq.store))

#Plot Approximate Posterior
hist(sigma.sq.store, freq = FALSE, xlab = expression(sigma^2), main = "")
mean(sigma.sq.store, na.rm = TRUE)
quantile(sigma.sq.store, c(0.025, 0.975), na.rm = TRUE)
