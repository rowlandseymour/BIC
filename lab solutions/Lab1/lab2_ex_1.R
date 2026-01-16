multivariate.normal.density <- function(x, mu, sigma){

  #Evaluate preliminary values
  k <- dim(sigma)[1]
  sigma.inv <- solve(sigma)
  sigma.det <- det(sigma)

  #Evaluate pdf
  sq.root.terms <- (2*pi)^(-k/2)*sigma.det^(-k/2)
  exponent.term <- -0.5*t(x-mu)%*%solve(sigma)%*%(x-mu)

  #Return pdf
  result <- sq.root.terms*exp(exponent.term)
  return(result)

}


multivariate.normal.density(c(0, 1, 2), c(1, 1, 1), diag(3))



