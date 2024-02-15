
# Set Up ABC function -----------------------------------------------------

abc.function <- function(epsilon){
  #input the value of epsilon


  #Set Up ABC Algorithm
  x <- c(2.6863422, 8.8468112, 8.8781831, 0.2712696, 1.8902442)
  n.iter <- 50000
  lambda.store <- numeric(n.iter)

  #Run ABC
  for(i in 1:n.iter){

    #Propose lambda
    lambda <-rbeta(1, 1, 3)

    #Simulate data
    x.star <- rexp(5, lambda)
    d <- sum((x-x.star)^2)

    #Accept Reject
    if(d < epsilon){
      lambda.store[i] <- lambda
    } else{
      lambda.store[i] <- NA
    }

  }

  #Report median
  return(median(lambda.store, na.rm = TRUE))
}

#Run for each value of epsilon
epsilon <- 20:100
abc.lambda <- numeric(length(epsilon))

for(i in 1:length(epsilon))
  abc.lambda[i] <- abc.function(i)

#Report relative error
plot(epsilon, (abc.lambda - 0.2)/0.2*100, xlab = expression(epsilon), ylab = "Relative Error")
