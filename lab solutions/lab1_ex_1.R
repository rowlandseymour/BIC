#Sample on unit square
N <- 10000      #number of points
x <- runif(N)   #sample N points uniformly at random on [0, 1]
y <- runif(N)   #sample N points uniformly at random on [0, 1]

#Estimate pi
r.sq                 <- x^2 + y^2                #check how far from origin
number.inside.circle <- sum(r.sq <= 1)           #count how mnay points inside unit cirlce
pi.estimate          <- number.inside.circle/N*4

#Plot points
par(pty = "s")         #make sure plot is square
plot(x, y, cex = 0.1)  #plot points
theta <- seq(0, pi/2, 0.01) #plot unit circle
lines(x = cos(theta), y = sin(theta), col = "red")


# Relative Error Question -------------------------------------------------

pi.estimate <- numeric(10000)

for(N in 1:10000){
  #Sample on unit square
  x <- runif(N)   #sample N points uniformly at random on [0, 1]
  y <- runif(N)   #sample N points uniformly at random on [0, 1]

  #Estimate pi
  r.sq                 <- x^2 + y^2                #check how far from origin
  number.inside.circle <- sum(r.sq <= 1)           #count how many points inside unit cirlce
  pi.estimate[N]          <- number.inside.circle/N*4
}


rel.error <- (pi.estimate - pi)/pi
plot(1:10000, rel.error, type = 'l', xlab = "N", ylab = "Relative error")


