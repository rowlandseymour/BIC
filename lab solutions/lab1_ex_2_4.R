X <- numeric(21)

#Set values for LCG
a <- 7
c <- 2
m <- 11
X[1]<- 0


#Run Generator
for(i in 2:50){
  X[i] <- (a*X[i-1] +c) %% m
}
X
(X+1)/(m + 1)
U <- (X+1)/(m + 1)

#Delay plot
plot(X[1:20], X[2:21], xlab = expression(X[i-1]), ylab = expression(X[i]), type = 'l')



# Shuffling ---------------------------------------------------------------

Y <- numeric(21)

#Set values for LCG
a <- 5
c <- 1
m <- 8
Y[1]<- 2


#Run Generator
for(i in 2:50){
  Y[i] <- (a*Y[i-1] +c) %% m
}
Y
X[Y]
plot(X[Y[1:20] + 1], X[Y[2:21] +1], xlab = expression(Z[i-1]), ylab = expression(Z[i]), type = 'l')



# RANDU ---------------------------------------------------------------

X<- numeric(10000)

#Set values for LCG
a <- 65539
c <- 0
m <- 2^31
X[1]<- 1


#Run Generator
for(i in 2:1000){
  X[i] <- (a*X[i-1] +c) %% m
}


plot(X[1:9999] , X[2:10000], xlab = expression(X[i-1]), ylab = expression(X[i]), type = 'l')


scatterplot3d::scatterplot3d(X[1:9998], X[2:9999], X[3:10000], angle=154,
                             xlab = expression(X[i]), ylab = expression(X[i+1]), zlab = expression(X[i+2]))

