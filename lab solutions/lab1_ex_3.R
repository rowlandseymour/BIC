# Plot one ----------------------------------------------------------------
x <- rnorm(100, 1.001, 0.005) #Simulate daily change for 100 days
plot(100*cumprod(x), type = 'l') #multiply each day by the previous days

# Plot 100 realisations ---------------------------------------------------
market.index <- matrix(NA, 100, 100) #Initialise a matrix to store trajectories
for(i in 1:100){
  x <- rnorm(100, 1.001, 0.005)
  market.index [, i] <- 100*cumprod(x)
}

#Plot all trajectories
matplot(prices, type = 'l')

#Get distribution of days 50 and 100
hist(prices[50, ])
hist(prices[100, ])
quantile(prices[50, ], c(0.25, 0.5, 0.75))
quantile(prices[100, ], c(0.25, 0.5, 0.75))



