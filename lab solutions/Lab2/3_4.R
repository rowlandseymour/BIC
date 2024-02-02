#The posterior distribution has density \pi( \beta \mid y) \propto \beta^{N + a + 1}e^{-b\beta}/prod(y^\beta + 1)


# View Data ---------------------------------------------------------------
y <- c(1.019844, 1.043574, 1.360953, 1.049228, 1.491926, 1.192943, 1.323738, 1.262572, 2.034768, 1.451654)
hist(y, main = "")
N <-length(y)

# Evaluate Posterior -----------------------------------------------------
a <- 1
b <- 2

#Create Grid
beta <- seq(0, 5, 0.01)

#Compute and plot posterior (check that MAP is there)
posterior <- beta^(N + a - 1)*exp(-b*beta)/(prod(y)^(beta+1))
plot(beta, posterior, type = 'l')

#Compute Map
beta[which.max(posterior)]
