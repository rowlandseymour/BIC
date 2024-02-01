#The posterior distribution has density \pi( \beta \mid y) \propto \beta^{N + a + 1}e^{-b\beta}/prod(y^\beta + 1)


# View Data ---------------------------------------------------------------
y <- c(1.080217, 1.660542, 1.057368, 1.231186, 1.006873, 2.143955, 1.815231, 1.000864)
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
