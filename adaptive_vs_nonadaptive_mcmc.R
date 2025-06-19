# -----------------------------------------------------
# Adaptive vs Non-Adaptive MCMC on Rosenbrock Function
# -----------------------------------------------------

library(adaptMCMC)
library(coda)

objective_function <- function(x){
	-1*((1-x[1])^2 + 100*(x[2]-x[1]^2)^2)
}

set.seed(66)
x0 <- c(-1,1)
niter <- 20000
scale <- c(0.01, 0.01)

mcmc_with_adaptation <- MCMC(objective_function, init=x0, n=niter)

mcmc_without_adaptation <- MCMC(objective_function, init=x0, 
					scale=scale, n=niter, adapt=FALSE)

#---Trajectories
#plot(mcmc_with_adaptation$samples, dim = 2, type="l", main="Trajectories")
xn <- mcmc_with_adaptation$samples[nrow(mcmc_with_adaptation$samples),]
points(x0[1], x0[2], col = "red", pch = 16, cex = 2)
points(xn[1], xn[2], col = "blue", pch = 16, cex = 2)
xn
-objective_function(xn)

#---Coda stuff
mcmc <- as.mcmc(matrix(mcmc_with_adaptation$samples, ncol=2))
#traceplot(mcmc, main="Trace")
#plot(autocorr(mcmc), main = "Autocorr", type="l", xlab="delay")

#---Heidelberger-Welch
heidel.diag(mcmc)


#---Trajectories
#plot(mcmc_without_adaptation$samples, dim = 2, type="l", main="Trajectories")
xn <- mcmc_without_adaptation$samples[nrow(mcmc_without_adaptation$samples),]
points(x0[1], x0[2], col = "red", pch = 16, cex = 2)
points(xn[1], xn[2], col = "blue", pch = 16, cex = 2)
xn
-objective_function(xn)

#---Coda stuff
mcmc <- as.mcmc(matrix(mcmc_without_adaptation$samples, ncol=2))
#traceplot(mcmc, main="Trace")
#plot(autocorr(mcmc), main = "Autocorr", type="l", xlab="delay")

#---Heidelberger-Welch
heidel.diag(mcmc)
