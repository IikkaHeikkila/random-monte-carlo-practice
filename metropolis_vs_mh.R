# -------------------------------------------------------------
# Monte Carlo Sampling in R: Metropolis and Metropolis-Hastings
# -------------------------------------------------------------

normm<-function (Nsim, a) 
{
        vec <- vector("numeric", Nsim)
        x <- 0 #I think that this is 0 because dnorm
        vec[1] <- x #First value to zero
	  count <- 0 #Setting count to zero for acceptance rate
        for (i in 2:Nsim) { 
                innov <- runif(1, -a, a) #innov from unif distribution
                Xstar <- x + innov #Adding "noise" x and putting that to Xstar
                aprob <- min(1, dnorm(Xstar)/dnorm(x)) #Calculating acceptance probability
                u <- runif(1)
                if (u < aprob){ #Checking acceptance
                        x <- Xstar #Updating x
				count <- count+1 #Count++
		    }
                vec[i] <- x #x_i to vec_i
        }
	  rate <- count/(Nsim-1) #Calculating acceptance rate
	  list(vec=vec, rate=rate) #Returns
}

set.seed(33)
Nsim <- 2000
a <- 55	#Tune here!
result <- normm(Nsim, a)
print(result$rate)
plot(result$vec, type = "l", xlab = "Iteration", 
	ylab = "Value", main = sprintf("a = %.1f", a))

hist(result$vec, breaks = 30, col = "blue", 
xlab = "Value", ylab = "Freq", main = sprintf("a = %.1f", a))


#---------------------------------------------------------------


gammh<-function (Nsim, a, b) #MH algorithm for gamma distribution
{
        mu <- a/b		#Mean of gamma
        sig <- sqrt(a/(b * b))	#Deviation of gamma
        vec <- vector("numeric", Nsim)
        x <- a/b
        vec[1] <- x
        for (i in 2:Nsim) {
             can <- rnorm(1, mu, sig) #From standard distribution
		 #Point here below is that if can is more dense in gamma and in norm,
		 #then there is a high propability to choose the second elemt of min()
		 #instead of 1
             aprob <- min(1, (dgamma(can, a, b)/dgamma(x, 
             a, b))/(dnorm(can, mu, sig)/dnorm(x,mu, sig)))
                u <- runif(1) #U from uniformal dist.
                if (u < aprob) #Checking acceptance
                        x <- can #Updating x
                vec[i] <- x #save to vec
        }
        vec #return
}
set.seed(33)
Nsim = 2000
a = 2				#Modify!
b = 2				#Modify!
vec = gammh(Nsim, a, b)

plot(vec, type = "l", xlab = "Iteration", 
	ylab = "Value", main = sprintf("a = %.2f, b = %.2f", a, b))

hist(vec, breaks = 30, col = "blue", 
xlab = "Value", ylab = "Freq", main = sprintf("a = %.2f, b = %.2f", a, b))












