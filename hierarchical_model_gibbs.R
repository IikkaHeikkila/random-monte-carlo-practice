# --------------------------------------------------
# Bayesian Hierarchical Modeling with Gibbs Sampling
# --------------------------------------------------


#In this implementation, we suppose that there is no missing data
#...and theres equal n of rows for each variable 

#Use your own file here
Energy <- read.table("Energy.txt", header = TRUE)

set.seed(33)
niter <- 11000

n <- nrow(Energy) * ncol(Energy)
n_i <- nrow(Energy) #Same for all columns
k <- ncol(Energy)
my_0 <- 1000
a1 <- 1
b1 <- 1
a2 <- 1
b2 <- 1
a3 <- 1
b3 <- 1

theta_1 <- mean(Energy[,1])
theta_2 <- mean(Energy[,2])
x_1 <- theta_1 				#Save empiric average here
x_2 <- thera_2 				#...
my <- (theta_1 + theta_2) / 2

sigma_my2_collection <- c()
tau2_collection <- c()
sigma2_collection <- c()
my_collection <- c()
theta_1_collection <- c()
theta_2_collection <- c()


for (i in 1:niter){
	theta_line = (theta_1 + theta_2) / 2

	#This generates SUM_{i,j}(X_ij-theta_i)^2
	the_sum <- 0
	for (i in 1:nrow(Energy)) {
    		the_sum <- the_sum + (Energy[i,1] - theta_1)^2
		the_sum <- the_sum + (Energy[i,2] - theta_2)^2
	}

	sigma_my2 <- rgamma(1, 0.5 + a3, 0.5 * (my-my_0)^2 + b3)
	tau2 <- rgamma(1, k/2 + a2, 0.5 * ((theta_1 - my)^2 + (theta_2 - my)^2) + b2)
	sigma2 <- rgamma(1, n/2 + a1, 0.5 * the_sum + b1)
	my <- rnorm(1, my_0 * tau2/(tau2 + k * sigma_my2) + theta_line * k * sigma_my2/(tau2 + k * sigma_my2), 
			sigma_my2 * tau2/(tau2 + k * sigma_my2))
	theta_1 <- rnorm(1, my * sigma2/(sigma2 + n_i * tau2) + x_1 * n_i * tau2/(sigma2 + n_i * tau2),
			sigma2 * tau2/(sigma2 + n_i * tau2))
	theta_2 <- rnorm(1, my * sigma2/(sigma2 + n_i * tau2) + x_2 * n_i * tau2/(sigma2 + n_i * tau2),
			sigma2 * tau2/(sigma2 + n_i * tau2))
	
	sigma_my2_collection <- append(sigma_my2_collection, sigma_my2)
	tau2_collection <- append(tau2_collection, tau2)
	sigma2_collection <- append(sigma2_collection, sigma2)
	my_collection <- append(my_collection, my)
	theta_1_collection <- append(theta_1_collection, theta_1)
	theta_2_collection <- append(theta_2_collection, theta_2)
	
}

#BURN!
sigma_my2_collection <- sigma_my2_collection[1001:niter]
tau2_collection <- tau2_collection[1001:niter]
sigma2_collection <- sigma2_collection[1001:niter]
my_collection <- my_collection[1001:niter]
theta_1_collection <- theta_1_collection[1001:niter]
theta_2_collection <- theta_2_collection[1001:niter]

calculate_summary <- function(vector) {
	mean <- mean(vector)
  	median <- median(vector)
  	lower_ci <- quantile(vector, 0.025)
  	upper_ci <- quantile(vector, 0.975)
  	print(deparse(substitute(vector)))
	cat("Mean:", mean, "\n")
	cat("Median:", median, "\n")
	sprintf("95 CI: [%g, %g]", lower_ci, upper_ci)
}

calculate_summary(sigma_my2_collection)
calculate_summary(tau2_collection)
calculate_summary(sigma2_collection)
calculate_summary(my_collection)
calculate_summary(theta_1_collection)
calculate_summary(theta_2_collection)






