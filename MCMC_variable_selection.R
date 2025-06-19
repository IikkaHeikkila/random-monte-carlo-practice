# -------------------------------------
# Bayesian Variable Selection via MCMC
# -------------------------------------

inv <- function(X){
	EV <- eigen(X)
	EV$vector%*%diag(1/EV$values)%*%t(EV$vector)
}


lpostw <- function(gam, y, X, beta){
	n <- length(y)
	qgam <- sum(gam)
	Xt1 <- cbind(rep(1,n), X[,which(gam==1)])
	if(gam!=0){
		P1 <- Xt1%*%inv(t(Xt1)%*%Xt1)%*%t(Xt1)
	}
	else{
		P1 <- matrix(0,n,n)
	}
	-(qgam+1)/2*log(n+1)-n/2*log(t(y)%*%y-n/(n+1)*t(y)%*%P1%*%y-1/(n+1)*t(beta)%*%t(cbind(rep(1,n),X))%*%P1%*%cbind(rep(1,n),X)%*%beta)
}

gocho <- function(niter, y, X){
	lga <- dim(X)[2]
	beta <- lm(y ~ ., data = df <- data.frame(y, X))$coeff
	y <- as.matrix(y)
	X <- as.matrix(X)
	gamma <- matrix(0, nrow=niter, ncol=lga)
	gamma[1,] <- sample(c(0,1), lga, rep=T)
	for(t in 1:(niter-1)){
		j <- sample(1:lga, 1)
		gam0=gam1=gamma[t,];gam1[j]=1-gam0[j]
		pr <- lpostw(gam0, y, X, beta)
		pr <- c(pr, lpostw(gam1, y, X, beta))
		pr <- exp(pr-max(pr))
		gamma[t+1,] <- gam0
		if(sample(c(0,1), 1, prob=pr)){
			gamma[t+1,]=gam1
		}
	}
	gamma
}

set.seed(33)
file_path <- "test.txt" #<--- Your file here 
data <- read.table(file_path)
df <- data.frame(data)
y <- df[,7]
X <- cbind(df[,3:6], df[,8:21])
niter <- 25000
gamma <- gocho(niter, y, X)
gamma <- gamma[5001:niter, ] #BURN!
apply(gamma, 2, mean)

prob_of_model <- rep(0, ncol(gamma))
for (i in 1:ncol(gamma)) {
  prob_of_model[i] <- sum(gamma[, i]) / nrow(gamma)
}
prob_sorted <- sort(prob_of_model, decreasing = TRUE)
prob_sorted
top_5 <- prob_sorted[1:5]
print(top_5)






