# --------------------------------------------------------
# Bayesian Model Averaging for Regression via MC3 Algorithm
# --------------------------------------------------------

library("BMA")
set.seed(666)
file_path <- "test.txt"
data <- read.table(file_path)
df <- data.frame(data)
y <- df[,7]
X <- cbind(df[,3:6], df[,8:21]) #Discard index variable and nominal variable
niter <- 100000

model <- MC3.REG(y, X, num.its = niter, outliers=FALSE, K=5)
summary(model)
