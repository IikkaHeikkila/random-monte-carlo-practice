# --------------------------------------------------------------------
# Nonparametric and Parametric Bootstrap for Linear Mixed Effects Model
# --------------------------------------------------------------------

dyes <- data.frame(
  Batch = c(1, 2, 3, 4, 5, 6),
  Yield = c(	
  c(1545, 1540, 1595, 1445, 1595, 1520),
  c(1440, 1555, 1550, 1440, 1630, 1455),
  c(1440, 1490, 1605, 1595, 1515, 1450),
  c(1520, 1560, 1510, 1465, 1635, 1480),
  c(1580, 1495, 1560, 1545, 1625, 1445)
  )
)


#Nonparametric bootstrap
library(nlme)
library(boot)
bs = function(data, indices) {
  d = data[indices,] 
  fit = lme(Yield~1, data = d, random=~1|Batch)
  return(c(fixef(fit),unlist(ranef(fit))[1:6]))
}
results = boot(data=dyes, statistic=bs, R=10000)

results

boot.ci(results, type = "perc")
boot.ci(results, type = "bca")
boot.ci(results, type = "stud")



#Parametric bootstrap
fit = lme(Yield~1,data=dyes, random=~1|Batch)
res = residuals(fit)
yhat = fitted(fit)
est = data.frame(yhat,res)
bs = function(data){
  fit2 = lme(Yield~1,data=data, random=~1|Batch)
  return(c(fixef(fit2),unlist(ranef(fit2))))
}
sim = function(data,mle){
  data$Yield = mle$yhat+sample(mle$res,30,T)
  data
}
results2 = boot(dyes,statistic=bs,R=10000,sim="parametric",ran.gen=sim,mle=est)

results2

boot.ci(results2, type = "perc")
boot.ci(results2, type = "bca")
boot.ci(results2, type = "stud")

