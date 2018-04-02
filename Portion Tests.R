########### BART ########### 
# BART = Bayesian Adaptive Regression Trees
# Lines 130-131

### original calculation from variables to identify:

## defining xtfull

Xt<- covs #line 432 of rep code 
#covs is defined in lasso section 

treatt<- treats #line 432 of rep code 
#treats defined in lasso section 

Xtfull <- model.matrix(~Xt*treatt) #line 56 of SLF

#install.packages("BayesTree")
library(BayesTree)
fit8<- bart(x.train=Xfull, y.train=factor(Y), x.test=Xtfull, ndpost=1000, nskip=500, usequants=T)

### test on portion:



fit8<- bart(x.train=Xfull, y.train=factor(Y), x.test=Xtfull, ndpost=1000, nskip=500, usequants=T)





