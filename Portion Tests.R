########### BART ########### 
# BART = Bayesian Adaptive Regression Trees
# Lines 130-131

### original calculation from variables to identify:

## defining variables

treats<- cbind(type.mat.final, num.mat.final[,1], stage.mat.final[,1:2],party.mat.final[,1], 
               along.mat.final[,1:2], type.mat.final[,1:5]*num.mat.final[,1], type.mat.final[,1:5]*stage.mat.final[,1], 
               type.mat.final[,1:5]*stage.mat.final[,2], type.mat.final[,1:5]*party.mat.final[,1], type.mat.final[,1:5]*along.mat.final[,1],
               type.mat.final[,1:5]*along.mat.final[,2], num.mat.final[,1]*stage.mat.final[,1], num.mat.final[,1]*stage.mat.final[,2], 
               num.mat.final[,1]*party.mat.final[,1], num.mat.final[,1]*along.mat.final[,1], num.mat.final[,1]*along.mat.final[,2],
               stage.mat.final[,1:2]*party.mat.final[,1], stage.mat.final[,1:2]*along.mat.final[,1], 
               stage.mat.final[,1:2]*along.mat.final[,2], party.mat.final[,1]*along.mat.final[,1], party.mat.final[,1]*along.mat.final[,2] )


treat<- treats #line 448 of rep code 

covs<- cbind(dem, rep, lib, cons) #line 373 of rep code 
X <- covs #line 432 of repcode 
Xfull <- model.matrix(~X*treat)

Xt<- covs #line 432 of rep code 
#covs is defined in lasso section 

treatt<- treats #line 432 of rep code 
#treats defined in lasso section 

Xtfull <- model.matrix(~Xt*treatt) #line 56 of SLF

Y<- approve_bi<- ifelse(svdat$approval<3, 1, 0) #line 292 of rep code 

#install.packages("BayesTree")
library(BayesTree)
fit8<- bart(x.train=Xfull, y.train=factor(Y), x.test=Xtfull, ndpost=1000, nskip=500, usequants=T)

## with Xfull twice instead of Xtfull b/c they seem the same? Works...
fit8<- bart(x.train=Xfull, y.train=factor(Y), x.test=Xfull, ndpost=1000, nskip=500, usequants=T)

### test on portion:

covstest_1 <- covs[c(1:107),]
covsfull_1 <- covs[-c(1:107),]

treattest_1 <- treat[c(1:107),]
treatfull_1 <-treat[-c(1:107),]

Xfull_1 <-model.matrix(~covsfull_1*treatfull_1)

Xtest_1 <- model.matrix(~covstest_1*treattest_1)

Yfull_1<- ifelse(svdat[-c(1:107),]$approval<3, 1, 0)

fit8<- bart(x.train=Xfull_1, y.train=factor(Yfull_1), x.test=Xtest_1, ndpost=1000, nskip=500, usequants=T)
## works!



#Show how to use predict

library(glmnet)
# Alpha = 1 is same as lasso

Yfull_1<- ifelse(svdat[-c(1:107),]$approval<3, 1, 0)

Xfull_1 <-model.matrix(~covsfull_1*treatfull_1)

fit1<- cv.glmnet(y = Yfull_1, x= Xfull_1, alpha=1, family='binomial', type='mse')

best.lambda = fit1$lambda.min

fit1.coefs = predict(fit1,s = best.lambda, type= "coefficients")
fit1.coefs

Ytest_1<- ifelse(svdat[c(1:107),]$approval<3, 1, 0)

fit1.predict = predict(fit1, s= best.lambda, newx = Xtest_1)
fit1.RSS = sum((fit1.predict-Ytest_1)^2)
fit1.RSS

fit1.MSE = mean((fit1.predict-Ytest_1)^2)
fit1.MSE

# Elastic Net, Alpha = .5
fit2<- cv.glmnet(y = Y, x= Xfull, alpha=0.5, family='binomial', type='mse')
# Elastic Net, Alpha = .2 5
fit3<- cv.glmnet(y = Y, x= Xfull, alpha=0.15, family='binomial', type='mse')
fit4<- cv.glmnet(y = Y, x= Xfull, alpha=0, family='binomial', type='mse')




