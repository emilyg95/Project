
#What we are doing 

part2<- wrap.func(covs, approve_bi, treats, covs, approve_bi, treats, speed=F)
#Do above function, indiviudally, for each test

#Elements that go into function 

covs<- cbind(dem, rep, lib, cons)
colnames(covs)<- c('Dem', 'Rep', 'Lib', 'Cons')

approve_bi<- ifelse(svdat$approval<3, 1, 0)

fit1<- cv.glmnet(y = Y, x= Xfull, alpha=1, family='binomial', type='mse')

treats<- cbind(type.mat.final, num.mat.final[,1], stage.mat.final[,1:2],party.mat.final[,1], 
               along.mat.final[,1:2], type.mat.final[,1:5]*num.mat.final[,1], type.mat.final[,1:5]*stage.mat.final[,1], 
               type.mat.final[,1:5]*stage.mat.final[,2], type.mat.final[,1:5]*party.mat.final[,1], type.mat.final[,1:5]*along.mat.final[,1],
               type.mat.final[,1:5]*along.mat.final[,2], num.mat.final[,1]*stage.mat.final[,1], num.mat.final[,1]*stage.mat.final[,2], 
               num.mat.final[,1]*party.mat.final[,1], num.mat.final[,1]*along.mat.final[,1], num.mat.final[,1]*along.mat.final[,2],
               stage.mat.final[,1:2]*party.mat.final[,1], stage.mat.final[,1:2]*along.mat.final[,1], 
               stage.mat.final[,1:2]*along.mat.final[,2], party.mat.final[,1]*along.mat.final[,1], party.mat.final[,1]*along.mat.final[,2] )




#What to do, define every variable in this file 

######################################################
###### From SLF_round 2 -- Comments by Jon ###########
######################################################


## See table 2 on pg 422 of Reference Paper for full list of methods

################## Setup ###############
## Packages you will need to install (uncomment and run this section once):
## You can uncomment all lines at once by highlighting the lines, then ctrl+shift+c
# install.packages(glmnet)
# install.packages(FindIt)
# install.packages(arm)
# install.packages(GAMBoost)
# install.packages(mboost)
# install.packages("KRLS")
# install.packages("rJava")
# install.packages("RWeka")



########### Methods & Vars ############
#### @Jack & @Emily -- do this ########

##  Variables that are used an need to be identified:
# Y
# Xfull

# It looks like, X-full is some variation of the X input passed into wrap Function
# X full looks to be created in lines 25-66 from X in SLF_round 2
# based on RepCode Line 425, that X seems to be covs in RepCode, and Y is approve_bi

# Variables below need to be identified, but are used only in FindIt, so the other methods should still run if can't get these:
# treat
# Xstd
# SDsToRescaleX



######### Lasso & Elastic Net ########
# Lines 68-74 in SLF_round2

# Lasso and Elastic Net use cv.glmnet(), which is an extension of glmnet()
# cv.glmnet() = glmnet with k-fold cross-validation()
# glm = generalized linear model (i.e. extension of linear regression model)
# Help:
## ? glmnet()
## ? cv.glmnet()

library(glmnet)
# Alpha = 1 is same as lasso 
fit1<- cv.glmnet(y = Y, x= Xfull, alpha=1, family='binomial', type='mse')
# Elastic Net, Alpha = .5
fit2<- cv.glmnet(y = Y, x= Xfull, alpha=0.5, family='binomial', type='mse')
# Elastic Net, Alpha = .2 5
fit3<- cv.glmnet(y = Y, x= Xfull, alpha=0.15, family='binomial', type='mse')
fit4<- cv.glmnet(y = Y, x= Xfull, alpha=0, family='binomial', type='mse')


######### FindIt ########
# Lines 76-113 in SLF_round2
# Supposedly takes a lot of time to run

library(FindIt)

#Next two lines change response variable from 0/1 to -1/1 (i.e. turn 0 into -1)
FIY <- Y
FIY[FIY==0] <- -1

# If processing on data that has received treatment
if(is.null(ncol(treat))==F){
  
  #Pre-processing of data (i.e. formatting)
  colnames(Xstd)<- paste('Cov', 1:ncol(Xstd), sep='')
  colnames(treat)<- as.character(1:ncol(treat))
  start<- model.matrix(~Xstd*treat)
  treat2<- start[,which(colnames(start)=='treat1'):ncol(start)]
  
  #Next Line is the acctual Algorithm
  # See ?FindIt for Syntax
  fit5 <- FindIt(FIY,X.c=Xstd, treat2, type='multiple',
                 scale.c= SDsToRescaleX, 
                 search.lambdas=TRUE, 
                 fit.glmnet=TRUE,wts=1) }

#If processing on data that has not received treatment
if(is.null(ncol(treat)) == T){
  colnames(Xstd)<- paste('Cov', 1:ncol(Xstd), sep='')
  fit5 <- FindIt(FIY,X.c=Xstd, treat, type='single',
                 scale.c= SDsToRescaleX, 
                 search.lambdas=TRUE, 
                 fit.glmnet=TRUE, wts=1)	}


##### Bayesian GLM #######

# Lines 116-117

library(arm)
fit6<- bayesglm(Y~Xfull-1, family=binomial(link=logit))

######### Boosted Tree ######
# NOTE: this appears in the SLF_round2 file, but does not appear in Table 2 of paper
# MIGHT NOT NEED TO DO THIS
# Lines 122 -125
library(mboost)
library(GAMBoost)
fit7<- GLMBoost(Xfull[,-1],Y,penalty= 100,stepno=100,  trace = T,  family=binomial())

########### BART ########### 
# BART = Bayesian Adaptive Regression Trees
# Lines 130-131
library(BayesTree)
fit8<- bart(x.train=Xfull, y.train=factor(Y), x.test=Xtfull, ndpost=1000, nskip=500, usequants=T)


######  Random Forest #######
# Lines 136-137
library(randomForest)
fit9<- randomForest(y = factor(Y), x = Xfull)


###### KRLS ############
# KRLS = Kernel-Based Regularized Least Squares (very new ML method, 2014)
# Line 150
library(KRLS)
fit11<- krls(X = Xfull[,-1], y = Y, derivative=F)

###### SVM-SMO #############
# SVM = Support Vector Machine
# 155- 165
# NOTE: this method requires 4 gb of RAM Free to Run. 
# I am commenting this section out, so you can verify that you have this before running this section.

# library(rJava)
# .jinit(parameters="-Xmx4g")
# library(RWeka)
# 
# 
# fit12 <- SMO(Y ~ ., data = data.frame(Y=factor(Y),Xfull),
#              control = Weka_control(M = TRUE ) )
# 

######## Naive Average ####
# Just a simple mean
# Don't know where this is in the code, but should be just:
mean(y)
