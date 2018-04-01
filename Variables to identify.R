
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

############# different def of covs on line 863 of rep code. Investigate this 

# Set WD as needed: 
setwd("C:/Users/jgros/documents/GitHub/Project/")
load("Het_Experiment.Rdata")

dem<- ifelse(svdat$pid3l=='Dem', 1, 0)  #line 366-369 of rep code
dem[which(is.na(dem))]<- 0
rep<- ifelse(svdat$pid3l=='Rep', 1, 0)
rep[which(is.na(rep))]<- 0

cons<- ifelse(svdat$ideo3<3, 1, 0) #line 230-231 of rep code
lib<- ifelse(svdat$ideo3==4|svdat$ideo3==5, 1, 0)

lib[which(is.na(lib))]<- 0 #line 370-371 of rep code 
cons[which(is.na(cons))]<- 0


############ Defining treats

type.mat<- matrix(0, nrow = 1074, ncol=7)
colnames(type.mat)<- sort(unique(as.character(svdat$cond.type)))
for(z in 1:nrow(type.mat)){
  type.mat[z,which(colnames(type.mat)==svdat$cond.type[z])]<- 1
  }

type.mat.final<- type.mat[,-1]


types<- sort(unique(as.character(svdat$cond.type)))
type.num<- match(svdat$cond.type, types)
number<- c('control', '$20 million', '$50 thousand')
amount.num<- match(svdat$cond.money, number)
request<- c('control', 'requested', 'secured', 'will request')
stage.num<- match(svdat$cond.stage, request)
party<- c('control', 'a Republican', 'a Democrat')
party.num<- match(svdat$cond.party, party)
along<- c('control', 'alone', 'w/ Rep', 'w/ Dem')
along.num<- match(svdat$cond.alongWith, along)


num.mat<- matrix(0, nrow=1074, ncol=3)
colnames(num.mat)<- number
for(z in 1:nrow(num.mat)){
  num.mat[z,which(colnames(num.mat)==svdat$cond.money[z])]<- 1
}
num.mat.final<- num.mat[,-1]

stage.mat<- matrix(0, nrow=1074, ncol=4)
colnames(stage.mat)<- request
for(z in 1:nrow(stage.mat)){
  stage.mat[z,which(colnames(stage.mat)==svdat$cond.stage[z])]<- 1
}

stage.mat.final<- stage.mat[,-1]

party.mat<- matrix(0, nrow=1074, ncol=3)
colnames(party.mat)<- party
for(z in 1:nrow(party.mat)){
  party.mat[z, which(colnames(party.mat)==svdat$cond.party[z])]<- 1
}

party.mat.final<- party.mat[,-1]	

along.mat<- matrix(0, nrow=1074, ncol=4)
colnames(along.mat)<- 	along
for(z in 1:nrow(along.mat)){
  along.mat[z,which(colnames(along.mat)==svdat$cond.alongWith[z])]<- 1
}

along.mat.final<- along.mat[,-1]	


treats<- cbind(type.mat.final, num.mat.final[,1], stage.mat.final[,1:2],party.mat.final[,1], 
               along.mat.final[,1:2], type.mat.final[,1:5]*num.mat.final[,1], type.mat.final[,1:5]*stage.mat.final[,1], 
               type.mat.final[,1:5]*stage.mat.final[,2], type.mat.final[,1:5]*party.mat.final[,1], type.mat.final[,1:5]*along.mat.final[,1],
               type.mat.final[,1:5]*along.mat.final[,2], num.mat.final[,1]*stage.mat.final[,1], num.mat.final[,1]*stage.mat.final[,2], 
               num.mat.final[,1]*party.mat.final[,1], num.mat.final[,1]*along.mat.final[,1], num.mat.final[,1]*along.mat.final[,2],
               stage.mat.final[,1:2]*party.mat.final[,1], stage.mat.final[,1:2]*along.mat.final[,1], 
               stage.mat.final[,1:2]*along.mat.final[,2], party.mat.final[,1]*along.mat.final[,1], party.mat.final[,1]*along.mat.final[,2] )


treat<- treats #line 448 of rep code 


### Defining the X variable 
covs<- cbind(dem, rep, lib, cons) #line 373 of rep code 
X <- covs #line 432 of repcode 
Xfull <- model.matrix(~X*treat)


## line 391 of rep code 


#Defining the Y variable 

#line 432 of rep code 
Y<- approve_bi<- ifelse(svdat$approval<3, 1, 0) #line 292 of rep code 




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

#install.packages("glmnet")
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

#install.packages("FindIt")
library(FindIt)

#Next two lines change response variable from 0/1 to -1/1 (i.e. turn 0 into -1)
FIY <- Y
FIY[FIY==0] <- -1


mkstand <- function(x){
  x <- x - mean(x, na.rm=T)
  if(sd(x, na.rm=T) >0){
    x <- x/sd(x, na.rm=T)		
  }
  return(x)
}


Xstd <- apply(X, 2, mkstand) #line 50 0f SLF


?ncol

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


#seems to require Y, Xstd, treat
# Y and treat already defined 



##### Bayesian GLM #######

# Lines 116-117

#install.packages("arm")
library(arm)
fit6<- bayesglm(Y~Xfull-1, family=binomial(link=logit))

## Y and Xfull defined in Lasso section 

######### Boosted Tree ######
# NOTE: this appears in the SLF_round2 file, but does not appear in Table 2 of paper
# MIGHT NOT NEED TO DO THIS
# Lines 122 -125

#install.packages("mboost")
#install.packages("GAMBoost")
library(mboost)
library(GAMBoost)
fit7<- GLMBoost(Xfull[,-1],Y,penalty= 100,stepno=100,  trace = T,  family=binomial())

#runs but it has the following warning:  "In 1/((x.linear[subset, best.candidate.linear] * D * weights) %*%  ... :
#Recycling array of length 1 in array-vector arithmetic is deprecated.
#Use c() or as.vector() instead.


#xfull and Y already defined 

########### BART ########### 
# BART = Bayesian Adaptive Regression Trees
# Lines 130-131

#install.packages("BayesTree")
library(BayesTree)
fit8<- bart(x.train=Xfull, y.train=factor(Y), x.test=Xtfull, ndpost=1000, nskip=500, usequants=T)


## defining xtfull

Xtfull <- model.matrix(~Xt*treatt) #line 56 of SLF
Xt<- covs #line 432 of rep code 
#covs is defined in lasso section 

treatt<- treats #line 432 of rep code 
#treats defined in lasso section 




######  Random Forest #######
# Lines 136-137
#install.packages("randomForest")
library(randomForest)
fit9<- randomForest(y = factor(Y), x = Xfull)

#already defined 


###### KRLS ############
# KRLS = Kernel-Based Regularized Least Squares (very new ML method, 2014)
# Line 150
#install.packages("KRLS")
library(KRLS)
fit11<- krls(X = Xfull[,-1], y = Y, derivative=F)
#warnings: 1: In Eigenobject$values + lambda :
#Recycling array of length 1 in vector-array arithmetic is deprecated.
#Use c() or as.vector() instead.

#already defined 

###### SVM-SMO #############
# SVM = Support Vector Machine
# 155- 165
# NOTE: this method requires 4 gb of RAM Free to Run. 
# I am commenting this section out, so you can verify that you have this before running this section.

#install.packages("rJava")
library(rJava)
.jinit(parameters="-Xmx4g")

#install.packages("RWeka")
library(RWeka)


fit12 <- SMO(Y ~ ., data = data.frame(Y=factor(Y),Xfull),
             control = Weka_control(M = TRUE ) )


######## Naive Average ####
# Just a simple mean
# Don't know where this is in the code, but should be just:
mean(y)