load("Het_Experiment.Rdata")

#############################  Defining Variables ############################### 

## dem, rep, cons, lib

dem<- ifelse(svdat$pid3l=='Dem', 1, 0)  #line 366-369 of rep code
dem[which(is.na(dem))]<- 0
rep<- ifelse(svdat$pid3l=='Rep', 1, 0)
rep[which(is.na(rep))]<- 0

cons<- ifelse(svdat$ideo3<3, 1, 0) #line 230-231 of rep code
lib<- ifelse(svdat$ideo3==4|svdat$ideo3==5, 1, 0)

lib[which(is.na(lib))]<- 0 #line 370-371 of rep code 
cons[which(is.na(cons))]<- 0


## treats

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


## X variable 

covs.original <- cbind(dem, rep, lib, cons) #line 373 of rep code 
X.original <- covs #line 432 of repcode 
Xfull.original <- model.matrix(~X*treat)


#Defining the Y variable 

Y.original <- approve_bi<- ifelse(svdat$approval<3, 1, 0) #line 292 of rep code 



############################ Define Function to make OOS Predicts #########################################################

OOSPredicts = function(x,y, test.indexes = sample(length(y),as.integer(length(y)/10))){
  # NOTE: THIS IS NOT DOING CROSS-VALIDATION right now (for most models)
  
  #test.indexes = sample(1074,107)
  x.train = x[-test.indexes,]
  x.test = x[test.indexes,]
  y.train = y[-test.indexes]
  y.test = y[test.indexes]
  
  
  ##Now predict them
  logist<- function(x){
    ff<- 1/(1 + exp(-x))
    return(ff)
  }
  
  # For fits 1,2, 3 we optimize the hyperparameters once, for efficiency (loses some accuracy)
  # Lasso
  fit1<- glmnet(x = x.train, y = y.train, alpha=1, family='binomial')
  fit1.predict = predict(fit1, s= best.lambda1, newx = x.test)
  fit1.logistPred = logist(fit1.predict)
  
  
  # Elastic Net, Alpha = .5
  fit2<- glmnet(y = y.train, x= x.train, alpha=0.5, family='binomial')
  fit2.predict = predict(fit2, s= best.lambda2, newx = x.test)
  fit2.logistPred = logist(fit2.predict)
  
  # Elastic Net, Alpha = .25
  fit3<- glmnet(y = y.train, x= x.train, alpha=0.25, family='binomial')
  fit3.predict = predict(fit3, s= best.lambda3, newx = x.test)
  fit3.logistPred = logist(fit3.predict)
  
  # Fit4 not published in paper -- so not doing it
  #fit4<- cv.glmnet(y = Y, x= Xfull, alpha=0, family='binomial', type='mse')
  
  ## Skipping FindIt since documentation changed (per)
  
  # Bayesian GLM -- Revisit -- probably not working right
  fit6<- bayesglm(y.train~x.train-1, family=binomial(link=logit))
  fit6.predict = logist(x.test%*%fit6$coefficients)
  
  # Fit 7 = Boosted Trees is not published ### SKipping
  
  # Fit 8 = BART
  fit8<- bart(x.train=x.train, y.train=factor(y.train), x.test=x.test, ndpost=1000, nskip=500, usequants=T)
  fit8.pred<- pnorm(apply(fit8$yhat.test, 2, mean))
  
  # Fit 9  = RandomForest
  fit9<- randomForest(y = factor(y.train), x = x.train)
  X.test.forest = x.test
  colnames(X.test.forest) = colnames(x.train)
  fit9.pred.raw = predict(fit9,newdata = X.test.forest,type = "prob" )
  fit9.pred = fit9.pred.raw[,2]
  
  
  # Fit 10 = Skipped in Paper and SLF_round2 code
  
  # Fit 11 = KRLS
  # Gets errors if all vals in column have same value (i.e. all 0 or all 1)
  x.train.krls = x.train[,-1]
  x.test.krls = x.test[,-1]
  
  bad.KRLS.index=numeric()
  sums = as.numeric(apply(x.train.krls,2,sum))
  bad.KRLS.index = which(sums==0)
  bad.KRLS.index = c(bad.KRLS.index, which(sums==dim(x.train.krls)[1]))
  
  if (length(bad.KRLS.index)>0){
    x.train.krls = x.train.krls[,-bad.KRLS.index]
    x.test.krls = x.test.krls[,-bad.KRLS.index]
  }
  fit11<- krls(X = x.train.krls, y = y.train, derivative=F)
  
  fit11.predict = predict(fit11,newdata = x.test.krls )$fit
  
  # Fit 12 = SVM-SMO
  .jinit(parameters="-Xmx4g")
  subset.index = (1:length(y))[-test.indexes]
  fit12 <- SMO(y ~ ., data = data.frame(y=factor(y),x), control = Weka_control(M = TRUE ) , subset = subset.index)
  fit12.predict =predict(fit12, newdata= data.frame(x[test.indexes,]), type="probability" )[,2]
  
  
  # Fit 13 = Simple Mean
  fit13.predict = mean(y.train)
  fit13.RSME= sqrt(mean((fit13.predict-y.test)^2))
  
  # # W/O FIt 12 (Java)
  # models = rbind("Lasso", "Elastic Net (a = .5)","Elastic Net (a = .25)", "Bayesian GLM", "BART", "Random Forest", "KRLS" , "Simple Average")#"SVM_SMO"
  # Preds.All = cbind(fit1.logistPred,fit2.logistPred,fit3.logistPred,fit6.predict,fit8.pred,fit9.pred,fit11.predict,  fit13.predict)#fit12
  
  
  # W/ Fit 12 (Java)
  models = rbind("Lasso", "Elastic Net (a = .5)","Elastic Net (a = .25)", "Bayesian GLM", "BART", "Random Forest", "KRLS", "SVM_SMO", "Simple Average")
  Preds.All = cbind(fit1.logistPred, fit2.logistPred ,fit3.logistPred,fit6.predict,fit8.pred,fit9.pred,fit11.predict, fit12.predict,  fit13.predict)
  
  colnames(Preds.All) = models
  return(Preds.All)
}

############################ Setup for ML ########################

#### Load Libraries

library(glmnet)
library(arm)
library(BayesTree)
library(randomForest)
library(KRLS)
library(rjava)
library(randomForest)
library(KRLS)
library(rJava)
library(RWeka)

###### GLMnet Hyper Parameters
# To improve runtime, with some loss of precision:
# determine hyperparameters for glmnet models once, instead of reoptimizing hyperparameters via cross validation everytime

# Lasso
fit1<- cv.glmnet(x = Xfull.original, y = Y.original, alpha=1, family='binomial', type='mse')
best.lambda1 = fit1$lambda.min

# Elastic Net, Alpha = .5
fit2<- cv.glmnet(y = Y.original, x= Xfull.original, alpha=0.5, family='binomial', type='mse')
best.lambda2 = fit2$lambda.min

# Elastic Net, Alpha = .25
fit3<- cv.glmnet(y = Y.original, x= Xfull.original, alpha=0.25, family='binomial', type='mse')
best.lambda3 = fit3$lambda.min



########################### Run Bootstrapping #################################

num.boostraps = 500
# For repeatable but still meaningful results, set two seeds, allowing each bootstrap sample to be different but still random
set.seed(10)
seednum = sample(10000,num.boostraps)
results = array(dim = c(1074,9,num.boostraps))
# Measure Run Time
start.time = Sys.time()

debugchecker = numeric()
debugchecker.inner = numeric()
for (i in 1:num.boostraps){
  debugchecker = i
  set.seed(seednum[i])
  bootstramp.sample.indexes = sample(1074,1074,replace = TRUE)
  
  Xfull = Xfull.original[bootstramp.sample.indexes,]
  Y = Y.original[bootstramp.sample.indexes]
  
  # With Cross Validation
  indexes = 1:1074 #963 + 111
  indexes = sample(indexes)
  indexes.first = indexes[1:963]
  indexes.matrix.first = matrix(indexes.first,nrow=9)
  indexes.last = indexes[964:1074]
  indexes.matrix.last = matrix(indexes.last, nrow = 1)
  
  preds = matrix(data = NA, ncol = 9);
  for(j in 1:9){
    debugchecker.inner=j
    temp <-OOSPredicts(Xfull,Y,indexes.matrix.first[j,])
    preds = rbind(preds,temp)
  }
  debugchecker.inner=10
  last<- OOSPredicts(Xfull,Y,indexes.matrix.last)
  preds = rbind(preds,last)
  
  preds.in.order = preds[order(as.numeric(rownames(preds))),]
  # Has a  row of all N/As at end that needs to be deleted
  preds.in.order = preds.in.order[1:1074,]
  results[,,i] = preds.in.order;
}

round_estimates <- function(x){
  if (x > 1 | x < 0){
    return(round(x))}
  else {return(x)}
}

apply(results[,,1], c(1,2), round_estimates)

finish.time = Sys.time()

run.time = finish.time-start.time



########################### Run Superlearners #################################

# Montgomery

#install.packages("EBMAforecast")
library(EBMAforecast)
Names = c("Lasso", "Elastic Net a = .5", "Elastic Net a = .25", "Bayesian GLM", "BART", "Random Forest", "KRLS", "SVM_SMO", "Simple Average")
ForecastData = makeForecastData(.predCalibration = preds.in.order, .outcomeCalibration = Y, .modelNames = Names)
myCal<-calibrateEnsemble(ForecastData)
myCal@modelWeights

# matrix of predicted weights
data_out <- data.frame()
for(i in 1:500){
  Names <- c("Lasso", "Elastic Net a = .5", "Elastic Net a = .25", "Bayesian GLM", "BART", "Random Forest", "KRLS", "SVM_SMO", "Simple Average")
  results_slice <- results[,,i]
  ForecastData <- makeForecastData(.predCalibration = results_slice, .outcomeCalibration = Y, .modelNames = Names)
  myCal <- calibrateEnsemble(ForecastData)
  weights <- myCal@modelWeights
  rbind(data_out, weights)
}


# Original Paper

regress.func <- function(Y, preds.var){
  
  # need to smartly figure out which columns are not NA 
  orgcols <- length(preds.var[1,])
  notNA <- which(!is.na(preds.var[1,]))
  predX <- preds.var[,notNA ]
  
  library(quadprog)
  d.mat <- solve(chol(t(predX)%*%predX))
  a.mat <- cbind(rep(1, ncol(predX)), diag(ncol(predX)))
  b.vec <- c(1, rep(0, ncol(predX)))
  d.vec <- t(Y) %*% predX
  out<- solve.QP(Dmat = d.mat, factorized =TRUE, dvec = d.vec, Amat = a.mat, bvec = b.vec, meq = 1)
  coefs <- rep(NA, orgcols)
  notDel <- c(1:orgcols)[notNA]#[notCor]
  coefs[notDel] <- out$solution
  return(coefs)
}
#####

set.seed(10)
seednum = sample(10000,num.boostraps)
Y.boostrap = matrix(nrow = 1074,ncol = 500)
for (i in 1:num.boostraps){
  set.seed(seednum[i])
  bootstramp.sample.indexes = sample(1074,1074,replace = TRUE)
  
  ordered = bootstramp.sample.indexes[order(as.numeric(bootstramp.sample.indexes))]
  
  Y.boostrap[,i] = Y[ordered]

}


#makes matrix of all predicted weights 

regress.func.results<- matrix(nrow = 500, ncol = 9)
for(i in 1:500){
  regress.func.results[i,] <- regress.func(Y.boostrap[,i], results[,,i])
}



error = numeric(9)
for (i in 1:9){
  error[i] =sd(regress.func.results[,i])
}

#####making the plot

point.estimate <- regress.func(Y, preds.in.order)
plot(point.estimate)
