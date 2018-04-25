RMSEforModel = function(x,y, test.indexes = sample(length(y),as.integer(length(y)/10))){
 # NOTE: THIS IS NOT DOING CROSS-VALIDATION right now (for most models)
    
  #test.indexes = sample(1074,107)
  x.train = x[-test.indexes,]
  x.test = x[test.indexes,]
  y.train = y[-test.indexes]
  y.test = y[test.indexes]
    
  library(glmnet)
  
  ##Now predict them
  logist<- function(x){
    ff<- 1/(1 + exp(-x))
    return(ff)
  }
  
  ##### Take out this Cross-Validation -- just do CV onces for everything to make run faster
  # Lasso
  fit1<- cv.glmnet(x = x.train, y = y.train, alpha=1, family='binomial', type='mse')
  best.lambda = fit1$lambda.min
  fit1.predict = predict(fit1, s= best.lambda, newx = x.test)
  fit1.logistPred = logist(fit1.predict)
  fit1.logistPred.RMSE = sqrt(mean((fit1.logistPred-y.test)^2))
  
  
  # Elastic Net, Alpha = .5
  fit2<- cv.glmnet(y = y.train, x= x.train, alpha=0.5, family='binomial', type='mse')
  best.lambda = fit2$lambda.min
  fit2.predict = predict(fit2, s= best.lambda, newx = x.test)
  fit2.logistPred = logist(fit2.predict)
  fit2.logistPred.RMSE = sqrt(mean((fit2.logistPred-y.test)^2))

  # Elastic Net, Alpha = .25
  fit3<- cv.glmnet(y = y.train, x= x.train, alpha=0.25, family='binomial', type='mse')
  best.lambda = fit3$lambda.min
  fit3.predict = predict(fit3, s= best.lambda, newx = x.test)
  fit3.logistPred = logist(fit3.predict)
  fit3.logistPred.RMSE = sqrt(mean((fit3.logistPred-y.test)^2))
  
  # Fit4 not published in paper -- so not doing it
  #fit4<- cv.glmnet(y = Y, x= Xfull, alpha=0, family='binomial', type='mse')
  
  ## Skipping FindIt since documentation changed (per)

  # Bayesian GLM -- Revisit -- probably not working right
  library(arm)
  fit6<- bayesglm(y.train~x.train-1, family=binomial(link=logit))
  fit6.predict = logist(x.test%*%fit6$coefficients)
  fit6.logistPred.RMSE = sqrt(mean((fit6.predict-y.test)^2))
  
  # Fit 7 = Boosted Trees is not published ### SKipping
  
  # Fit 8 = BART
  library(BayesTree)
  fit8<- bart(x.train=x.train, y.train=factor(y.train), x.test=x.test, ndpost=1000, nskip=500, usequants=T)
  fit8.pred<- pnorm(apply(fit8$yhat.test, 2, mean))
  fit8.rmse = sqrt(mean((fit8.pred-y.test)^2))
  
  # Fit 9  = RandomForest
  library(randomForest)
  fit9<- randomForest(y = factor(y.train), x = x.train)
  X.test.forest = x.test
  `colnames<-`(X.test.forest,colnames(x.train))
  fit9.pred.raw = predict(fit9,newdata = X.test.forest,type = "prob" )
  fit9.pred = fit9.pred.raw[,2]
  fit9.rmse = sqrt(mean((fit9.pred-y.test)^2))
  
  
  # Fit 10 = Skipped in Paper and SLF_round2 code
  
  # Fit 11 = KRLS
  library(KRLS)
  fit11<- krls(X = x.train[,-1], y = y.train, derivative=F)
  fit11.predict = predict(fit11,newdata = x.test[,-1])$fit
  fit11.rmse = sqrt(mean((fit11.predict-y.test)^2))
  
  # Fit 12 = SVM-SMO
  library(rJava)
  .jinit(parameters="-Xmx4g")
  library(RWeka)
  subset.index = (1:length(y))[-test.indexes]
  fit12 <- SMO(y ~ ., data = data.frame(y=factor(y),x), control = Weka_control(M = TRUE ) , subset = subset.index)
  fit12.predict =predict(fit12, newdata= data.frame(x[test.indexes,]), type="probability" )[,2]
  fit12.RMSE = sqrt(mean((fit12.predict-y.test)^2))

  
  # Fit 13 = Simple Mean
  fit13.predict = mean(y.train)
  fit13.RSME= sqrt(mean((fit13.predict-y.test)^2))
  

  # W/O Fit12 (Req Java)
  # Preds.All = cbind(fit1.logistPred,fit2.logistPred,fit3.logistPred,fit6.predict,fit8.pred,fit9.pred,fit11.predict,  fit13.predict)#fit12
  #models = rbind("Lasso", "Elastic Net (a = .5)","Elastic Net (a = .25)", "Bayesian GLM", "BART", "Random Forest", "KRLS" , "Simple Average")#"SVM_SMO"
  
  # W/ Fit 12
  Preds.All = cbind(fit1.logistPred,fit2.logistPred,fit3.logistPred,fit6.predict,fit8.pred,fit9.pred,fit11.predict, fit12.predict, fit13.predict)
  models = rbind("Lasso", "Elastic Net (a = .5)","Elastic Net (a = .25)", "Bayesian GLM", "BART", "Random Forest", "KRLS" ,"SVM_SMO", "Simple Average")
  
  #RMSE.all = rbind(fit1.logistPred.RMSE,fit2.logistPred.RMSE,fit3.logistPred.RMSE,fit6.logistPred.RMSE,fit8.rmse,fit9.rmse,fit11.rmse,fit12.RMSE, fit13.RSME)
  colnames(Preds.All) = models
  #return(data.frame(RMSE.all))
  return(Preds.All)
}




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

# # One Query
# df<-RMSEforModel(Xfull,Y)

# Slightly wrong
# set.seed(100)
# # With Cross Validation
# indexes = 1:1074
# indexes = sample(indexes)
# indexes.matrix = matrix(indexes,nrow=10)
# 
# 
# #Makes data frame with all 10 RMSE
# preds = matrix(data = NA, ncol = 9);
# for(i in 1:10){
#   # df$i <-RMSEforModel(Xfull,Y,indexes.matrix[i,])
#   temp <-RMSEforModel(Xfull,Y,indexes.matrix[i,])
#   preds = rbind(preds,temp)
# }
# preds.in.order = preds[order(as.numeric(rownames(preds))),]


numBoostraps = 

set.seed(100)
# With Cross Validation
indexes = 1:1074 #963 + 111
indexes = sample(indexes)
indexes.first = indexes[1:963]
indexes.matrix.first = matrix(indexes.first,nrow=9)
indexes.last = indexes[964:1074]
indexes.matrix.last = matrix(indexes.last, nrow = 1)
## If Java
preds = matrix(data = NA, ncol = 9);
## If No Java
# preds = matrix(data = NA, ncol = 8);
for(i in 1:9){
  # df$i <-RMSEforModel(Xfull,Y,indexes.matrix[i,])
  temp <-RMSEforModel(Xfull,Y,indexes.matrix.first[i,])
  preds = rbind(preds,temp)
}

last<- RMSEforModel(Xfull,Y,indexes.matrix.last)
preds = rbind(preds,last)

preds.in.order = preds[order(as.numeric(rownames(preds))),]








