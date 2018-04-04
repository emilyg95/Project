RMSEforModel = function(x,y){
 # NOTE: THIS IS NOT DOING CROSS-VALIDATION right now (for most models)
    
  test.indexes = sample(1074,107)
  x.train = x[-test.indexes]
  x.test = x[test.indexes]
  y.train = y[-test.indexes]
  y.test = y[test.indexes]
    
  library(glmnet)
  
  ##Now predict them
  logist<- function(x){
    ff<- 1/(1 + exp(-x))
    return(ff)
  }
  
  # Lasso
  fit1<- cv.glmnet(x = x.train, y = y.train, alpha=1, family='binomial', type='mse')
  best.lambda = fit1$lambda.min
  fit1.predict = predict(fit1, s= best.lambda, newx = x.test)
  #fit1.RMSE = sqrt(mean((fit1.predict-y.test)^2))
  fit1.logistPred = logist(fit1.predict)
  fit1.logistPred.RMSE = sqrt(mean((fit1.logistPred-y.test)^2))
  
  
  # Elastic Net, Alpha = .5
  fit2<- cv.glmnet(y = y.train, x= x.train, alpha=0.5, family='binomial', type='mse')
  best.lambda = fit2$lambda.min
  fit2.predict = predict(fit2, s= best.lambda, newx = x.test)
  #fit2.RMSE = sqrt(mean((fit2.predict-y.test)^2))
  fit2.logistPred = logist(fit2.predict)
  fit2.logistPred.RMSE = sqrt(mean((fit2.logistPred-y.test)^2))

  # Elastic Net, Alpha = .25
  fit3<- cv.glmnet(y = y.train, x= x.train, alpha=0.25, family='binomial', type='mse')
  best.lambda = fit3$lambda.min
  fit3.predict = predict(fit3, s= best.lambda, newx = x.test)
  #fit3.RMSE = sqrt(mean((fit3.predict-y.test)^2))
  fit3.logistPred = logist(fit3.predict)
  fit3.logistPred.RMSE = sqrt(mean((fit3.logistPred-y.test)^2))
  
  # Fit4 not published in paper -- so not doing it
  #fit4<- cv.glmnet(y = Y, x= Xfull, alpha=0, family='binomial', type='mse')
  
  ## Skipping FindIt since documentation changed (per)

  # Bayesian GLM -- Revisit -- probably not working right
  #Not sure if I should directly predict via linear estiamtion, or use a logistic regression
  # Prediction Function Doesn't really work for this
  #install.packages("arm")
  library(arm)
  fit6<- bayesglm(y.train~x.train-1, family=binomial(link=logit))
  #fit6.predict = x.test%*%fit6$coefficients
  fit6.predict2 = logist(x.test%*%fit6$coefficients)
  #fit6.1 = bayesglm(y~x-1, family=binomial(link=logit))
  #fit6.RMSE = sqrt(mean(resid(fit6.1)^2))
  fit6.logistPred.RMSE = sqrt(mean((fit6.predict2-y.test)^2))
  
  # Fit 7 = Boosted Trees is not published ### SKipping
  
  # Fit 8 = BART
  library(BayesTree)
  fit8<- bart(x.train=x.train, y.train=factor(y.train), x.test=x.test, ndpost=1000, nskip=500, usequants=T)
  fit8.pred<- pnorm(apply(fit8$yhat.test, 2, mean))
  fit8.rmse = sqrt(mean((fit8.pred-y.train)^2))
  
  # Fit 9  = RandomForest
  library(randomForest)
  fit9<- randomForest(y = factor(y.train), x = x.train)
  X.test.forest = X.test
  `colnames<-`(X.test,colnames(x.train))
  #Only works for X.test??
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
  fit12 <- SMO(y ~ ., data = data.frame(Y=factor(y),x), control = Weka_control(M = TRUE ) , subset = subset.index)
  #fit12 <- SMO(Y ~ ., data = data.frame(Y=factor(Y),Xfull), control = Weka_control(M = TRUE ) , subset = ((1:1074)[-test.indexes]))
  fit12.predict =predict(fit12, newdata= data.frame(x[test.indexes,]), type="probability" )[,2] 
  fit12.RMSE = sqrt(mean((fit12.predict-y.test)^2))
  
  
  # Fit 13 = Simple Mean
  fit13.predict = mean(y.train)
  fit13.RSME= sqrt(mean((fit13.predict-y.test)^2))
  
  models = c("Lasso", "Elastic Net (a = .5)","Elastic Net (a = .25)", "Bayesian GLM", "BART", "Random Forest", "KRLS", "SVM-SMO", "Simple Average")
  RMSE.all = c(fit1.logistPred.RMSE,fit2.logistPred.RMSE,fit3.logistPred.RMSE,fit6.logistPred.RMSE,fit8.rmse,fit9.rmse,fit11.rmse,fit12.RMSE, fit13.RSME)
  
  return(data.frame(models,RMSE.all))
  
}

