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


### excluding fit 7

Y.final<- approve_bi<- ifelse(svdat$approval<3, 1, 0)#line 292 of rep code 

excluding_8<- results[,c(1:6,8:9),]

set.seed(10)
seednum = sample(10000,num.boostraps)
Y.boostrap = matrix(nrow = 1074,ncol = 500)
for (i in 1:num.boostraps){
  set.seed(seednum[i])
  bootstramp.sample.indexes = sample(1074,1074,replace = TRUE)
  ordered = bootstramp.sample.indexes[order(as.numeric(bootstramp.sample.indexes))]
  Y.boostrap[,i] = Y.final[ordered]
}


######## original paper ######## 

regress.func.results<- matrix(nrow = 500, ncol = 8)
for(i in 1:500){
  regress.func.results[i,] <- regress.func(Y.boostrap[,i], excluding_8[,,i])
}


mean.coefs = numeric(8)
error = numeric(8)
for (i in 1:8){
  error[i] =sd(regress.func.results[,i])
  mean.coefs[i] = mean(regress.func.results[,i])
}

mean.coefs


######## Montgomery ######## 

#install.packages("EBMAforecast")
library(EBMAforecast)


round_estimates <- function(x){
  if (x >= 1 | x <= 0){
    return(round(x)+0.001)}
  else {return(x)}
}


rounded_output <- array(dim = c(1074,8,500))
for (i in 1:length(excluding_8)){
  rounded_output[i] <- round_estimates(excluding_8[i])
}
rounded_output


montgomery_results <- data.frame()
for(i in 1:500){
  Names <- c("Lasso", "Elastic Net a = .5", "Elastic Net a = .25", "Bayesian GLM", "BART", "Random Forest", "KRLS", "Simple Average")
  results_slice <- rounded_output[,,i]
  ForecastData <- makeForecastData(.predCalibration = results_slice, .outcomeCalibration = Y.boostrap[,i], .modelNames = Names)
  myCal <- calibrateEnsemble(ForecastData)
  weights <- myCal@modelWeights
  montgomery_results <- rbind(montgomery_results, weights)
}
colnames(montgomery_results) <- Names
montgomery_results


mean.coefs = numeric(8)
error = numeric(8)
for (i in 1:8){
  error[i] =sd(montgomery_results[,i])
  mean.coefs[i] = mean(montgomery_results[,i])
}

mean.coefs


#### all models ###############


Y.final<- approve_bi<- ifelse(svdat$approval<3, 1, 0)#line 292 of rep code 

set.seed(10)
seednum = sample(10000,num.boostraps)
Y.boostrap = matrix(nrow = 1074,ncol = 500)
for (i in 1:num.boostraps){
  set.seed(seednum[i])
  bootstramp.sample.indexes = sample(1074,1074,replace = TRUE)
  
  ordered = bootstramp.sample.indexes[order(as.numeric(bootstramp.sample.indexes))]
  
  Y.boostrap[,i] = Y.final[ordered]
  
}


######## original paper ######## 


regress.func.results<- matrix(nrow = 500, ncol = 9)
for(i in 1:500){
  regress.func.results[i,] <- regress.func(Y.boostrap[,i], results[,,i])
}


mean.coefs = numeric(9)
error = numeric(9)
for (i in 1:9){
  error[i] =sd(regress.func.results[,i])
  mean.coefs[i] = mean(regress.func.results[,i])
}


######## Montgomery ######## 

dim(results)

rounded_output <- array(dim = c(1074,9,500))
for (i in 1:length(results)){
  rounded_output[i] <- round_estimates(results[i])
}
rounded_output

montgomery_results <- data.frame()
for(i in 1:500){
  Names <- c("Lasso", "Elastic Net a = .5", "Elastic Net a = .25", "Bayesian GLM", "BART", "Random Forest", "KRLS", "Simple Average")
  results_slice <- rounded_output[,,i]
  ForecastData <- makeForecastData(.predCalibration = results_slice, .outcomeCalibration = Y.boostrap[,i], .modelNames = Names)
  myCal <- calibrateEnsemble(ForecastData)
  weights <- myCal@modelWeights
  rbind(montgomery_results, weights)
}

mean.coefs = numeric(9)
error = numeric(9)
for (i in 1:9){
  error[i] =sd(montgomery_results[,i])
  mean.coefs[i] = mean(montgomery_results[,i])
}

mean.coefs


##################### Plotting all models

######## original paper ######## 

plotting_data<- as.data.frame(mean.coefs)
error

plotting_data<- as.data.frame(mean.coefs)
upper<-(mean.coefs+ 1.96*error)
lower<- (mean.coefs - 1.96*error)

ggplot(data = plotting_data, mapping = aes(x = plotting_data, y = plotting_data$mean.coefs)) +
  geom_point()

ggplot(plotting_data, aes(x = c(1:9), y = plotting_data$mean.coefs)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = upper, ymin = lower))
