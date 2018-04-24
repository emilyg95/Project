install.packages("plyr")
library("plyr")

install.packages("EBMAforecast")
library(EBMAforecast)

Names = c("Lasso", "Elastic Net a = .5", "Elastic Net a = .25", "Bayesian GLM", "BART", "Random Forest", "KRLS", "SVM_SMO", "Simple Average")
aaply(results, 3, makeForecastData, .outcomeCalibration = Y, .modelNames = Names)

results[,,3]
dim(results)

data_out <- data.frame()
for(i in 1:500){
Names <- c("Lasso", "Elastic Net a = .5", "Elastic Net a = .25", "Bayesian GLM", "BART", "Random Forest", "KRLS", "SVM_SMO", "Simple Average")
results_slice <- results[,,i]
ForecastData <- makeForecastData(.predCalibration = results_slice, .outcomeCalibration = Y, .modelNames = Names)
myCal <- calibrateEnsemble(ForecastData)
weights <- myCal@modelWeights
rbind(data_out, weights)
}



