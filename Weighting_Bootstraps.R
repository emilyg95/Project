install.packages("EBMAforecast")
library(EBMAforecast)

data_out <- data.frame()
for(i in 1:500){
Names <- c("Lasso", "Elastic Net a = .5", "Elastic Net a = .25", "Bayesian GLM", "BART", "Random Forest", "KRLS", "SVM_SMO", "Simple Average")
results_slice <- results[,,i]
ForecastData <- makeForecastData(.predCalibration = results_slice, .outcomeCalibration = Y, .modelNames = Names)
myCal <- calibrateEnsemble(ForecastData)
weights <- myCal@modelWeights
rbind(data_out, weights)
}

min(results[,,1])


