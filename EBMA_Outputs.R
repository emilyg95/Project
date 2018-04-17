install.packages("EBMAforecast")
library(EBMAforecast)
Names = c("Lasso", "Elastic Net a = .5", "Elastic Net a = .25", "Bayesian GLM", "BART", "Random Forest", "KRLS", "SVM_SMO", "Simple Average")
ForecastData = makeForecastData(.predCalibration = preds.in.order, .outcomeCalibration = Y, .modelNames = Names)
myCal<-calibrateEnsemble(ForecastData)
myCal@modelWeights
str(myCal)
