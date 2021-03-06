% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/EBMApredict.R, R/predictLogit.R, R/predictNormal.R
\docType{methods}
\name{EBMApredict}
\alias{EBMApredict}
\alias{EBMApredict,ForecastData-method}
\alias{FDatFitLogit-method}
\alias{FDatFitNormal-method}
\alias{ForecastDataLogit-method}
\alias{prediction}
\alias{prediction,}
\alias{prediction,FDatFitLogit-method}
\alias{prediction,FDatFitNormal-method}
\alias{prediction,ForecastDataNormal-method}
\title{EBMApredict}
\usage{
EBMApredict(EBMAmodel, Predictions, Outcome = NULL, ...)

\S4method{EBMApredict}{ForecastData}(EBMAmodel, Predictions, Outcome = NULL,
  ...)

prediction(EBMAmodel, Predictions, Outcome, ...)

\S4method{prediction}{FDatFitLogit}(EBMAmodel, Predictions, Outcome, ...)

\S4method{prediction}{FDatFitNormal}(EBMAmodel, Predictions, Outcome = c(),
  ...)
}
\arguments{
\item{EBMAmodel}{Output of estimated EBMA model}

\item{Predictions}{A matrix with a column for each component model's predictions.}

\item{Outcome}{An optional vector containing the true values of the dependent variable for all observations in the test period.}

\item{...}{Not implemented}
}
\value{
Returns a data of class 'FDatFitLogit' or FDatFitNormal, a subclass of 'ForecastData', with the following slots:
\item{predTest}{A matrix containing the predictions of all component models and the EBMA model for all observations in the test period.}#' \item{period}{The period, "calibration" or "test", for which the statistics were calculated.}
\item{outcomeTest}{An optional vector containing the true values of the dependent variable for all observations in the test period.}
\item{modelNames}{A character vector containing the names of all component models.  If no model names are specified, names will be assigned automatically.}
\item{modelWeights}{A vector containing the model weights assigned to each model.}
}
\description{
Function allows users to create new predictions given an already estimated EBMA model
This function produces predictions based on EBMA model weights and component model predictions.
}
\author{
Michael D. Ward <\email{michael.d.ward@duke.edu}> and Jacob M. Montgomery <\email{jacob.montgomery@wustl.edu}> and Florian M. Hollenbach <\email{florian.hollenbach@tamu.edu}>
}
\references{
Montgomery, Jacob M., Florian M. Hollenbach and Michael D. Ward. (2015). Calibrating ensemble forecasting models with sparse data in the social sciences.   \emph{International Journal of Forecasting}. In Press.

Montgomery, Jacob M., Florian M. Hollenbach and Michael D. Ward. (2012). Improving Predictions Using Ensemble Bayesian Model Averaging. \emph{Political Analysis}. \bold{20}: 271-291.
}

