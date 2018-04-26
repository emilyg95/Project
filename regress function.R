regress.func <- function(Y, preds.var){
  
  # need to smartly figure out which columns are not NA 
  orgcols <- length(preds.var[1,])
  notNA <- which(!is.na(preds.var[1,]))
  predX <- preds.var[,notNA ]
  predX <-predX[1:1074,]#had to het rid of NA's. Also make same number of rows as Y
  
  library(quadprog)
  d.mat <- solve(chol(t(predX)%*%predX))
  a.mat <- cbind(rep(1, ncol(predX)), diag(ncol(predX)))
  b.vec <- c(1, rep(0, ncol(predX)))
  d.vec <- t(Y) %*% predX #doesn't like diff number of rows
  out<- solve.QP(Dmat = d.mat, factorized =TRUE, dvec = d.vec, Amat = a.mat, bvec = b.vec, meq = 1)
  coefs <- rep(NA, orgcols)
  notDel <- c(1:orgcols)[notNA]#[notCor]
  coefs[notDel] <- out$solution
  return(coefs)
}

regress.func(Y, preds.in.order)

# results: 
#3.660097e-01  9.006526e-17  0.000000e+00 -5.683887e-17  3.122957e-01  3.097471e-01  1.974784e-18  1.194746e-02

x<- as.data.frame(results[,,1])
 

