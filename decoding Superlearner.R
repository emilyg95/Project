supLearnFit <- function(X, treats, Y, nfold = 10, speed=T){
  
  # first straighten out data:
  if(class(X)=='matrix' | class(X)=='data.frame' & any(is.na(X)) ==F){
    toInclude <- complete.cases(cbind(X,treats,Y))	
    X <- X[toInclude,]}
  if(class(X)!= 'matrix' & class(X) != 'data.frame' & any(is.na(X))==F){		
    toInclude <- complete.cases(cbind(X,treats,Y))
    X<- X[toinclude]}
  if(any(is.na(X))==T){
    toInclude<- complete.cases(cbind(treats, Y))
  } 			
  
  
  Y <- Y[toInclude]
  if(class(treats)=='matrix'|class(treats)=='data.frame'){
    treats <- treats[toInclude,]
  }
  if(class(treats)!= 'matrix' & class(treats)!= 'data.frame'){
    treats<- treats[toInclude]}
  
  #Xfull <- model.matrix(~X*treats)
  Xfull<- X
  
  ##this is the quadratic programming solution to the weights on the forecasters
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
  
  ## 10fcv 
  fold <- sample(nfold, nrow(X), replace = T)
  preds.var <- matrix(NA, nrow=nrow(X),ncol=12)
  
  wrapresults <- list()
  # This will store each CV pred fold
  for (z in sort(unique(fold))) {
    print(cat(paste('Fold', z, sep = ' ' ), '\n'))
    X.train<- Xfull[fold != z,]
    X.test <- Xfull[fold == z,]
    Y.train <- Y[fold != z]
    Y.test <- Y[fold == z]
    if(is.null(ncol(treats))==F){
      treats.train <- treats[fold != z,]
      treats.test <- treats[fold == z,]
    }
    if(is.null(ncol(treats))==T){
      treats.train <- treats[fold != z]
      treats.test <- treats[fold == z]}
    
    
    #		debugonce(wrap.func)
    wrapresults[[z]] <- wrap.func(X.train, Y.train, 
                                  treats.train, X.test, Y.test, treats.test, speed=speed)[[1]]
    preds.var[fold == z,] <- wrapresults[[z]]
  }
  
  # Now run the function:
  store <- regress.func(Y, preds.var)
  store
  names(store) <- c('lasso', 'e_net_0.75', 'e_net_0.5', 'e_net_0.25', 'FindIt', 
                    'BayesGLM', 'GLMBoost', 'BART', 'RandomForest', 'glm', 'KRLS', 'SVM-SMO')
  print(round(store, 4))
  return( list(store, preds.var) )
}

supLearnFit(covs, treats, approve_bi, 10, F)

X = covs
treats = treats
Y = approve_bi


fold <- sample(10, nrow(X), replace = T)
preds.var <- list_test



#install.packages("quadprog")
regress.func <- function(Y, preds.var){
  
  # need to smartly figure out which columns are not NA 
  orgcols <- length(preds.var)
  notNA <- which(!is.na(preds.var))
  predX <- preds.var[notNA ]
  
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


store <- regress.func(Y, preds.var)
store




fold <- sample(10, nrow(X), replace = T)
preds.var <- matrix(NA, nrow=nrow(X),ncol=12)


for (z in sort(unique(fold))) {
  print(cat(paste('Fold', z, sep = ' ' ), '\n'))
  X.train<- Xfull[fold != z,]
  X.test <- Xfull[fold == z,]
  Y.train <- Y[fold != z]
  Y.test <- Y[fold == z]
  if(is.null(ncol(treats))==F){
    treats.train <- treats[fold != z,]
    treats.test <- treats[fold == z,]
  }
  if(is.null(ncol(treats))==T){
    treats.train <- treats[fold != z]
    treats.test <- treats[fold == z]
}
}
