
#What we are doing 

part2<- wrap.func(covs, approve_bi, treats, covs, approve_bi, treats, speed=F)
#Do above function, indiviudally, for each test

#Elements that go into function 

covs<- cbind(dem, rep, lib, cons)
colnames(covs)<- c('Dem', 'Rep', 'Lib', 'Cons')

approve_bi<- ifelse(svdat$approval<3, 1, 0)

fit1<- cv.glmnet(y = Y, x= Xfull, alpha=1, family='binomial', type='mse')

treats<- cbind(type.mat.final, num.mat.final[,1], stage.mat.final[,1:2],party.mat.final[,1], 
               along.mat.final[,1:2], type.mat.final[,1:5]*num.mat.final[,1], type.mat.final[,1:5]*stage.mat.final[,1], 
               type.mat.final[,1:5]*stage.mat.final[,2], type.mat.final[,1:5]*party.mat.final[,1], type.mat.final[,1:5]*along.mat.final[,1],
               type.mat.final[,1:5]*along.mat.final[,2], num.mat.final[,1]*stage.mat.final[,1], num.mat.final[,1]*stage.mat.final[,2], 
               num.mat.final[,1]*party.mat.final[,1], num.mat.final[,1]*along.mat.final[,1], num.mat.final[,1]*along.mat.final[,2],
               stage.mat.final[,1:2]*party.mat.final[,1], stage.mat.final[,1:2]*along.mat.final[,1], 
               stage.mat.final[,1:2]*along.mat.final[,2], party.mat.final[,1]*along.mat.final[,1], party.mat.final[,1]*along.mat.final[,2] )




#What to do, define every variable in this file 