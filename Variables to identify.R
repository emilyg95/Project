
#first 4 methods 

Xfull <- model.matrix(~X*treat)
Xtfull <- model.matrix(~Xt*treatt)

library(glmnet)
fit1<- cv.glmnet(y = Y, x= Xfull, alpha=1, family='binomial', type='mse')
#fit2<- cv.glmnet(y = Y, x= Xfull, alpha=0.75, family='binomial', type='mse')
fit3<- cv.glmnet(y = Y, x= Xfull, alpha=0.5, family='binomial', type='mse')
fit4<- cv.glmnet(y = Y, x= Xfull, alpha=0, family='binomial', type='mse')

















part2<- wrap.func(covs, approve_bi, treats, covs, approve_bi, treats, speed=F)
#Do above function, indiviudally, for each test

#Elements that go into function 

covs<- cbind(dem, rep, lib, cons)
colnames(covs)<- c('Dem', 'Rep', 'Lib', 'Cons')

lib<- ifelse(subset$Q78==4 , 1, 0)
cons<- ifelse(subset$Q78==2, 1, 0)
dem<- ifelse(svdat$pid3l=='Dem', 1, 0)
dem[which(is.na(dem))]<- 0
rep<- ifelse(svdat$pid3l=='Rep', 1, 0)
rep[which(is.na(rep))]<- 0
lib[which(is.na(lib))]<- 0
cons[which(is.na(cons))]<- 0

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