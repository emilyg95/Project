#making 10 portions 

nrow(svdat)



Xtfull <- model.matrix(~covs*treat)


fit8<- bart(x.train=covsfull_1, y.train=factor(Y), x.test=covstest_1, ndpost=1000, nskip=500, usequants=T)



# Xfull 9/10
#xtest 1/10 

#Def of 1/10

covstest_1 <- covs[c(1:107),]

covstest_2<- covs[c(108:215),]
covstest_3<- covs[c(216:323),]
covstest_4<- covs[c(324:431),]
covstest_5<- covs[c(432:539),]
covstest_6<- covs[c(540:647),]
covstest_7<- covs[c(648:755),]
covstest_8<- covs[c(756:863),]
covstest_9<- covs[c(864:971),]
covstest_10<- covs[c(972:1074),]


# Def of 9/10

covsfull_1 <- covs[-covstest_1,]
covsfull_2 <- covs[-covstest_2,]
covsfull_3 <- covs[-covstest_3,]
covsfull_4 <- covs[-covstest_4,]
covsfull_5 <- covs[-covstest_5,]
covsfull_6 <- covs[-covstest_6,]
covsfull_7 <- covs[-covstest_7,]
covsfull_8 <- covs[-covstest_8,]
covsfull_9 <- covs[-covstest_9,]
covsfull_10 <- covs[-covstest_10,]


####do exact same thing with treat


first<- svdat[c(1:107),]
second<- svdat[c(108:215),]
third<- svdat[c(216:323),]
fourth<- svdat[c(324:431),]
fifth<- svdat[c(432:539),]
sixth<- svdat[c(540:647),]
seventh<- svdat[c(648:755),]
eighth<- svdat[c(756:863),]
ninth<- svdat[c(864:971),]
tenth<- svdat[c(972:1074),]

