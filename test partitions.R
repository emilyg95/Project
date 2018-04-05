##making 10 portions 

#random ordering

Y = ifelse(svdat$approval<3, 1, 0)
data <- cbind(covs, treat, Y)
set.seed(100)
data_r <- cbind(data, "random" = sample(1:1074, 1074))
data_r <- as.data.frame(data_r)
data_r <- data_r[order(data_r$random),] 
data_r$random <- NULL
data_r <- data
covs <- data[,1:4]
treat <- data[,5:59]
Y  <- data[,60]


#automated sampling

covs_sample <- sample.int(n = nrow(covs_r), size = floor(.9*nrow(covs_r)), replace = F)
covs_train <- covs_r[covs_sample, ]
covs_test  <- covs_r[-covs_sample, ]

##############

datatest_1 <- data[c(1:107),]
datatest_2<- data[c(108:215),]
datatest_3<- data[c(216:323),]
datatest_4<- data[c(324:431),]
datatest_5<- data[c(432:539),]
datatest_6<- data[c(540:647),]
datatest_7<- data[c(648:755),]
datatest_8<- data[c(756:863),]
datatest_9<- data[c(864:971),]
datatest_10<- data[c(972:1074),]


# Def of 9/10 for covs

datafull_1 <- data[-(1:107),]
datafull_2 <- data[-c(108:215),]
datafull_3 <- data[-c(216:323),]
datafull_4 <- data[-c(324:431),]
datafull_5 <- data[-c(432:539),]
datafull_6 <- data[-c(540:647),]
datafull_7 <- data[-c(648:755),]
datafull_8 <- data[-c(756:863),]
datafull_9 <- data[-c(864:971),]
datafull_10 <- data[-c(972:1074),]









#Def of 1/10 for covs

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


# Def of 9/10 for covs

covsfull_1 <- covs[-(1:107),]
covsfull_2 <- covs[-c(108:215),]
covsfull_3 <- covs[-c(216:323),]
covsfull_4 <- covs[-c(324:431),]
covsfull_5 <- covs[-c(432:539),]
covsfull_6 <- covs[-c(540:647),]
covsfull_7 <- covs[-c(648:755),]
covsfull_8 <- covs[-c(756:863),]
covsfull_9 <- covs[-c(864:971),]
covsfull_10 <- covs[-c(972:1074),]


####def of 1/10 for treat

treattest_1 <- treat[c(1:107),]
treattest_2<- treat[c(108:215),]
treattest_3<- treat[c(216:323),]
treattest_4<-treat[c(324:431),]
treattest_5<- treat[c(432:539),]
treattest_6<- treat[c(540:647),]
treattest_7<- treat[c(648:755),]
treattest_8<- treat[c(756:863),]
treattest_9<- treat[c(864:971),]
treattest_10<- treat[c(972:1074),]


# Def of 9/10 for treat

treatfull_1 <-treat[-c(1:107),]
treatfull_2 <- treat[-c(108:215),]
treatfull_3 <- treat[-c(216:323),]
treatfull_4 <- treat[-c(324:431),]
treatfull_5 <- treat[-c(432:539),]
treatfull_6 <- treat[-c(540:647),]
treatfull_7 <- treat[-c(648:755),]
treatfull_8 <- treat[-c(756:863),]
treatfull_9 <- treat[-c(864:971),]
treatfull_10 <- treat[-c(972:1074),]


#################
#def y in portions 

Ytest_1<- ifelse(svdat[c(1:107),]$approval<3, 1, 0)
Ytest_2<- ifelse(svdat[c(108:215),]$approval<3, 1, 0)
Ytest_3<- ifelse(svdat[c(216:323),]$approval<3, 1, 0)
Ytest_4<- ifelse(svdat[c(324:431),]$approval<3, 1, 0)
Ytest_5<- ifelse(svdat[c(432:539),]$approval<3, 1, 0)
Ytest_6<- ifelse(svdat[c(540:647),]$approval<3, 1, 0)
Ytest_7<- ifelse(svdat[c(648:755),]$approval<3, 1, 0)
Ytest_8<- ifelse(svdat[c(756:863),]$approval<3, 1, 0)
Ytest_9<- ifelse(svdat[c(864:971),]$approval<3, 1, 0)
Ytest_10<- ifelse(svdat[c(972:1074),]$approval<3, 1, 0)


Yfull_1<- ifelse(svdat[-c(0:107),]$approval<3, 1, 0)
Yfull_2<- ifelse(svdat[-c(108:215),]$approval<3, 1, 0)
Yfull_3<- ifelse(svdat[-c(216:323),]$approval<3, 1, 0)
Yfull_4<- ifelse(svdat[-c(324:431),]$approval<3, 1, 0)
Yfull_5<- ifelse(svdat[-c(432:539),]$approval<3, 1, 0)
Yfull_6<- ifelse(svdat[-c(540:647),]$approval<3, 1, 0)
Yfull_7<- ifelse(svdat[-c(648:755),]$approval<3, 1, 0)
Yfull_8<- ifelse(svdat[-c(756:863),]$approval<3, 1, 0)
Yfull_9<- ifelse(svdat[-c(864:971),]$approval<3, 1, 0)
Yfull_10<- ifelse(svdat[-c(972:1074),]$approval<3, 1, 0)




#################
#full data subsets

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

