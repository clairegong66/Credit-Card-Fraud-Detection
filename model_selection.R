library(dplyr)
#data=read.csv('200variables.csv')


#1.select 115 var after KS/FDR Jan-Oct.
#2.forward stepwise regression--filter first 100
#sum(sapply(data, is.na))
#sum(sapply(data, is.infinite))
#Date_previous_card  >>>>>>>>>factor error
##data$Date_previous_card=is.numeric.Date(data$Date_previous_card)
#library(leaps)#ALL-SUBSETS REGRESSION-NO NEED FOR LASSO
#Actual_max_card_ms3
#fsr_model<- regsubsets(Fraud~.,data[,1:43],nvmax=20,really.big = T, method='forward')#,matrix.logical=TRUE wrongsummary(fsr_model)
#bestvariables <-names(which(summary(fsr_model)$which[20,]))


#importance(fsr_model)
##########
#library(MASS)
#library(DMwR)
#library(SignifReg)
## Fit the full model 
#fsr_model <- lm(Fraud ~., data)
# Stepwise regression model
#fsr_model<- stepAIC(fsr_model, direction ="forward")
#library(relaimpo)
#relImportance <- calc.relimp(fsr_model, type = "lmg", rela = TRUE)


#plot(models)
#3.update data>>update xy
#4.LASSO
#library(glmnet)
#define x/y
#cvfit = cv.glmnet(x, y)#x:input matrix;y:response vector
#lasso_model<-glmnet(x, y, family = "binomial", alpha = 1, lambda =cvfit$lambda.min)
#summary(c)
#predictors(lasso_model)
#####################################################################################
#5.output final variable selecton--20
#prepare final 20 var data as data
################start from here
library(lubridate)
data1=read.csv('vars_final2.csv')
data1$Date=as.Date(data1$Date)
data<-data1[data1$Date<='2010-10-31',]
oot<-data1[data1$Date>'2010-10-31',]

#6.get train/test dataset
library(caret)
trainIndex = createDataPartition(data$Fraud,
                                 p=0.75, list=FALSE,times=1)
data$Fraud=as.numeric(data$Fraud)
oot$Fraud=as.numeric(oot$Fraud)
train =data[trainIndex,]
test =data[-trainIndex,]
#logistic regression
#timestart<-Sys.time()
lg.reg<-glm(Fraud~.-Recnum,data=train,family=binomial)
lg.pre<-predict(lg.reg,test,type='response')
#timeend<-Sys.time()
#rank test based on lg.res, calculate 30% fraud
df<-summary(lg.reg)
#runningtime<-timeend-timestart
#print(runningtime) 
##train FDR
train_res<-data.frame(Recnum=train$Recnum,pro=lg.reg$fitted.values,Fraud=train$Fraud)
num=floor(0.03*nrow(train_res))#2142
train_res<-train_res[order(-train_res$pro),][0:num,]
sum(train_res$Fraud)/sum(train$Fraud)#0.2281735-train
##test FDR
test_res<-data.frame(Recnum=test$Recnum,pro=lg.pre,Fraud=test$Fraud)
num=floor(0.03*nrow(test_res))
test_res<-test_res[order(-test_res$pro),][0:num,]
sum(test_res$Fraud)/sum(test$Fraud)#0.2491961-test
##oot
lg.pre1<-predict(lg.reg,oot,type='response')
oot_res<-data.frame(Recnum=oot$Recnum,pro=lg.pre1,Fraud=oot$Fraud)
num=floor(0.03*nrow(oot_res))
oot_res<-oot_res[order(-oot_res$pro),][0:num,]
sum(oot_res$Fraud)/sum(oot$Fraud)#0.2207084-oot

#randomForest-wrong!!!
library(ranger)

#library(randomForest)#vector too largee
#rf<-randomForest(formula=Fraud~.-Recnum,train,importance=TRUE,mtry = 14,ntree=50)#500
#train_res_rf1<-predict(rf,test,type='prob')[,2]
##solve the memory issue
rf2<-ranger(Fraud~.-Recnum-Date,train,importance = "impurity",num.trees =1000)
train_res_rf<-predict(rf2,train)
#
train_res_rf<-data.frame(Recnum=train$Recnum,pro=train_res_rf[['predictions']],Fraud=as.numeric(train$Fraud))
num=floor(0.03*nrow(train_res_rf))#2142
train_res_rf<-train_res_rf[order(-train_res_rf$pro),][0:num,]

sum(train_res_rf$Fraud)/sum(train$Fraud)#0.3460096-train
#test FDR
test_res_rf<-predict(rf2,test)
#
test_res_rf<-data.frame(Recnum=test$Recnum,pro=test_res_rf[['predictions']],Fraud=as.numeric(test$Fraud))
num=floor(0.03*nrow(test_res_rf))#2142
test_res_rf<-test_res_rf[order(-test_res_rf$pro),][0:num,]

sum(test_res_rf$Fraud)/sum(test$Fraud)#0.3440514
#oot FDR
oot_res_rf<-predict(rf2,oot)
#
oot_res_rf<-data.frame(Recnum=oot$Recnum,pro=oot_res_rf[['predictions']],Fraud=as.numeric(oot$Fraud))
num=floor(0.03*nrow(oot_res_rf))#2142
oot_res_rf<-oot_res_rf[order(-oot_res_rf$pro),][0:num,]

sum(oot_res_rf$Fraud)/sum(oot$Fraud)#0.3297003

#boosting tree
#Gradient boosting tree cannot take factor as Y variable
library(gbm)


gra_boost<- gbm(formula = Fraud~ .-Recnum-Date,
                    distribution ='bernoulli', 
                    data =train,
                    n.trees = 1000)
ntree_opt<- gbm.perf(object =gra_boost, 
                     method = 'OOB', 
                     oobag.curve = TRUE)
gra_boost<- gbm(formula = Fraud~ .-Recnum-Date, 
                distribution ='bernoulli', 
                data =train,
                n.trees =ntree_opt)
gb_pre_train<- predict(object =gra_boost, 
                 newdata =train,type='response',
                 n.trees = ntree_opt)

gb_pre_test<- predict(object =gra_boost, 
                  newdata =test,type='response',
                  n.trees = ntree_opt)

gb_pre_oot<- predict(object =gra_boost, 
                      newdata =oot,type='response',
                      n.trees = ntree_opt)
#train FDR
train_res_bt<-data.frame(Recnum=train$Recnum,pro=gb_pre_train,Fraud=as.numeric(train$Fraud))
num=floor(0.03*nrow(train_res_bt))#2142
train_res_bt<-train_res_bt[order(-train_res_bt$pro),][0:num,]
sum(train_res_bt$Fraud)/sum(train$Fraud)#0.2554901
#test FDR
test_res_bt<-data.frame(Recnum=test$Recnum,pro=gb_pre_test,Fraud=as.numeric(test$Fraud))
num=floor(0.03*nrow(test_res_bt))#2142
test_res_bt<-test_res_bt[order(-test_res_bt$pro),][0:num,]

sum(test_res_bt$Fraud)/sum(test$Fraud)#0.255627
#oot FDR
oot_res_bt<-data.frame(Recnum=oot$Recnum,pro=gb_pre_oot,Fraud=as.numeric(oot$Fraud))
num=floor(0.03*nrow(oot_res_bt))#2142
oot_res_bt<-oot_res_bt[order(-oot_res_bt$pro),][0:num,]

sum(oot_res_bt$Fraud)/sum(oot$Fraud)#0.26703

#neural network
library(nnet)
ideal <- class.ind(train$Fraud)
nn= nnet(train[,-c(1,2,3)],ideal, size=10,  softmax = TRUE)
#train
nn_pre_train<-predict(nn,train[,-c(1,2,3)], type="raw")[,2]
train_res_nn<-data.frame(Recnum=train$Recnum,pro=nn_pre_train,Fraud=as.numeric(train$Fraud))
num=floor(0.03*nrow(train_res_nn))#2142
train_res_nn<-train_res_nn[order(-train_res_nn$pro),][0:num,]
sum(train_res_nn$Fraud)/sum(train$Fraud)#0.1274772
#test
nn_pre_test<-predict(nn,test[,-c(1,2,3)], type="raw")[,2]
test_res_nn<-data.frame(Recnum=test$Recnum,pro=nn_pre_test,Fraud=as.numeric(test$Fraud))
num=floor(0.03*nrow(test_res_nn))#2142
test_res_nn<-test_res_nn[order(-test_res_nn$pro),][0:num,]
sum(test_res_nn$Fraud)/sum(test$Fraud)#0.1125402
#oot
nn_pre_oot<-predict(nn,oot[,-c(1,2,3)], type="raw")[,2]
oot_res_nn<-data.frame(Recnum=oot$Recnum,pro=nn_pre_oot,Fraud=as.numeric(oot$Fraud))
num=floor(0.03*nrow(oot_res_nn))#2142
oot_res_nn<-oot_res_nn[order(-oot_res_nn$pro),][0:num,]
sum(oot_res_nn$Fraud)/sum(oot$Fraud)#0.08446866


for (i in range(21,51)){
  lst.append(i*122)
}

oot_res_rf
write.csv(oot_res_rf,'C:/Users/clair/Desktop/USC/DSO562 Fraud/project2/oot_res_rf.csv')







