data =read.table("kaggle.txt")
#saveRDS(data, "tables/data.rds")
#data = data.frame(readRDS("data.rds"))

colnames(data)[1] <- "Ind"
colnames(data)[2] <- "Default"
colnames(data)[3] <- "UnsecuredLines"
colnames(data)[4] <- "Age"
colnames(data)[5] <- "UnderSixtyDaysPast"
colnames(data)[6] <- "DebtRatio"
colnames(data)[7] <- "MonthlyIncome"
colnames(data)[8] <- "CreditLinesAndLoans"
colnames(data)[9] <- "NinetyDaysLate"
colnames(data)[10] <- "RealEstateLoansOrLines"
colnames(data)[11] <- "UnderNinetyDaysPastDue"
colnames(data)[12] <- "NumberOfDependents"

#Les Traitements
mediane = readRDS("tables/mediane.rds")
winsorize = readRDS("tables/winsorize.rds")
sampled = readRDS("tables/train_sampled.rds")
new_variables = readRDS("tables/new_variables.rds")

#Les Tables
train_new = readRDS("tables/train_new.rds")
test_new = readRDS("tables/test_new.rds")
train_sampled = readRDS("tables/train_sampled.rds")
test = readRDS("tables/test.rds")
sens = readRDS("tables/sens.rds")
X = readRDS("tables/X.rds")
X_test = readRDS("tables/X_test.rds")
Y_test = test_new$Default

df = winsorize
attach(df)
library(corrplot)
library(precrec)
library(forecast)

#Régression Logistique
reg = glm(Default~Age+Age2+CreditLinesAndLoans+DebtRatio+MonthlyIncome+MonthlyIncome2+NinetyDaysLate+NumberOfDependents+RealEstateLoansOrLines+UnderNinetyDaysPastDue+UnderSixtyDaysPast+UnsecuredLines+DependentbyAge+CreditbyRealEstate,data=train_new, family="binomial")
logreg_pred <- predict(reg, type = "response", newdata = test_new)
prediction = ifelse(logreg_pred>0.5,1,0)
tbl = table(test_new$Default, logreg_pred> 0.5)
P_log = prediction
Confusion_Log_Reg = readRDS("tables/Confusion_Log_Reg.rds")
Plot_Log_Reg = readRDS("tables/Plot_Log_Reg.rds")

#RIDGE
library(glmnet)
library(tibble)
library(cvms)
library(precrec)
library(forecast)


#Adaptive Lasso
X_new = train_new[,-1]
log_reg = glm(train_new$Default~., data=X_new,family="binomial")
coef_log = log_reg$coefficients
coef_log[is.na(coef_log)]=0

#Elastic Net
library(caret)
lambda_opt = 0.0002008347
alpha_opt = 1

#CART
library(rpart)
library(rpart.plot)
arbre1 =  rpart(as.factor(train_new$Default)~.,data=train_new, control = rpart.control(cp = 0.001)) 
arbre1.prune = prune(arbre1, cp=0.001015950)
pred_elag = predict(arbre1.prune,newdata=test_new,type="class")

#BAGGING
library(ipred)
train_Bag = readRDS("tables/train_Bag.rds")
train_Bag$Default = as.factor(train_Bag$Default)

#Random Forest
library('randomForest')
library(caret)
train_sampled_rf = train_sampled
train_sampled_rf["Age2"]=train_sampled_rf["Age"]^2
train_sampled_rf["Income2"]=train_sampled_rf$MonthlyIncome^2
test_rf = test
test_rf["Age2"] = test_rf$Age^2
test_rf["Income2"] = test_rf$MonthlyIncome^2

#AdaBoost
library(fastAdaboost)
library(adabag)
train_sampled_Ada = train_sampled_rf
train_sampled_Ada$Default = as.factor(train_sampled_Ada$Default)
test_sampled_Ada = test_rf
test_sampled_Ada$Default = as.factor(test_sampled_Ada$Default)

#Gradient Boosting
library(gbm)

#XGBOOST
library(xgboost)
set.seed(123)
Index = sample.int(n=nrow(train_sampled_rf), size = floor(0.5*nrow(train_sampled_rf)),replace=F)

val = as.matrix(train_sampled_rf[Index,])
train_xgb = as.matrix(train_sampled_rf[-Index,])

xgb.data.train = xgb.DMatrix(data=train_xgb[,-1],label=train_xgb[,1])
xgb.data.test1 = xgb.DMatrix(data=val[,-1],label=val[,1])
xgb.data.test = xgb.DMatrix(data=as.matrix(test_rf[,c(-1,-2,-13)]),label=test_rf[,2])
train_xgb = xgb.data.train
test_xgb = xgb.data.test

#Stochatic Boosting
library(ada)
