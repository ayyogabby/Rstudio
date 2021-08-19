library(dplyr)
library(tidyr)
library(plotrix)
library(ggvis)
library(caret)
library(ggplot2)
library(gplots)
dat <- read.csv("autis.csv")

cate <- c("sex_18","HHLanguage_18","LowBWght_18","genetic_18","BornPre_18","SC_HISPANIC_R","MEMORYCOND", "behavioralcons", "cluster")
dat[cate] <- lapply(dat[cate], factor)

multicate <- c("SC_RACER","LearnSev_18","povlev4_18","ChHlthSt_18","AGEPOS4")
dat[multicate] <- lapply(dat[multicate], factor)

set.seed(3333)
library(caret)

summary(dat)
table(dat$AGEPOS4)


#currently have autism YES=1 OR NO = 0 or 2
# Fitting Classification Trees
intrain <- createDataPartition(dat$cluster,p=0.85,list = FALSE)
train1 <- dat[intrain,]
test1 <- dat[-intrain,]
trctrl <- trainControl(method = "cv", number = 10)
tree_fit <- train(cluster ~., data = train1, method = "rpart",
                  trControl=trctrl)
#To see the tuned complexity parameter (Gini Coeff)
tree_fit$bestTune
#To see the tree splits
tree_fit$finalModel
#Plot complexity parameter tuning runs
plot(tree_fit)
#Plot the tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree_fit$finalModel)
#Predict
predictionsTR <- predict(tree_fit, test1)
#Performance metrics
CONTF <- confusionMatrix(predictionsTR,test1$cluster)
CONTF
overallTF <- CONTF$overall
AccTF <- overallTF["Accuracy"]

#To see the importance of the variables
treeImp <- varImp(tree_fit, scale = TRUE)
treeImp
plot(treeImp)



library(pROC)
# ROC Curve
roccurveTR <- roc(test1$cluster ~ as.numeric(predictionsTR))
AUC_TREE <- roccurveTR$auc
AUC_TREE
PRECISION_TREE <- posPredValue(predictionsTR, test1$cluster, positive="1")
RECALL_TREE <- sensitivity(predictionsTR, test1$cluster, positive="1")
plot(roccurveTR)

#F1
precision_TREE <- posPredValue(predictionsTR, test1$cluster, positive="1")
recall_TREE <- sensitivity(predictionsTR, test1$cluster, positive="1")
F1_TREE <- (2 * precision_TREE * recall_TREE) / (precision_TREE + recall_TREE)
F1_TREE


#Random Forest
#intrain <- createDataPartition(Boston$medv,p=0.50,list = FALSE)
#train1 <- Boston[intrain,]
#test1 <- Boston[-intrain,]
#Note: if you want to use out of bag error estimation use method = "oob" below without the number parameter
#trctrl <- trainControl(method = "cv", number = 10)
#Fit the random forest (method = "rf"). Set importance = TRUE to have the variable importance calculated.
#Parameter mtry in the train function lets you set how many variables are considered at each split


forest_fit <- train(cluster~., data = train1, method = "rf",importance = T,
                    trControl=trctrl)
#To see model details
forest_fit
#To see the tuned mtry parameter.  Mtry is the number of randomly selected predictors
forest_fit$bestTune
#To see the the % variance explained
forest_fit$finalModel
#Plot complexity parameter tuning runs
plot(forest_fit)
#Predict
predictionsRF <- predict(forest_fit, test1)
#confusion matrix
CONRF <- confusionMatrix(predictionsRF,test1$cluster)
CONRF
#To see the importance of the variables
forestImp <- varImp(forest_fit)
forestImp
plot(forestImp)

overallRF <- CONRF$overall
AccRF <- overallRF["Accuracy"]



library(pROC)
# ROC Curve
roccurveRF <- roc(test1$cluster ~ as.numeric(predictionsRF))
AUC_RF<- roccurveRF$auc
PRECISION_RF <- posPredValue(predictionsRF, test1$cluster, positive="1")
RECALL_RF<- sensitivity(predictionsRF, test1$cluster, positive="1")
plot(roccurveRF)
#F1
precisionRF <- posPredValue(predictionsRF, test1$cluster, positive="1")
recallRF <- sensitivity(predictionsRF, test1$cluster, positive="1")
F1_RF <- (2 * precisionRF * recallRF) / (precisionRF + recallRF)
F1_RF



#Bagging
#No tuning parameters supported
bag_fit <- train(cluster ~., data = train1, method = "treebag",
                 trControl=trctrl)
bag_fit
predictionsBAG <- predict(bag_fit, newdata = test1)
#confusion matrix
CONBAG <- confusionMatrix(predictionsBAG,test1$cluster)
CONBAG

#To see the importance of the variables
bagImp <- varImp(bag_fit, scale=TRUE)
bagImp
plot(bagImp)

overallBAG <- CONBAG$overall
AccBAG <- overallBAG["Accuracy"]


library(pROC)
# ROC Curve
roccurveBAG <- roc(test1$cluster ~ as.numeric(predictionsBAG))
AUC_BAG <- roccurveBAG$auc
AUC_BAG
PRECISION_BAG <- posPredValue(predictionsBAG, test1$cluster, positive="1")
RECALL_RF<- sensitivity(predictionsBAG, test1$cluster, positive="1")
plot(roccurveRF)
#F1
precisionBAG <- posPredValue(predictionsBAG, test1$cluster, positive="1")
recallBAG <- sensitivity(predictionsBAG, test1$cluster, positive="1")
F1_BAG <- (2 * precisionBAG * recallBAG) / (precisionBAG + recallBAG)
F1_BAG






# Boosting
#intrain <- createDataPartition(dat$cluster,p=0.75,list = FALSE)
#train1 <- dat[intrain,]
#test1 <- dat[-intrain,]
#trctrl <- trainControl(method = "cv", number = 5)


#Fit Ada Boost
boost_fit <- train(cluster~., data = train1, method = "ada")
#To see model details
boost_fit
boost_fit$bestTune
#Plot complexity parameter tuning runs
plot(boost_fit)
#Predict
predictionsADA <- predict(boost_fit, newdata = test1)
#Performance metrics
CONboost <-confusionMatrix(predictionsADA,test1$cluster)
confusionMatrix
#To see the importance of the variables
boostImp <- varImp(boost_fit)
boostImp
plot(boostImp)

overallbst <- CONboost$overall
Accboost <- overallbst["Accuracy"]


library(pROC)
# ROC Curve
roccurveADA <- roc(test1$cluster ~ as.numeric(predictionsADA))
AUC_ADA <- roccurveBAG$auc
AUC_ADA
PRECISION_ADA <- posPredValue(predictionsBAG, test1$cluster, positive="1")
RECALL_RF<- sensitivity(predictionsADA, test1$cluster, positive="1")
plot(roccurveADA)
#F1
precisionADA <- posPredValue(predictionsADA, test1$cluster, positive="1")
recallADA <- sensitivity(predictionsADA, test1$cluster, positive="1")
F1_ADA<- (2 * precisionADA * recallADA) / (precisionADA+ recallADA)
F1_ADA


library(Matrix)
library(xgboost)
#XGBoost
#Binary Classification example
intrain <- createDataPartition(dat$cluster,p=0.85,list = FALSE)
train1 <- dat[intrain,]
test1 <- dat[-intrain,]
trctrl <- trainControl(method = "cv", number = 10)
#Convert target to numeric
tr_label <- as.numeric(train1$cluster) -1
ts_label <- as.numeric(test1$cluster) - 1
#One hot encode predictor variables to convert categorical variables to numeric
m_train <- model.matrix(~.+0,data = train1[,-18]) 
m_test <- model.matrix(~.+0,data = test1[,-18])
dtrain <- xgb.DMatrix(data = m_train,label = tr_label) 
dtest <- xgb.DMatrix(data = m_test,label=ts_label)
params <- list(booster = "gbtree", objective = "binary:logistic", 
               eta=0.3, gamma=0, max_depth=2, min_child_weight=1, 
               subsample=1, colsample_bytree=1)
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, 
                 nfold = 5, showsd = T, stratified = T, 
                 print_every_n = 10, early_stop_rounds = 20, 
                 maximize = F)
bst <- xgb.train(params=params, data=dtrain, nrounds=100)
pred <- predict(bst, dtest)
pred <- ifelse (pred > 0.5,1,0)
pred <- factor(pred)
ts_label <- factor(ts_label)
CONxgb <- confusionMatrix (pred, ts_label)
ARI(pred, ts_label)
mat <- xgb.importance (feature_names = colnames(m_train),model = bst)
xgb.plot.importance (importance_matrix = mat) 


overallxgb <- CONxgb$overall
Accxgb <- overallxgb["Accuracy"]

library(pROC)
# ROC Curve
roccurveXGB <- roc(ts_label ~ as.numeric(pred))
AUC_XGB <- roccurveBAG$auc
AUC_XGB
PRECISION_XGB <- posPredValue(pred, ts_label, positive="1")
RECALL_XGB <- sensitivity(pred, ts_label, positive="1")
plot(roccurveXGB)
#F1
precisionXGB <- posPredValue(pred, ts_label, positive="1")
recallXGB <- sensitivity(pred, ts_label, positive="1")
F1_XGB <- (2 * precisionXGB * recallXGB) / (precisionXGB + recallXGB)
F1_XGB



# Support Vector Machine using Caret
library(caret)
#Create traning test split
intrain <- createDataPartition(dat$cluster,p=0.85,list = FALSE)
train1 <- dat[intrain,]
test1 <- dat[-intrain,]
#Create cross validation
trctrl <- trainControl(method = "cv", number = 10)
#Fit the SVM model to training data
#svmRadial uses the Radial Kernel.  
#You can explicitly specify the cost parameter by using the C = option 
#and the gamma (sigma) parameter using the sigma = option
modSVMRad <- train(cluster ~ .,method="svmRadial",sigma =.2,trControl=trctrl,data=train1)
#See model fit details
modSVMRad$finalModel
#See the tuning parametrs used (cost C, and sigma of the radial kernel function)
modSVMRad$bestTune
#See the results details by each optimization run
modSVMRad$results
#Predict test dataset
SVMpredictRad <- predict(modSVMRad,test1)
CONrad <- confusionMatrix(SVMpredictRad,test1$cluster)

overallSVMR <- CONrad$overall
AccSVMR <- overallSVMR["Accuracy"]


library(pROC)
# ROC Curve
roccurveSVMRad <- roc(test1$cluster ~ as.numeric(SVMpredictRad))
AUC_RAD <- roccurveSVMRad$auc
AUC_RAD
PRECISION_RAD <- posPredValue(SVMpredictRad, test1$cluster, positive="1")
RECALL_XGB <- sensitivity(SVMpredictRad, test1$cluster, positive="1")
plot(roccurveSVMRad)
#F1
precisionRAD <-posPredValue(SVMpredictRad, test1$cluster, positive="1")
recallRAD <- sensitivity(SVMpredictRad, test1$cluster, positive="1")
F1_RAD <- (2 * precisionRAD * recallRAD) / (precisionRAD + recallRAD)
F1_RAD






#Use a polynomial Kernel
modSVMPOLY <- train(cluster ~ .,method="svmPoly",degree = 3, trControl=trctrl,data=train1)
#See model fit details
modSVMPOLY$finalModel
#The polynomial kernel is defined as (scale * crossprod(x, y) + offset)^degree
#See the tuning parametrs used (cost C, Scale of the kernel function)
modSVMPOLY$bestTune
#See the results details by each optimization run
modSVMPOLY$results
#Predict test dataset
SVMpredictPOLY <- predict(modSVMPOLY,test1)
CONSVMP <- confusionMatrix(SVMpredictPOLY,test1$cluster)

overallsvmp <- CONSVMP$overall
AccSVMP <- overallsvmp["Accuracy"]

library(pROC)
# ROC Curve
roccurveSVMPOLY <- roc(test1$cluster ~ as.numeric(SVMpredictPOLY))
AUC_POLY <- roccurveSVMPOLY$auc
AUC_POLY
PRECISION_POLY <- posPredValue(SVMpredictPOLY, test1$cluster, positive="1")
RECALL_POLY <- sensitivity(SVMpredictPOLY, test1$cluster, positive="1")
plot(roccurveSVMRad)
#F1
precisionPOLY <-posPredValue(SVMpredictPOLY, test1$cluster, positive="1")
recallPOLY <- sensitivity(SVMpredictPOLY, test1$cluster, positive="1")
F1_POLY <- (2 * precisionPOLY * recallPOLY) / (precisionPOLY + recallPOLY)
F1_POLY



#Use a tuning grid to tune parameters.  Need one column for each parameter that can be tuned
grid <- expand.grid(C=c(.1,1,5,10), degree=c(2,3,4), scale=c(1,2))
modSVMFitPARA <- train(cluster ~ .,method="svmPoly",tuneGrid=grid, trControl=trctrl,data=train1)
#See model fit details
modSVMFitPARA$finalModel
#The polynomial kernel is defined as (scale * crossprod(x, y) + offset)^degree
#See the tuning parametrs used (cost C, Scale of the kernel function)
modSVMFitPARA$bestTune
#See the results details by each grid search run
modSVMFitPARA$results
#Predict test dataset
SVMpredictPARA <- predict(modSVMFitPARA,test1)
CONSVMPP <- confusionMatrix(SVMpredictPARA,test1$cluster)

overallSVMPP <- CONSVMPP$overall
AccSVMPP <- overallSVMPP["Accuracy"]


library(pROC)
# ROC Curve
roccurveSVMPARA <- roc(test1$cluster ~ as.numeric(SVMpredictPARA))
AUC_PARA <- roccurveSVMPARA$auc
AUC_PARA
PRECISION_PARA <- posPredValue(SVMpredictPARA, test1$cluster, positive="1")
RECALL_PARA <- sensitivity(SVMpredictPARA, test1$cluster, positive="1")
plot(roccurveSVMPARA)
#F1
precisionPARA <-posPredValue(SVMpredictPARA, test1$cluster, positive="1")
recallPARA<- sensitivity(SVMpredictPARA, test1$cluster, positive="1")
F1_PARA <- (2 * precisionPOLY * recallPARA) / (precisionPARA + recallPARA)
F1_PARA

#Create cross validation
trctrl <- trainControl(method = "cv", number = 10)
#Fit the SVM model to training data
#svmLinear uses the linear Kernel.  
modSVMFitLN <- train(cluster ~ .,method="svmLinear2",trControl=trctrl,data=train1)
SVMpredictLN <- predict(modSVMFitLN,test1)
CONLN <- confusionMatrix(SVMpredictLN,test1$cluster)
modSVMFitLN$finalModel
library(pROC)

overallLN <- CONLN$overall
AccLN <- overallLN["Accuracy"]
# ROC Curve
roccurveSVMLN <- roc(test1$cluster ~ as.numeric(SVMpredictLN))
AUC_LN <- roccurveSVMLN$auc
AUC_LN
PRECISION_LN <- posPredValue(SVMpredictLN, test1$cluster, positive="1")
RECALL_LN <- sensitivity(SVMpredictLN, test1$cluster, positive="1")
plot(roccurveSVMLN)
#F1
precisionLN <- posPredValue(SVMpredictLN, test1$cluster, positive="1")
recallLN <- sensitivity(SVMpredictLN, test1$cluster, positive="1")
F1_LN <- (2 * precisionLN * recallLN) / (precisionLN + recallLN)
F1_LN




#Accuracy <- c("92.95 %", "98.72 %", "96.79 %", "99.46 %","100.0 %", "98.08 %", "97.44 %", "98.08 %", "96.15 %")

Accuracy <- c(AccTF, AccRF, AccBAG, Accboost, Accxgb, AccSVMR, AccSVMP, AccSVMPP, AccLN)
Precision <- c(precision_TREE, precisionRF, precisionBAG, precisionADA, precisionXGB, precisionRAD, precisionPOLY, precisionLN, precisionPARA)
Recall <- c(recall_TREE, recallRF, recallBAG, recallADA,recallXGB , recallRAD, recallPOLY, recallLN, recallPARA)
AUC <- c(AUC_TREE,AUC_RF,AUC_BAG, AUC_ADA, AUC_XGB,AUC_RAD, AUC_POLY, AUC_LN, AUC_PARA)
F1 <- c(F1_TREE,F1_RF,F1_BAG,F1_ADA, F1_XGB ,F1_RAD, F1_POLY, F1_LN, F1_PARA)


d <- c("Decision Trees","Random Forest", "Bagging", "Boosting", "XGBoost", "SVMRadial", "SVMPOLY", "SVMLinear", "SVMPOLY Parameter tuning")
result <- data.frame(Accuracy, Precision, Recall, AUC, F1, row.names = d)

write.csv(result, "Result.csv", row.names=FALSE)




