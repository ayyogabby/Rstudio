library(dplyr)
library(tidyr)
library(data.table)
library(plyr)
library(readr)
#----------------------------------------------------------------------------------
mypath="~/Desktop/njcu ML/Homework 3/mid project/Student Retention Challenge Data/Student Progress Data/"
myfiles = list.files(path=mypath, pattern="*.csv", full.names=TRUE)
myfiles
Student_Progress_Data = ldply(myfiles, read_csv)
Student_Progress_Data


StudP <- Student_Progress_Data[!duplicated(Student_Progress_Data$StudentID),]
StudST <- Student_Static_Data[duplicated(Student_Static_Data),]


saveRDS(StudP, file = "Student_Progress_Data.Rda")

Student_Progress_Data[Student_Progress_Data == -2] <- 0
Student_Progress_Data[Student_Progress_Data == -1] <- NA
#Taking out columns with a larger values of NAs
Student_Progress_Data <- select(Student_Progress_Data, -c("Cohort","CompleteCIP2","AcademicYear","Major2","TransferIntent"))
library(Hmisc)
col <- c("StudentID","CohortTerm","Term","CompleteDevMath","CompleteDevEnglish","Major1",
         "Complete1","Complete2","CompleteCIP1","DegreeTypeSought","TermGPA","CumGPA")

sum(!complete.cases(Student_Progress_Data))
library(VIM)
Student_Progress_Data <- kNN(Student_Progress_Data, k=6, impNA = TRUE, imp_var = FALSE)
#"Cohort","CohortTerm","Term","AcademicYear","CompleteDevMath","CompleteDevEnglish","Major1",           
#"Major2","Complete1","Complete2","CompleteCIP1","CompleteCIP2","TransferIntent","DegreeTypeSought","TermGPA", "CumGPA" 
sum(!complete.cases(Student_Progress_Data))
#table(Student_Progress_Data)
col_SP <- c("CohortTerm","Term","CompleteDevMath","CompleteDevEnglish","Complete1","Complete2")
Student_Progress_Data[col_SP] <- lapply(Student_Progress_Data[col_SP], factor)
names(Student_Progress_Data)
saveRDS(Student_Progress_Data, file = "Stu_sp")

#------------------------------------------------------------------
##chosing csv directory and storing
mypth="~/Desktop/njcu ML/Homework 3/mid project/Student Retention Challenge Data/Student Static Data/"
myfiles = list.files(path=mypth, pattern="*.csv", full.names=TRUE)
myfiles
Student_Static_Data = ldply(myfiles, read_csv)
#Takingout columns with alot of missing values and unstructed columns
Student_Static_Data <- select(Student_Static_Data,-c("Cohort","TwoOrMoreRace","CohortTerm","Campus","Address1",
                                                     "Address2","City","State","Zip",
                                                     "RegistrationDate","FirstGen","HSDipYr","HSGPAUnwtd",
                                                     "HSGPAWtd",))
Student_Static_Data <- Student_Static_Data[duplicated(Student_Static_Data),]
saveRDS(Student_Static_Data, file = "Student_Static_Data.Rda")


#All -2 to 1 to merge with those that complete with nt refered
Student_Static_Data[Student_Static_Data == -2] <- 0
Student_Static_Data[Student_Static_Data == -1] <- NA

col_st <- c("Gender","Hispanic","AmericanIndian","Asian","Black","NativeHawaiian","White","HSDip","DualHSSummerEnroll","EnrollmentStatus",
            "HighDeg","MathPlacement","EngPlacement","GatewayMathStatus","GatewayEnglishStatus")  

sum(!complete.cases(Student_Static_Data))

library(VIM)
Student_Static_Data <- kNN(Student_Static_Data, k=6, imp_var = FALSE, impNA = TRUE,
                           variable=c("StudentID","Gender","BirthYear","BirthMonth","Hispanic",
                                      "AmericanIndian","Asian","Black","NativeHawaiian","White",
                                      "HSDip","DualHSSummerEnroll","EnrollmentStatus"))
Student_Static_Data <- kNN(Student_Static_Data, k=3, imp_var = FALSE,impNA = TRUE,
                           variable=c("NumColCredAttemptTransfer","NumColCredAcceptTransfer", 
                                      "CumLoanAtEntry","HighDeg","MathPlacement","EngPlacement","GatewayMathStatus","GatewayEnglishStatus"))  


Student_Static_Data <- mutate(Student_Static_Data, Gender = ifelse(Student_Static_Data$Gender == 2, 0,1))
sum(!complete.cases(Student_Static_Data))


#"StudentID","Gender","BirthYear","BirthMonth","Hispanic","AmericanIndian","Asian","Black","NativeHawaiian","White",
#"HSDip","DualHSSummerEnroll","EnrollmentStatus","NumColCredAttemptTransfer","NumColCredAcceptTransfer" 
#"CumLoanAtEntry","HighDeg","MathPlacement","EngPlacement","GatewayMathStatus","GatewayEnglishStatus"

saveRDS(Student_Static_Data, file = "Stu_sta")


col_st <- c("Gender","Hispanic","AmericanIndian","Asian","Black","NativeHawaiian","White","HSDip","DualHSSummerEnroll","EnrollmentStatus",
            "HighDeg","MathPlacement","EngPlacement","GatewayMathStatus","GatewayEnglishStatus")  

Student_Static_Data[col_st] <- lapply(Student_Static_Data[col_st], factor)
names(Student_Static_Data)
#--------------------------------------------------------------------
#library(readxl)
#Student_Financial_Aid <- read_excel("~/Desktop/njcu ML/Homework 3/mid project/Student Retention Challenge Data/Student Financial Aid Data/2011-2017_Cohorts_Financial_Aid_and_Fafsa_Data.xlsx")

aid <- read.csv("~/Desktop/cleanProject data/aid.csv")
aid <- mutate(aid, NumApp= ifelse(aid$NumApp > 0, 1,0))
aid <- select(aid, -c('X', "cohort","cohort.term"))
aid[aid == -2 ]<-0
aid[aid == -1 ]<- NA
library(VIM)
aid <- kNN(aid, k=3, imp_var = FALSE,impNA = TRUE)


col_a <- c("Marital.Status", "Father.s.Highest.Grade.Level","Mother.s.Highest.Grade.Level","Housing","NumApp")
aid[col_a] <- lapply(aid[col_a], factor)
names(aid)[names(aid) =='ID.with.leading'] <- "StudentID"
sum(is.na(aid))

saveRDS(aid, file = "aidf")
#-------------------------------------------------------------------------

#Merging
Student_Progress_Data
Student_Static_Data
aid

Student_info <- merge(aid,Student_Static_Data, by= "StudentID")
Student_W_aid <- inner_join(Student_Progress_Data,Student_info,by="StudentID")
Student_W_aid <- Student_W_aid[!duplicated(Student_W_aid$StudentID),]

#---------------------------------------------------------------------------------
label <- read.csv("~/Desktop/new work/DropoutTrainLabels.csv")
TestIDs <- read.csv("~/Desktop/new work/TestIDs.csv")

library(caret)
Non_zv <- nearZeroVar(Student_W_aid)
filtered <- Student_W_aid[, -Non_zv]
dim(Student_W_aid)
dim(filtered)




training<-filtered[-which(filtered$StudentID %in% TestIDs$StudentID),]
Kaggletest<-filtered[which(filtered$StudentID %in% TestIDs$StudentID),]
Kaggletest

student2 <- merge(label,training, by="StudentID")
student2$Dropout<-factor(student2$Dropout)

#---------------------------------------------------------------------------------


sum(!complete.cases(student2))
library(caret)
#Feature/Variable Importance in Caret
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model.fit <- train(Dropout~.-StudentID, data=student2, method="rpart", preProcess="scale", trControl=control)
# Estimate variable importance
importance <- varImp(model.fit, scale=TRUE)
# summarize importance
print(importance)
# plot importance
plot(importance)

#--------------------------------------------------------------------------------

#Prepare data for unbalanced library use
n<-ncol(ubIonosphere)
output<-ubIonosphere$Class
input<-ubIonosphere[ ,-n]
#Tomek Links using library unbalanced
data<-ubTomek(X=input, Y= output)
#Edited nearest neighbor (ENN)
data<-ubENN(X=input, Y= output)
#Neighborhood Clearing Rule
data<-ubNCL(X=input, Y= output)

#Combine data for use in caret
newData<-cbind(data$X, data$Y)
names(newData)[33] <- "Class"




#-------------------------------------------------------------------------------------
student2 <- select(student2, -c(StudentID))
library(caret)
set.seed(334)
intrain <- createDataPartition(student2$Dropout,p=0.85,list = FALSE)
train <- student2[intrain,]
test <- student2[-intrain,]

model_weights <- ifelse(train$Dropout == 0,
                        (1/table(train$Dropout)[1]) * 0.5,
                        (1/table(train$Dropout)[2]) * 0.5)
#Setup the cross validation. method is the type of cross validation and number is the number of folds
trctrl <- trainControl(method = "cv", number = 10, sampling = "down")
#fitting classification tree
#tree_fit<-train(Dropout ~CumGPA+TermGPA+Term+BirthYear+Major1+EngPlacement+CompleteDevEnglish+Marital.Status+
#                HighDeg+GatewayEnglishStatus+Adjusted.Gross.Income,data = train,method="rpart",
#                trControl=trctrl, weights=model_weights)

tree_fit<-train(Dropout ~.,data = train,method="rpart",
                trControl=trctrl, weights=model_weights)

predictionsT <- predict(tree_fit, newdata = test)
#Performance metrics
Tcf=confusionMatrix(predictionsT,test$Dropout)
Tcf
predTkagg <- predict(tree_fit, newdata = Kaggletest, type = "raw")
submission1<-cbind(StudentID=TestIDs,Dropout=predTkagg)

write.csv(submission1,"submissionAy.csv",row.names=FALSE)
submission1

library(ROCR)
predT = prediction(treemodel.probs[,2], treemodel.labels) 

plot(performance(predictionsT, "tpr", "fpr"), col="red", lwd=2) 
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=2)
auc = performance(predictionsT, 'auc')
slot(auc, 'y.values')

#--------------------------------------------------------------------------------
set.seed(2434)
#Bagging
intrain <- createDataPartition(student2$Dropout,p=0.80,list = FALSE)
train3 <- student2[intrain,]
test3 <- student2[-intrain,]
#Note: if you want to use out of bag error estimation use method = "oob" below without the number parameter

trctrl <- trainControl(method = "cv", number = 10)
#No tuning parameters supported
bag_fit <- train(Dropout ~.,data = train3,method="treebag",
                 trControl=trctrl)
bag_fit
predictionsbg <- predict(bag_fit, newdata = test3)
BGcf=confusionMatrix(predictionsbg, test3$Dropout)
BGcf

predTkagg1 <- predict(bag_fit, newdata = Kaggletest, type = "raw")
submission2<-cbind(StudentID=TestIDs,Dropout=predTkagg1)
write.csv(submission2,"submissionAy1.csv",row.names=FALSE)
submission2
library(ROCR)
ROCRpred <- prediction(as.numeric(predictionsbg), as.numeric(test3$Dropout))
ROCFperf <- performance(ROCRpred, "tpr", "fpr")
plot(ROCFperf, colorize=TRUE)
abline(0,1)

precision <- posPredValue(predictionsbg, test3$Dropout, positive = "0")
recall <- sensitivity(predictionsbg, test3$Dropout, positive = "0")
F1 <- (2*precision*recall)/(precision+recall)
F1

#submission1<-unique(submission1)
write.csv(submission1,"submission1.csv")
submission1

#To see the importance of the variables
bagImp <- varImp(bag_fit, scale=TRUE)
bagImp
plot(bagImp)
#-----------------------------------------------------------------------------------
set.seed(333)
intrain <- createDataPartition(student2$Dropout,p=0.80,list = FALSE)
trainf <- student2[intrain,]
testf <- student2[-intrain,]
#Note: if you want to use out of bag error estimation use method = "oob" below without the number parameter
trctrl <- trainControl(method = "cv", number = 10, sampling = "up")
#Fit the random forest (method = "rf"). Set importance = TRUE to have the variable importance calculated.
#Parameter mtry in the train function lets you set how many variables are considered at each split
#forest_fit <- train(Dropout ~ BirthYear+CumGPA+NumApp+TermGPA+Major1+EngPlacement+CompleteCIP1+         
#                    NumColCredAcceptTransfer+Complete1+NumColCredAttemptTransfer+CohortTerm+     
 #                   Adjusted.Gross.Income+CompleteDevEnglish+GatewayEnglishStatus+EnrollmentStatus+              
  #                  Term+CompleteDevMath+MathPlacement+Housing+HighDeg,
 #                   data = trainf,method="rf",importance = T,trControl=trctrl)

forest_fit <- train(Dropout ~.,data = trainf,method="rf",importance = T,
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
predictionsF <- predict(forest_fit, newdata = testf)
#Calculate MSE
Prf=confusionMatrix(predictionsF, testf$Dropout)
Prf


kaggtest <- select(Kaggletest, c("CumGPA","TermGPA","Term","BirthYear","Major1","EngPlacement","CompleteDevEnglish","Marital.Status",
                                   "HighDeg","GatewayEnglishStatus","Adjusted.Gross.Income"))
set.seed(3432)
kaggleprediction <- predict(forest_fit, newdata = Kaggletest, type = "raw")

set.seed(232)
submission9<-cbind(StudentID=TestIDs,Dropout=kaggleprediction)
unique(sub)

table(submission9$Dropout)

write.csv(submission9,"submission9.csv",row.names=FALSE)
submission6
#To see the importance of the variables
forestImp <- varImp(forest_fit)
forestImp
plot(forestImp)






"BirthYear","CumGPA","NumApp","TermGPA","Major1","EngPlacement","CompleteCIP1",         
"NumColCredAcceptTransfer","Complete1","NumColCredAttemptTransfer","CohortTerm",      
"Adjusted.Gross.Income","CompleteDevEnglish","GatewayEnglishStatus","EnrollmentStatus",              
"Term","CompleteDevMath","MathPlacement","Housing","HighDeg" 











#---------------------------------------------------------------------------------
set.seed(3333)
# Boosting
intrain <- createDataPartition(student2$Dropout,p=0.80,list = FALSE)
train4 <- student2[intrain,]
test4 <- student2[-intrain,]
trctrl <- trainControl(method = "cv", number = 10)
#Fit Ada Boost
boost_fit <- train(Dropout ~ BirthYear+CumGPA+NumApp+TermGPA+Major1+EngPlacement+CompleteCIP1+         
                     NumColCredAcceptTransfer+Complete1+NumColCredAttemptTransfer+CohortTerm+     
                     Adjusted.Gross.Income+CompleteDevEnglish+GatewayEnglishStatus+EnrollmentStatus+              
                     Term+CompleteDevMath+MathPlacement+Housing+HighDeg, data = train4, method = "ada")
#To see model details
boost_fit
boost_fit$bestTune
#Plot complexity parameter tuning runs
plot(boost_fit)
predictionsboss <- predict(boost_fit, newdata = test4)
#Performance metrics
confusionMatrix(predictionsboss,test4$Dropout)
#Predict
predictionsboss <- predict(boost_fit, newdata = kaggletest)
#Performance metrics
confusionMatrix(predictionsboss,kaggletest$Dropout)
#To see the importance of the variables
boostImp <- varImp(boost_fit)
boostImp
plot(boostImp)


#-----------------------------------------------------------------------------------
set.seed(3456)
#Support Vector classifier
intrain <- createDataPartition(student2$Dropout,p=0.85,list = FALSE)
train5 <- student2[intrain,]
test5 <- student2[-intrain,]
#Create cross validation
trctrl <- trainControl(method = "cv", number = 10)
#Fit the SVM model to training data
#svmRadial uses the Radial Kernel.  
#You can explicitly specify the cost parameter by using the C = option 
#and the gamma (sigma) parameter using the sigma = option
modSVMFit <- train(Dropout~ BirthYear+CumGPA+NumApp+TermGPA+Major1+EngPlacement+CompleteCIP1+         
                     NumColCredAcceptTransfer+Complete1+NumColCredAttemptTransfer+CohortTerm+     
                     Adjusted.Gross.Income+CompleteDevEnglish+GatewayEnglishStatus+EnrollmentStatus+              
                     Term+CompleteDevMath+MathPlacement+Housing+HighDeg,method="svmRadial",
                   trControl=trctrl,data=train5,degree=3 ,sigma=5,preProcess=c("center","scale"))
#See model fit details
modSVMFit$finalModel
#See the tuning parametrs used (cost C, and sigma of the radial kernel function)
modSVMFit$bestTune
#See the results details by each optimization run
modSVMFit$results
#Predict test dataset
SVMpredictL <- predict(modSVMFit,test5)
confusionMatrix(SVMpredictL,test5$Dropout)


#Polynomial
set.seed(4899)
intrain <- createDataPartition(student2$Dropout,p=0.85,list = FALSE)
train2 <- student2[intrain,]
test2 <- student2[-intrain,]
#Create cross validation
trctrl <- trainControl(method = "cv", number = 10)
#Fit the SVM model to training data
#svmRadial uses the Radial Kernel.  
#You can explicitly specify the cost parameter by using the C = option 
#and the gamma (sigma) parameter using the sigma = option
grid <- expand.grid(cost=c(.01, 0.5, 10), degree=c(2,3,4), scale(1,2))
modSVMFitp <- train(Dropout~ BirthYear+CumGPA+NumApp+TermGPA+Major1+EngPlacement+CompleteCIP1+         
                      NumColCredAcceptTransfer+Complete1+NumColCredAttemptTransfer+CohortTerm+     
                      Adjusted.Gross.Income+CompleteDevEnglish+GatewayEnglishStatus+EnrollmentStatus+              
                      Term+CompleteDevMath+MathPlacement+Housing+HighDeg,method="svmRadial",TuneGrid=grid,trControl=trctrl,data=train2) 
#See model fit details
modSVMFitp$finalModel
#See the tuning parametrs used (cost C, and sigma of the radial kernel function)
modSVMFitp$bestTune
#See the results details by each optimization run
modSVMFitp$results
#Predict test dataset
SVMpredictp <- predict(modSVMFitp,test2)
confusionMatrix(SVMpredictp,test2$Dropout)




predictionsvm <- predict(modSVMFitp, newdata = Kaggletest, type = 'raw')
#Performance metrics
submission8<-cbind(StudentID=TestIDs,Dropout=predictionsvm)
unique(sub)

write.csv(submission8,"submission8.csv",row.names=FALSE)
submission6




Tcf
BGcf
predictionsF



































install.packages("LaTeX")



install.packages('knitr')
install.packages("rmarkdown")
install.packages("markdown")







#---------------------------------------------------------------------------

student_st <- read.csv("~/Desktop/njcu ML/Homework 3/mid project/Student Retention Challenge Data/Student Static Data/St_full.csv")
aid <- read.csv("~/Desktop/cleanProject data/aid.csv")
Student_P <- read.csv("~/Desktop/njcu ML/Homework 3/mid project/processed.csv")
#change the column name of student ids in aid, from I.D.with.leading to StudentID
names(aid)[names(aid) =='ID.with.leading'] <- "StudentID"
student_data <- inner_join(Student_P, student_st,by='StudentID')


nwdata <- left_join(aid, student_data, by ="StudentID")
#df <- nwdata[!duplicated(nwdata$StudentID),]


dat_d <- select(nwdata, -c("X","Campus","Address1","Address2","City","State","Zip",
                           "CumLoanAtEntry","HSDip","HSDipYr","HSGPAUnwtd","HSGPAWtd",
                           "FirstGen","RegistrationDate","Cohort.x","CohortTerm.x",
                           "Cohort.y","CohortTerm.y","cohort","TwoOrMoreRace"))
library(VIM)


dat_d[dat_d==-1]<- 999
dat_d[is.na(dat_d)]<- 999
dat_d[dat_d == 999]<-NA





fill(data = dat_d, .direction = c("up"))
dat_d %>% fill(dat_d[10:42])



dat_d <- kNN(data = dat_d, k=6, imp_var = FALSE)


dat_d[dat_d == -1]<- kNN(dat_d, k=5, imp_var = FALSE)
Student<-dat_d[complete.cases(dat_d),]


library(Hmisc)
dat_d = regressionImp(data=dat_d, imp_var = FALSE)
dat_d[is.na(dat_d)] <- mean(dat_d, na.rm = T)


jd = merge(TestIDs,dat_d, by="StudentID")
jd2 <- jd[!duplicated(jd$StudentID),]



data <- na.omit(data)
sum(is.na(data))
colSums(!is.na(data))-nrow(data)
#---------------------------------------------------------------------------
saveRDS(data, file='new.Rda')
neww <- readRDS(file = "new.Rda")
datt1 <- readRDS(file='new2.Rda')

#-------------------------------------------------------------------------
datt1 <- data[!duplicated(data$StudentID),]

label <- read.csv("~/Desktop/new work/DropoutTrainLabels.csv")
TestIDs <- read.csv("~/Desktop/new work/TestIDs.csv")

library(caret)
nzv <- nearZeroVar(datt1, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]

nzv <- nearZeroVar(datt1)
filtered <- datt1[, -nzv]
dim(datt1)
dim(filtered)




dt <- merge(StudentIDs=TestIDs,Predictions=kaggetest1, by="StudentID", all.StudentIDs=TRUE)
kaggetest1 <-left_join(TestIDs, filtered, by="StudentID")

train<-filtered[-which(filtered$StudentID %in% TestIDs$StudentID),]
Kaggletest<-filtered[which(filtered$StudentID %in% TestIDs$StudentID),]
Kaggletest



Good <- Kaggletest[!duplicated(Kaggletest$StudentID),]
student2 <- merge(train, label, by="StudentID")

student2$Dropout<-factor(student2$Dropout)

library(caret)
set.seed(3333)
intrain <- createDataPartition(student2$Dropout,p=0.70,list = FALSE)
train <- student2[intrain,]
test <- student2[-intrain,]
#Setup the cross validation. method is the type of cross validation and number is the number of folds
trctrl <- trainControl(method = "cv", number = 5, sampling = "smote")
#fitting classification tree
tree_fit<-train(Dropout ~.-StudentID,data = train,method="rpart",
                trControl=trctrl, preProcess="scale")

predictionsT <- predict(tree_fit, newdata = test)
#Performance metrics
confusionMatrix(predictionsT,test$Dropout)


predTkagg <- predict(tree_fit, newdata = Kaggletest, type = "raw")

#prediction<-predict(Fit,newdata = Kaggletest,type = "raw")

submission1<-cbind.data.frame(StudentID=Kaggletest$StudentID,Dropout=predTkagg)
#submission1<-unique(submission1)
write.csv(submission1,"submission1.csv")
submission1
```
```{r}
write.csv(submission1,"submission1.csv")
submission1


m1 <- anti_join(TestIDs,submission1,by = "StudentID")
m1
write.csv(m1,"submissions3.csv",row.names=FALSE)
#dim(m1)




submission11 <-anti_join(TestIDs, predTkagg, by="StudentID")
write.csv(submission11,"submissionOJ.csv",row.names=FALSE)
testID <-read.csv("~/Desktop/cleanProject data/TestIDs.csv")
pred <- c(predictions)
ayo_olu <- cbind(testID=c(testID),predictions=c(pred))


#----------------------------------------------------------------------------------

#Bagging
intrain <- createDataPartition(student2$Dropout,p=0.75,list = FALSE)
train3 <- student2[intrain,]
test3 <- student2[-intrain,]
#Note: if you want to use out of bag error estimation use method = "oob" below without the number parameter
trctrl <- trainControl(method = "cv", number = 10,sampling = "smote")
#No tuning parameters supported
bag_fit <- train(Dropout ~.-StudentID, data = train3, method = "treebag",
                 trControl=trctrl)
bag_fit
predictionsbg <- predict(bag_fit, newdata = kaggletest)
confusionMatrix(predictionsbg, kaggletest$Dropout)
#To see the importance of the variables
bagImp <- varImp(bag_fit, scale=TRUE)
bagImp
plot(bagImp)



























































library(dplyr)
library(tidyr)

student_st <- read.csv("~/Desktop/njcu ML/Homework 3/mid project/Student Retention Challenge Data/Student Static Data/St_full.csv")
aid <- read.csv("~/Desktop/cleanProject data/aid.csv")
Student_P <- read.csv("~/Desktop/njcu ML/Homework 3/mid project/processed.csv")
#change the column name of student ids in aid, from I.D.with.leading to StudentID
names(aid)[names(aid) =='ID.with.leading'] <- "StudentID"
student_data <- inner_join(Student_P, student_st,by='StudentID')


nwdata <- left_join(aid, student_data, by ="StudentID")
#df <- nwdata[!duplicated(nwdata$StudentID),]


dat_d <- select(nwdata, -c("X","Campus","Address1","Address2","City","State","Zip",
                          "CumLoanAtEntry","HSDip","HSDipYr","HSGPAUnwtd","HSGPAWtd",
                          "FirstGen","RegistrationDate","Cohort.x","CohortTerm.x",
                          "Cohort.y","CohortTerm.y","cohort","TwoOrMoreRace"))
library(VIM)


dat_d[dat_d==-1]<- 999
dat_d[is.na(dat_d)]<- 999
dat_d[dat_d == 999]<-NA





fill(data = dat_d, .direction = c("up"))
dat_d %>% fill(dat_d[10:42])



dat_d <- kNN(data = dat_d, k=6, imp_var = FALSE)


dat_d[dat_d == -1]<- kNN(dat_d, k=5, imp_var = FALSE)
Student<-dat_d[complete.cases(dat_d),]


library(Hmisc)
dat_d = regressionImp(data=dat_d, imp_var = FALSE)
dat_d[is.na(dat_d)] <- mean(dat_d, na.rm = T)


jd = merge(TestIDs,dat_d, by="StudentID")
jd2 <- jd[!duplicated(jd$StudentID),]



data <- na.omit(data)
sum(is.na(data))
colSums(!is.na(data))-nrow(data)
#---------------------------------------------------------------------------
saveRDS(data, file='new.Rda')
neww <- readRDS(file = "new.Rda")
datt1 <- readRDS(file='new2.Rda')

#-------------------------------------------------------------------------
datt1 <- data[!duplicated(data$StudentID),]

label <- read.csv("~/Desktop/new work/DropoutTrainLabels.csv")
TestIDs <- read.csv("~/Desktop/new work/TestIDs.csv")

library(caret)
nzv <- nearZeroVar(datt1, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]

nzv <- nearZeroVar(datt1)
filtered <- datt1[, -nzv]
dim(datt1)
dim(filtered)




dt <- merge(StudentIDs=TestIDs,Predictions=kaggetest1, by="StudentID", all.StudentIDs=TRUE)
kaggetest1 <-left_join(TestIDs, filtered, by="StudentID")

train<-filtered[-which(filtered$StudentID %in% TestIDs$StudentID),]
Kaggletest<-filtered[which(filtered$StudentID %in% TestIDs$StudentID),]
Kaggletest



Good <- Kaggletest[!duplicated(Kaggletest$StudentID),]
student2 <- merge(train, label, by="StudentID")

student2$Dropout<-factor(student2$Dropout)

library(caret)
set.seed(3333)
intrain <- createDataPartition(student2$Dropout,p=0.70,list = FALSE)
train <- student2[intrain,]
test <- student2[-intrain,]
#Setup the cross validation. method is the type of cross validation and number is the number of folds
trctrl <- trainControl(method = "cv", number = 5, sampling = "smote")
#fitting classification tree
tree_fit<-train(Dropout ~.-StudentID,data = train,method="rpart",
                trControl=trctrl, preProcess="scale")

predictionsT <- predict(tree_fit, newdata = test)
#Performance metrics
confusionMatrix(predictionsT,test$Dropout)


predTkagg <- predict(tree_fit, newdata = Kaggletest, type = "raw")

#prediction<-predict(Fit,newdata = Kaggletest,type = "raw")

submission1<-cbind.data.frame(StudentID=Kaggletest$StudentID,Dropout=predTkagg)
#submission1<-unique(submission1)
write.csv(submission1,"submission1.csv")
submission1

write.csv(submission1,"submission1.csv")
submission1






submission11 <-anti_join(TestIDs, predTkagg, by="StudentID")
write.csv(submission11,"submissionOJ.csv",row.names=FALSE)
testID <-read.csv("~/Desktop/cleanProject data/TestIDs.csv")
pred <- c(predictions)
ayo_olu <- cbind(testID=c(testID),predictions=c(pred))


#----------------------------------------------------------------------------------

#Bagging
intrain <- createDataPartition(student2$Dropout,p=0.75,list = FALSE)
train3 <- student2[intrain,]
test3 <- student2[-intrain,]
#Note: if you want to use out of bag error estimation use method = "oob" below without the number parameter
trctrl <- trainControl(method = "cv", number = 10,sampling = "smote")
#No tuning parameters supported
bag_fit <- train(Dropout ~.-StudentID, data = train3, method = "treebag",
                 trControl=trctrl)
bag_fit
predictionsbg <- predict(bag_fit, newdata = kaggletest)
confusionMatrix(predictionsbg, kaggletest$Dropout)
#To see the importance of the variables
bagImp <- varImp(bag_fit, scale=TRUE)
bagImp
plot(bagImp)
#-----------------------------------------------------------
#eloi <- inner_join(student_st,Student_Progress_Data,by='StudentID')
#eloi2 <- merge(aid,eloi, by ="StudentID")

#eloi3 <- select(eloi2, -c("X","Campus","Address1","Address2","City","State","Zip",
#                          "CumLoanAtEntry","HSDip","HSDipYr","HSGPAUnwtd","HSGPAWtd",
##                        "Cohort.y","CohortTerm.y","cohort","TwoOrMoreRace"))
#sum(!complete.cases(eloi3))
#eloi3 <- na.omit(eloi3)

#Student<-eloi3[complete.cases(eloi3),]
#head(Student)
#eloi4 <- eloi3[!duplicated(eloi3$StudentID),]
#eloi4[eloi4== -2]<- 0

#-----------------------------------------------------------------------------------

#label <- read.csv("~/Desktop/new work/DropoutTrainLabels.csv")
#TestIDs <- read.csv("~/Desktop/new work/TestIDs.csv")



#nonzv <- nearZeroVar(eloi4)
#filtered <- eloi4[, -nzv]
#dim(eloi4)
#dim(filtered)

#filtered$Marital.Status <-as.numeric(factor(filtered$Marital.Status))
#filtered$Father.s.Highest.Grade.Level <- as.numeric(factor(filtered$Father.s.Highest.Grade.Level))
#filtered$Mother.s.Highest.Grade.Level <- as.numeric(factor(filtered$Mother.s.Highest.Grade.Level))
#filtered$Housing <- as.numeric(factor(filtered$Housing))

#--------------------------------------------------------------------------------------------
#TestIDs <- read.csv("~/Desktop/njcu ML/Homework 3/mid project/Student Retention Challenge Data/Test Data/TestIDs.csv", sep="")

#training<-filtered[-which(filtered$StudentID %in% TestIDs$StudentID),]
#Kaggletest<-filtered[which(filtered$StudentID %in% TestIDs$StudentID),]
#Kaggletest

#student2 <- merge(label,training, by="StudentID")
#student2$Dropout<-factor(student2$Dropout)

#---------------------------------------------------------------------------------------
#student2 <-select(student2, -c("StudentID","AcademicYear"))

#Find correlated features
# calculate correlation matrix
correlationMatrix <- cor(student2[,2:32])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)
#ploting the correlations
library(corrplot)
#printing the names of the variables to remove
name<-names(student3[names(select_if(student3, is.numeric))])[highlyCorrelated]
name
filtered1 <- student3[,!names(student3)%in%name]
#checking the new dimnensions
dim(filtered1)
