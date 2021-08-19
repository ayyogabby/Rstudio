library(dplyr)
library(tidyr)


SP <- read.csv("~/Desktop/njcu ML/Homework 3/mid project/processed.csv")
sum(!complete.cases(SP))

ST <- read.csv("~/Desktop/njcu ML/Homework 3/mid project/SP_clean.csv")
DT <- read.csv("~/Desktop/cleanProject data/aid.csv")
AD <-as.data.frame(DT)
names(AD)[names(AD) =='ID.with.leading'] <- "StudentID"
AD1 <- select(AD, -c("X", "cohort", "cohort.term"))
#sp <- select(SP, -c('Cohort', 'CohortTerm','Term','AcademicYear','Term','CumGPA',"season"))

st <- as.data.frame(ST)
ad1 <- as.data.frame(AD1)
sp <- as.data.frame(SP)
df <- left_join(ad1,st,by ="StudentID")
df <- SP[!duplicated(SP$StudentID),]

DF4 <- left_join(df, sp, by = 'StudentID',no.dups = FALSE,all= FALSE)

Good <- DF4[!duplicated(DF4$StudentID),]

library(VIM)

colnames(Good)

njcu <- kNN(Good, k=6,imp_var = FALSE)

colSums(!is.na(njcu))-nrow(njcu)

mydata <- as.data.frame(Good, col.names= names(Good))

sum(!complete.cases(njcu))

#------------------------------------------------------------------------

#``````````````````````````````````````````````````````````````````````````````
write.csv(njcu,"~/Desktop/cleanProject data/mynj.csv",row.names = FALSE)


install.packages("devtools")
install.packages("roxygen2")






####

library(dplyr)
library(tidyr)
library(ggvis)
library(ggplot2)
dt = readRDS(file = 'mytrain.Rda')
data <- data.frame(dt)
test <- readRDS(file="mykaggle.Rda")
kaggletest <-data.frame(test)


test$Mother.s.Highest.Grade.Level <- as.numeric(test$Mother.s.Highest.Grade.Level)

levels(data$Term)
saveRDS(data, file = "myyydata.Rda")

#---------------------------------------------------------------------------


dt <- read.csv("~/Desktop/cleanProject data/mynjcu.csv")

njcu<- data.frame(dt)
saveRDS(testdata1, file = "tllll.Rda")
nyyy = readRDS(file = 'mydt.Rda')
all <- left_join(nyyy,traindt, by="StudentID")



traindt <-read.csv("~/Desktop/cleanProject data/DropoutTrainLabels.csv")
all <- merge(traindt,nyyy, by="StudentID")



all$Dropout<-factor(all$Dropout)

all$Dropout[all$Dropout == NA] <- 1


sum(is.na(all))
library(VIM)
all <- kNN(data = all, k=6, imp_var=FALSE)

sum(is.na(all))



test <-read.csv("~/Desktop/cleanProject data/TestIDs.csv")
testdata1 <- merge(test, all, by ="StudentID")

test1 <- merge.data.frame(test, nyyy, by='StudentID')


#dt = readRDS(file = 'kaggletest.Rda')
#data<-select(dt, -c("StudentID"))









tr1$Dropout<- factor(tr1$Dropout)
test$Mother.s.Highest.Grade.Level<- as.numeric(test$Mother.s.Highest.Grade.Level)

colnames(dt)

ts1 <- select(d45,c("TermGPA","CumGPA","Adjusted.Gross.Income","Parent.Adjusted.Gross.Income",
                     "NumColCredAcceptTransfer","HSGPAUnwtd","Term","Mother.s.Highest.Grade.Level",
                     "Father.s.Highest.Grade.Level","Housing","CompleteDevEnglish","Gender","CompleteDevMath",             
                     "HighDeg","GatewayEnglishStatus","GatewayMathStatus","Dropout"))

tt2$Dropout <- factor(tt2$Dropout)
table(dt$CompleteDevEnglish)
table(test$CompleteDevEnglish)


t1 <- readRDS(file="train.Rda")
t12 <- readRDS(file="testtt.Rda")
saveRDS(tt2, file='ttestt.Rda')

tt2 <- select(tt2, -c("NumApp"))
d45 <- readRDS(file="testt.Rda")
tt2$Dropout <- factor(tt2$Dropout)
tt2$Father.s.Highest.Grade.Level <- as.numeric(tt2$Father.s.Highest.Grade.Level)
tt2$Mother.s.Highest.Grade.Level <- as.numeric(tt2$Mother.s.Highest.Grade.Level)
tt2 <- select(t1,c("TermGPA","CumGPA",'NumApp',"Adjusted.Gross.Income","Parent.Adjusted.Gross.Income",
                     "NumColCredAcceptTransfer","HSGPAUnwtd","Term","Mother.s.Highest.Grade.Level",
                     "Father.s.Highest.Grade.Level","Housing","CompleteDevEnglish","Gender","CompleteDevMath",             
                     "HighDeg","GatewayEnglishStatus","GatewayMathStatus","Dropout"))

names(ts1)
set.seed(3333)
#Runing our first model based on the the top 20 most important variable in our data
library(caret)
#Create training and test datasets. The p parameter determines the split(e.g. 75/25)
intrain <- createDataPartition(tt2$Dropout,p=0.80,list = FALSE)
train <- tt2[intrain,]
test <- tt2[-intrain,]
#Setup the cross validation. method is the type of cross validation and number is the number of folds
trctrl <- trainControl(method = "cv", number = 10, sampling = "smote")
cost= c(0.01, 0.5, 1.0)
library(caret)
modfit <- train(Dropout ~ ., method = 'svmLinear', data = train,
                trControl = trctrl, tune=cost)
predictionsW <- predict(modfit, newdata = ts1)
confusionMatrix(predictionsW,ts1$Dropout)

table(tt2$Dropout)
sum(is.na(tt2))

library(readxl)
Student_Financial_Aid <- read_excel("~/Desktop/njcu ML/Homework 3/mid project/Student Retention Challenge Data/Student Financial Aid Data/2011-2017_Cohorts_Financial_Aid_and_Fafsa_Data.xlsx")

t <- readRDS(file="trainn.Rda")
saveRDS(T, file='angel.Rda')




T <- select(t, c("Term","TermGPA","CumGPA","AppLoan","NumColCredAcceptTransfer","Housing","CompleteDevEnglish",
                       "Housing","Dropout"))
data$GatewayEnglishStatus <- as.numeric(data$GatewayEnglishStatus)

kaggletest$GatewayEnglishStatus <- as.numeric(kaggletest$GatewayEnglishStatus)






install.packages("subspace")
library(subspace)
install.packages('JVM')
