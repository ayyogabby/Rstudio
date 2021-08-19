library(dplyr)
library(tidyr)
library(plotrix)
library(ggvis)
library(ggplot2)
library(gplots)
library(caret)
dt <- read.csv("Autism_fullTable.csv")
#Loading the autism dataset
#subsetting the data set
dt1 <- dplyr::select(dat, c("sex_18","HHLanguage_18","LowBWght_18","genetic_18","BornPre_18","ChHlthSt_18",
                            "behavioralcons","MEMORYCOND","MOMAGE","age5_18",
                            "TOTKIDS_R","TOTCSHCN","SC_HISPANIC_R","SC_RACER","povlev4_18",  
                            "AGEPOS4","LearnSev_18", "cluster"))#,"SC_AGE_YEARS","AutismSev_18","K2Q36A"))       
#data preprocessing
cate <- c("sex_18","HHLanguage_18","LowBWght_18","genetic_18","BornPre_18","SC_HISPANIC_R","MEMORYCOND", "behavioralcons")
dt1[cate] <- lapply(dt1[cate], factor)

multicate <- c("SC_RACER","LearnSev_18","povlev4_18","ChHlthSt_18","AGEPOS4")
dt1[multicate] <- lapply(dt1[multicate], factor)

#Pre-assumed ztrue
dt1 <- dplyr::select(dt1, -c("cluster"))
ztrue <- dplyr::select(dt, c("cluster"))
#ztrue$K2Q35A[ztrue$K2Q35A == 0] <- 2
ztruee <- factor(ztrue$K2Q35A)
#table(ztruee)
#set.seed(3223)
#sum(!complete.cases(dt1))



library(VarSelLCM)
# Cluster analysis without variable selection(BIC)
res_withoutBIC <- VarSelCluster(dt1, gvals = 1:3, vbleSelec = FALSE, crit.varsel = "BIC")
# Cluster analysis with variable selection (with parallelisation)
res_withBIC <- VarSelCluster(dt1, gvals = 1:3,nbcores = 5,crit.varsel="BIC")
#Comparison of the BIC for both models: variable selection permits to improve the BIC
# Estimated partition
BIC(res_withoutBIC)
BIC(res_withBIC)
table(fitted(res_withoutBIC))
table(fitted(res_withBIC))


#Evaluation of the partition accuracy: Adjusted Rand Index (ARI) is computed between 
#the true partition (ztrue) and its estimators. The expectation of ARI is zero if the two partitions are independent. The ARI is equal to one if the partitions are equals. Variable selection permits to improve the ARI. Note that ARI cannot
#be used for model selection in clustering, because there is no true partition.
#ARI(ztruee, fitted(res_without))
#ARI(ztruee, fitted(res_with))
table(ztruee, fitted(res_withBIC))


# Estimated probabilities of classification
head(fitted(res_withBIC, type="probability"))
head(fitted(res_withoutBIC, type="probability"))

# Summary of the best model
summary(res_withBIC)

# Summary of the best model
summary(res_withoutBIC)

#Discriminative power of the variables (here, the most discriminative variable is MaxHeartRate). 
#The greater this index, the more the variable distinguishes the clusters.
plot(res_withBIC)

dev.copy(jpeg, filename="momoage.jpg")
plot(res_withBIC, y="MOMAGE", type="cdf")
plot(res_withBIC, y="age5_18", type="cdf")
plot(x=res_withBIC, y="sex_18")
plot(x=res_withBIC, y="MEMORYCOND")
#Distribution of the most discriminative variable per clusters
# Boxplot for the continuous variable MOMAGE
#Empirical and theoretical distributions of the most discriminative variable (to check that the distribution is well-fitted)
# Empirical and theoretical distributions (to check that the distribution is well-fitted)
dev.copy(jpeg, filename="momoage.jpg")
plot(res_withBIC, y="MOMAGE", type="cdf")
dev.off()

dev.copy(jpeg, filename="age_1to5age.jpg")
plot(res_withBIC, y="age5_18", type="cdf")
dev.off()


#Distribution of a categorical variable per clusters
# Summary of categorical variable

#file formats for graphic output
dev.copy(jpeg, filename="sex.jpg")
plot(x=res_withBIC, y="sex_18")
dev.off()



dev.copy(jpeg, filename="Bornpremature.jpg")
plot(x=res_withBIC, y="BornPre_18")
dev.off()



dev.copy(jpeg, filename="GeneticINherited.jpg")
plot(x=res_withBIC, y="genetic_18")
dev.off()



dev.copy(jpeg, filename="childhealthstatus.jpg")
plot(x=res_withBIC, y="ChHlthSt_18")
dev.off()




dev.copy(jpeg, filename="totchwithspecial.jpg")
plot(x=res_withBIC, y="TOTCSHCN")
dev.off()




dev.copy(jpeg, filename="learnsev.jpg")
plot(x=res_withBIC, y="LearnSev_18")
dev.off()



dev.copy(jpeg, filename="memorycon.jpg")
plot(x=res_withBIC, y="MEMORYCOND")
dev.off()




#To have details about the selected model
# More detailed output
dev.copy(jpeg, filename="summmary.jpg")
print(res_withBIC)
dev.off()



#To print the parameters
# Print model parameter
coef(res_withBIC)



#Shiny application
#All the results can be analyzed by the Shiny application…
# Start the shiny application
#VarSelShiny(res_withBIC)

clus <- fitted(res_withBIC)

df1<-data.frame(dt1,cluster=clus)
df1$cluster <- factor(df1$cluster)
write.csv(df1,"autis.csv",row.names=FALSE)


prop.table(table(df1$cluster))
#the data distribution has 37% female(n= 387) and 63% male (n=659)

prop.table(table(dt1$HHLanguage_18))
#household of selected children that  communicate in English compare to those that speak other languages
#is 93%(n = 976) to 7%(n=70) respectively.

prop.table(table(dt1$LowBWght_18))
#percentage of children with low birth weight below 2500lb vs children with birth weight greater than 
#2500lb is 12%(n=121) and 88% respectively (n=925)


prop.table(table(dt1$genetic_18))
#selected kids with blood genetic issiue is 8.9%(n=93) of the total sample while the percentage of kids without
#genetic disorder is 91% (n=953)

prop.table(table(dt1$BornPre_18))
#the number of children with born 3 weeks before due date is 1.4% (n=145) and selected children
#born without premature birth is 86% (n=901).

prop.table(table(dt1$SC_RACER))
#"SC_RACER": Race of Selected Child
#Total number of selected sample "White alone" childern is 78% (n=820)
# Total number of "Black or African American alone" is 5% while (n= 56)
#"Other is 1.6% (n=170)

prop.table(table(dt1$SC_HISPANIC_R))
#"SC_HISPANIC_R": Hispanic Origin of Selected Child, Recoded
#1=yes"Hispanic or Latino Origin" is 1.2%(n=128)
#2=0 no="Not Hispanic or Latino Origin" is 88%(n=918)


prop.table(table(dt1$ChHlthSt_18))
#"ChHlthSt_18": Indicator 1.1: Children's overall health status
#1 = 86% (n=902) of selected children with "Excellent or very good"overall health status
#2 = 11% (n=111) of selected children with "good health status.
#3 = 3% (n=33) of selected children with "fair or poor" overall health status.

prop.table(table(dt1$MEMORYCOND))
#"MEMORYCOND": Serious Difficulty Concentrating, Remembering, or Making Decisions 
#1 = "Yes" total number of children with overall memorycondition 31%(n=324)
#2 = "No" total number of children without overall memorycondition  69%(n=722)


prop.table(table(dt1$behavioralcons))
# Parent-rated severity of current behavioral condition
#0 = children that "Do not currently have condition" behavioral condition is 72%(n=750)
#1 = children that "Current have behavioral condition 28% (n=296) 

prop.table(table(dt1$LearnSev_18))
#"LearnSev_18":  Parent-rated severity of current learning disability, age 3-5 years 
#1 = selected children that "Do not currently have condition" learning disability 74%
#2 = selected children that "Currently have condition, rated mild" 9%
#3 = selected children that "Current condition, rated moderate/severe" 17%

prop.table(table(dt1$povlev4_18))
#povlev4_18: Poverty level of this household based on DHHS guidelines - Imputed : 
#1 = selected number of children with  within "0-199% FPL" is 12%
#2= selected number of children with  within  "200%-299% FPL" 16%
#3 = selected number of children with  within "300%-399% FPL" 30%
#4 = selected number of children with  within "400% FPL or above" 42%


prop.table(table(dt1$TOTCSHCN))
