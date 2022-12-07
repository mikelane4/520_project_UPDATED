####THIS WILL UPDATE OFTEN####

install.packages("misty")

library(caret)
library(rlang)
library(rpart)
library(rpart.plot)
library(e1071)
library(tidyr)
library(dplyr)
library(mlbench)
library(randomForest)
library(Hmisc)
library(corrplot)
library(ggcorrplot)

ransomware <- read.csv('C:/Users/Mike/Downloads/ransomware/ransomware.csv' , sep = ',')

set.seed(42)

ransomware$x <- NULL

head(ransomware)
tail(ransomware)
summary(ransomware)
str(ransomware)

#Sector as.numeric
ransomware[ransomware =='academic'] <- as.numeric(0)             
ransomware[ransomware =='automotive'] <- as.numeric(1)
ransomware[ransomware =='energy'] <- as.numeric(2)
ransomware[ransomware =='finance'] <- as.numeric(3)
ransomware[ransomware =='finace'] <- as.numeric(3)
ransomware[ransomware =='food & beverage'] <- as.numeric(4)
ransomware[ransomware =='gaming'] <- as.numeric(5)
ransomware[ransomware =='government'] <- as.numeric(6)
ransomware[ransomware =='healthcare'] <- as.numeric(7)
ransomware[ransomware =='healthcare, tech'] <- as.numeric(8)
ransomware[ransomware =='legal'] <- as.numeric(9)
ransomware[ransomware =='logistics'] <- as.numeric(10)
ransomware[ransomware =='logistics, telecoms'] <- as.numeric(11)
ransomware[ransomware =='media & sports'] <- as.numeric(12)
ransomware[ransomware =='military'] <- as.numeric(13)
ransomware[ransomware =='misc'] <- as.numeric(14)
ransomware[ransomware =='mixed'] <- as.numeric(15)
ransomware[ransomware =='retail'] <- as.numeric(16)
ransomware[ransomware =='tech'] <- as.numeric(17)
ransomware[ransomware =='tech '] <- as.numeric(17)
ransomware[ransomware =='telecomms'] <- as.numeric(18)
ransomware[ransomware =='telecoms'] <- as.numeric(18)
ransomware[ransomware =='transport'] <- as.numeric(19)
ransomware$sector[ransomware$sector ==''] <- as.numeric(20)
ransomware$sector[ransomware$sector =='NA'] <- as.numeric(20)

#ransom.paid as.numeric
#ransomware[ransomware =='refused'] <- as.numeric(0)
#ransomware[ransomware =='ransom paid'] <- as.numeric(1)
#ransomware[ransomware =='unknown'] <- as.numeric(2)

ransomware[ransomware =='refused'] <- as.numeric(1)
ransomware[ransomware =='ransom paid'] <- as.numeric(2)
ransomware[ransomware =='unknown'] <- as.numeric(0)

#Ransomware as.numeric
ransomware[ransomware =='Armada Collective'] <- as.numeric(0)
ransomware[ransomware =='Astro'] <- as.numeric(1)
ransomware$Ransomware[ransomware$Ransomware =='unknown'] <- as.numeric(2)
ransomware$Ransomware[ransomware$Ransomware =='Not revealed'] <- as.numeric(2)
ransomware[ransomware =='Babuk'] <- as.numeric(3)
ransomware[ransomware =='Babuk '] <- as.numeric(3)
ransomware[ransomware =='Babuk Locker'] <- as.numeric(3)
ransomware[ransomware =='BadRabbit'] <- as.numeric(4)
ransomware[ransomware =='Black Shadow'] <- as.numeric(5)
ransomware[ransomware =='Clop'] <- as.numeric(6)
ransomware[ransomware =='CLOP'] <- as.numeric(6)
ransomware[ransomware =='Conti'] <- as.numeric(7)
ransomware[ransomware =='CryptoLocker'] <- as.numeric(8)
ransomware[ransomware =='CryptoWall'] <- as.numeric(9)
ransomware[ransomware =='Cuba ransomware'] <- as.numeric(10)
ransomware[ransomware =='DarkSide'] <- as.numeric(11)
ransomware[ransomware =='Darkside'] <- as.numeric(11)
ransomware[ransomware =='Defray'] <- as.numeric(12)
ransomware[ransomware =='DoppelPaymer'] <- as.numeric(13)
ransomware[ransomware =='Egregor'] <- as.numeric(14)
ransomware[ransomware =='Erkan'] <- as.numeric(15)
ransomware[ransomware =='EvilCorp'] <- as.numeric(16)
ransomware[ransomware =='HDDCryptor'] <- as.numeric(17)
ransomware[ransomware =='HelloKitty'] <- as.numeric(18)
ransomware[ransomware =='Hotarus Corp'] <- as.numeric(19)
ransomware[ransomware =='iEncrypt'] <- as.numeric(20)
ransomware[ransomware =='LockBit'] <- as.numeric(21)
ransomware[ransomware =='Lockbit'] <- as.numeric(21)
ransomware[ransomware =='Maze'] <- as.numeric(22)
ransomware[ransomware =='Mount Locker'] <- as.numeric(23)
ransomware[ransomware =='MountLocker'] <- as.numeric(23)
ransomware[ransomware =='Avaddon'] <- as.numeric(24)
ransomware[ransomware =='Nefilim'] <- as.numeric(25)
ransomware[ransomware =='Netwalker'] <- as.numeric(26)
ransomware[ransomware =='NetWalker'] <- as.numeric(26)
ransomware[ransomware =='NotPetya'] <- as.numeric(27)
ransomware[ransomware =='NotPetya & Petya'] <- as.numeric(27)
ransomware[ransomware =='Pay2Key'] <- as.numeric(28)
ransomware[ransomware =='Phoenix Locker'] <- as.numeric(29)
ransomware[ransomware =='PYSA'] <- as.numeric(30)
ransomware[ransomware =='Qbot'] <- as.numeric(31)
ransomware[ransomware =='QBot'] <- as.numeric(31)
ransomware[ransomware =='Ragnar Locker'] <- as.numeric(32)
ransomware[ransomware =='Ragnarok'] <- as.numeric(32)
ransomware[ransomware =='RansomEXX'] <- as.numeric(33)
ransomware[ransomware =='RansomExx'] <- as.numeric(33)
ransomware[ransomware =='REvil'] <- as.numeric(34)
ransomware[ransomware =='RobbinHood'] <- as.numeric(35)
ransomware[ransomware =='Ryuk'] <- as.numeric(36)
ransomware[ransomware =='TrickBot, Ryuk'] <- as.numeric(36)
ransomware[ransomware =='SamSam'] <- as.numeric(37)
ransomware[ransomware =='Shadow Kill Hackers'] <- as.numeric(38)
ransomware[ransomware =='Snake'] <- as.numeric(39)
ransomware[ransomware =='SYNack'] <- as.numeric(40)
ransomware[ransomware =='TA505'] <- as.numeric(41)
ransomware[ransomware =='TeslaCrypt'] <- as.numeric(42)
ransomware[ransomware =='WannaCry'] <- as.numeric(43)
ransomware[ransomware =='WastedLocker'] <- as.numeric(44)
ransomware[ransomware =='Xing Team'] <- as.numeric(45)
ransomware$Ransomware[ransomware$Ransomware == ''] <- as.numeric(2)


#More processing
ransomware$revenue <- gsub(",","", ransomware$revenue)
ransomware$revenue <- as.numeric(ransomware$revenue)
ransomware$YEAR <- as.numeric(ransomware$YEAR)
ransomware$ransom.paid <- as.numeric(ransomware$ransom.paid)
ransomware$sector <- as.numeric(ransomware$sector)
ransomware$Ransomware <- as.numeric(ransomware$Ransomware)
ransomware$organisation.size <- as.numeric(ransomware$organisation.size)

ransomware_cleaned <- ransomware_clean[complete.cases(ransomware_clean),]

#Remove redundant YEAR.code
ransomware_clean <- ransomware[-c(6)]
str(ransomware_clean)

#write file to csv
write.csv(ransomware_clean, 'C:/Users/Mike/Downloads/ransomware/ransomware_edit.csv', row.names = FALSE)

#--------------------------------------------------------------------------------------------------------------------------------

#CorrelationMatrix doesn't work well
correlationMatrix_ransomware <- cor(na.omit(ransomware_cleaned[ ,1:6])) 
print(correlationMatrix_ransomware)
plot(correlationMatrix_ransomware)

#Needs tweaked (lose NA's.. na.rm())
visCorMatrix1_ransomware <- corrplot(cor(na.omit(ransomware_cleaned)))
visCorMatrix1_ransomware
visCorMatrix2_ransomware <-ggcorrplot(cor(na.omit(ransomware_cleaned)))
visCorMatrix2_ransomware

# find attributes that are highly corrected (ideally >0.75) - No work lose the NA's
highlyCorrelated_ransomware <- na.exclude(findCorrelation(correlationMatrix_ransomware, cutoff=0.5,verbose = FALSE,names = TRUE ))
print(highlyCorrelated_ransomware)
plot(highlyCorrelated_ransomware)

#Make sure it is set
set.seed(42)

# prepare training scheme for rfe
control_ransomware <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model for rfe
model_ransomware_ransom.paid <- train(as.factor(ransom.paid)~., data=ransomware_cleaned, na.action=na.exclude, method="lvq", preProcess="scale", trControl=control_ransomware)
model_ransomware_YEAR <- train(as.factor(YEAR)~., data=ransomware_cleaned, na.action=na.exclude, method="lvq", preProcess="scale", trControl=control_ransomware)
#model_ransomware_revenue <- train(as.factor(revenue)~., data=ransomware_cleaned, na.action=na.exclude, method="lvq", preProcess="scale", trControl=control_ransomware)
#model_ransomware_Ransomware <- train(as.factor(Ransomware)~., data=ransomware_cleaned, na.action=na.exclude, method="lvq", preProcess="scale", trControl=control_ransomware)
model_ransomware_sector <- train(as.factor(YEAR)~., data=ransomware_cleaned, na.action=na.exclude, method="lvq", preProcess="scale", trControl=control_ransomware)
#model_ransomware_ransom.cost <- train(as.factor(ransom.cost)~., data=ransomware_cleaned, na.action=na.exclude, method="lvq", preProcess="scale", trControl=control_ransomware)
model_ransomware_organisation.size <- train(as.factor(organisation.size)~., data=ransomware_cleaned, na.action=na.exclude, method="lvq", preProcess="scale", trControl=control_ransomware)

# estimate variable importance
importance_ransom.paid <- varImp(model_ransomware_ransom.paid, scale=FALSE)
print(importance_ransom.paid)
plot(importance_ransom.paid)

importance_YEAR <- varImp(model_ransomware_YEAR, scale=FALSE)
print(importance_YEAR)
plot(importance_YEAR)

importance_sector <- varImp(model_ransomware_sector, scale=FALSE)
print(importance_sector)
plot(importance_sector)

#Lets use this, but add color
importance_organisation.size <- varImp(model_ransomware_organisation.size, scale=FALSE)
print(importance_organisation.size)
plot(importance_organisation.size)

control_ransomware_rfe <- rfeControl(functions=rfFuncs, method="cv", number=10)

set.seed(42)

#rfe
rfe_ransomware_model <- rfe(ransomware_cleaned[,0:5], ransomware_cleaned[ ,6], sizes=c(0:5), na.action=na.exclude, rfeControl=control_ransomware_rfe)
rfe_ransomware_model
plot(rfe_ransomware_model)

predictors(rfe_ransomware_model)

#----------------------------------------------------------------------------------------------------------------------------------

#trainctrl for knn, nnet, and svm
trainctrl_ransomware <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)

#ransom.paid

#knn, nnet, and svm for **RANSOM.PAID**
knn.model_ransomware_ransom.paid <- train(as.factor(ransom.paid)~., data = ransomware_cleaned, na.action=na.exclude, method="knn",
                             tuneLength = 10,
                             trControl = trainctrl_ransomware,
                             metric="Accuracy")

nnet.model_ransomware_ransom.paid <- train(as.factor(ransom.paid)~., data = ransomware_cleaned, na.action=na.exclude, method="nnet",
                              tuneLength = 10,
                              trControl = trainctrl_ransomware,
                              metric="Accuracy")

svm.model_ransomware_ransom.paid <- train(as.factor(ransom.paid)~., data = ransomware_cleaned, na.action=na.exclude, method="svmRadial",
                             tuneLength = 10,
                             trControl = trainctrl_ransomware,
                             metric="Accuracy")

knn.model_ransomware_ransom.paid
nnet.model_ransomware_ransom.paid
svm.model_ransomware_ransom.paid   

#Predictions
knn.model_ransomware_ransom.paid_predict <- predict(knn.model_ransomware_ransom.paid, ransomware_cleaned)
nnet.model_ransomware_ransom.paid_predict <- predict(nnet.model_ransomware_ransom.paid, ransomware_cleaned)
svm.model_ransomware_ransom.paid_predict <- predict(svm.model_ransomware_ransom.paid, ransomware_cleaned)

#Results / Length and Levels are TRUE and confusionMatrix works
identical(length(ransomware_clean[ransomware_clean$ransom.paid == "prediction",1]) , length(ransomware_clean[ransomware_clean$ransom.paid == "real",1]))

confusionMatrix(knn.model_ransomware_ransom.paid_predict, as.factor(ransomware_cleaned$ransom.paid), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_ransom.paid_predict, as.factor(ransomware_cleaned$ransom.paid), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_ransom.paid_predict, as.factor(ransomware_cleaned$ransom.paid), mode = "prec_recall")

#Results plot
plot(knn.model_ransomware_ransom.paid) 
plot(nnet.model_ransomware_ransom.paid) 
plot(svm.model_ransomware_ransom.paid)

#Simple plot for as.numeric for ransom.paid
plot(knn.model_ransomware_ransom.paid_predict) 
plot(nnet.model_ransomware_ransom.paid_predict) 
plot(svm.model_ransomware_ransom.paid_predict)

#--------------------------------------------------------------------------------------------------------------------------

# YEAR

trainctrl_ransomware <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)
set.seed(42)

#knn, nnet, and svm for **YEAR**
knn.model_ransomware_year <- train(as.factor(YEAR)~., data = ransomware_cleaned, na.action=na.exclude, method="knn",
                              tuneLength = 10,
                              trControl = trainctrl_ransomware,
                              metric="Accuracy")

nnet.model_ransomware_year <- train(as.factor(YEAR)~., data = ransomware_cleaned, na.action=na.exclude, method="nnet",
                               tuneLength = 10,
                               trControl = trainctrl_ransomware,
                               metric="Accuracy")

svm.model_ransomware_year <- train(as.factor(YEAR)~., data = ransomware_cleaned, na.action=na.exclude, method="svmRadial",
                              tuneLength = 10,
                              trControl = trainctrl_ransomware,
                              metric="Accuracy")

knn.model_ransomware_year
nnet.model_ransomware_year
svm.model_ransomware_year   

#Predictions
knn.model_ransomware_year_predict <- predict(knn.model_ransomware_year, ransomware_cleaned)
nnet.model_ransomware_year_predict <- predict(nnet.model_ransomware_year, ransomware_cleaned)
svm.model_ransomware_year_predict <- predict(svm.model_ransomware_year, ransomware_cleaned)

knn.model_ransomware_year_predict = as.factor(knn.model_ransomware_year_predict)
nnet.model_ransomware_year_predict = as.factor(nnet.model_ransomware_year_predict)
svm.model_ransomware_year_predict = as.factor(svm.model_ransomware_year_predict)

#Results 
confusionMatrix(knn.model_ransomware_year_predict, as.factor(ransomware_cleaned$YEAR), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_year_predict, as.factor(ransomware_cleaned$YEAR), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_year_predict, as.factor(ransomware_cleaned$YEAR), mode = "prec_recall")

#Results / Length and Levels are TRUE and confusionMatrix works
identical(length(ransomware_cleaned[ransomware_cleaned$YEAR== "prediction",1]) , length(ransomware_cleaned[ransomware_cleaned$YEAR == "real",1]))
identical(levels(ransomware_cleaned[ransomware_cleaned$YEAR== "prediction",1]) , levels(ransomware_cleaned[ransomware_cleaned$YEAR == "real",1]))

#Results plot
plot(knn.model_ransomware_year)
plot(nnet.model_ransomware_year)
plot(svm.model_ransomware_year)

#Simple plot for as.numeric for YEAR
plot(knn.model_ransomware_year_predict)
plot(nnet.model_ransomware_year_predict)
plot(svm.model_ransomware_year_predict)

#--------------------------------------------------------------------------------------------------------------------------

# sector

trainctrl_ransomware <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)
set.seed(42)
str(ransomware_cleaned)
summary(ransomware_cleaned)

#Insufficient Info for sectors 8, 13, and 20
ransomware_cleaned_sector <- ransomware_cleaned[!(ransomware_cleaned$sector == 8 | ransomware_cleaned$sector == 13 | ransomware_cleaned$sector == 20),]
ransomware_cleaned_sector 


#knn, nnet, and svm for **sector**
knn.model_ransomware_sector <- train(as.factor(sector)~., data = ransomware_cleaned_sector, na.action=na.exclude, method="knn",
                                    tuneLength = 10,
                                    trControl = trainctrl_ransomware,
                                    metric="Accuracy")

nnet.model_ransomware_sector <- train(as.factor(sector)~., data = ransomware_cleaned_sector, na.action=na.exclude, method="nnet",
                                    tuneLength = 10,
                                    trControl = trainctrl_ransomware,
                                    metric="Accuracy")

svm.model_ransomware_sector <- train(as.factor(sector)~., data = ransomware_cleaned_sector, na.action=na.exclude, method="svmRadial",
                                   tuneLength = 10,
                                   trControl = trainctrl_ransomware,
                                   metric="Accuracy")

knn.model_ransomware_sector
nnet.model_ransomware_sector
svm.model_ransomware_sector   

#Predictions
knn.model_ransomware_sector_predict <- predict(knn.model_ransomware_sector, ransomware_cleaned_sector)
nnet.model_ransomware_sector_predict <- predict(nnet.model_ransomware_sector, ransomware_cleaned_sector)
svm.model_ransomware_sector_predict <- predict(svm.model_ransomware_sector, ransomware_cleaned_sector)

#Results
confusionMatrix(knn.model_ransomware_sector_predict, as.factor(ransomware_cleaned_sector$sector), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_sector_predict, as.factor(ransomware_cleaned_sector$sector), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_sector_predict, as.factor(ransomware_cleaned_sector$sector), mode = "prec_recall")

#Results plot
plot(knn.model_ransomware_sector)
plot(nnet.model_ransomware_sector)
plot(svm.model_ransomware_sector)

#Simple plot for as.numeric for sector
plot(knn.model_ransomware_sector_predict)
plot(nnet.model_ransomware_sector_predict)
plot(svm.model_ransomware_sector_predict)

#------------------------------------------------------------------------------------------------------------------------

#organisation.size

trainctrl_ransomware <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)
set.seed(42)

#knn, nnet, and svm for **organisation.size*
knn.model_ransomware_organisation.size <- train(as.factor(organisation.size)~., data = ransomware_cleaned, na.action=na.exclude, method="knn",
                                     tuneLength = 10,
                                     trControl = trainctrl_ransomware,
                                     metric="Accuracy")

nnet.model_ransomware_organisation.size <- train(as.factor(organisation.size)~., data = ransomware_cleaned, na.action=na.exclude, method="nnet",
                                      tuneLength = 10,
                                      trControl = trainctrl_ransomware,
                                      metric="Accuracy")

svm.model_ransomware_organisation.size <- train(as.factor(organisation.size)~., data = ransomware_cleaned, na.action=na.exclude, method="svmRadial",
                                     tuneLength = 10,
                                     trControl = trainctrl_ransomware,
                                     metric="Accuracy")
#Predictions
knn.model_ransomware_organisation.size_predict <- predict(knn.model_ransomware_organisation.size, ransomware_cleaned)
nnet.model_ransomware_organisation.size_predict <- predict(nnet.model_ransomware_organisation.size, ransomware_cleaned)
svm.model_ransomware_organisation.size_predict <- predict(svm.model_ransomware_organisation.size, ransomware_cleaned)

#Results
confusionMatrix(knn.model_ransomware_organisation.size_predict, as.factor(ransomware_cleaned$organisation.size), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_organisation.size_predict, as.factor(ransomware_cleaned$organisation.size), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_organisation.size_predict, as.factor(ransomware_cleaned$organisation.size), mode = "prec_recall")

#Results plot
plot(knn.model_ransomware_organisation.size)
plot(nnet.model_ransomware_organisation.size)
plot(svm.model_ransomware_organisation.size)

#Simple plot for as.numeric for sector
plot(knn.model_ransomware_organisation.size_predict)
plot(nnet.model_ransomware_organisation.size_predict)
plot(svm.model_ransomware_organisation.size_predict)

#------------------------------------------------------------------------------------------------------------------------

#Ransomware

trainctrl_ransomware <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)
set.seed(42)

#knn, nnet, and svm for **organisation.size*
knn.model_ransomware_Ransomware <- train(as.factor(Ransomware)~., data = ransomware_cleaned, na.action=na.exclude, method="knn",
                                         tuneLength = 10,
                                         trControl = trainctrl_ransomware,
                                         metric="Accuracy")

nnet.model_ransomware_Ransomware <- train(as.factor(Ransomware)~., data = ransomware_cleaned, na.action=na.exclude, method="nnet",
                                          tuneLength = 10,
                                          trControl = trainctrl_ransomware,
                                          metric="Accuracy")

svm.model_ransomware_Ransomware <- train(as.factor(Ransomware)~., data = ransomware_cleaned, na.action=na.exclude, method="svmRadial",
                                         tuneLength = 10,
                                         trControl = trainctrl_ransomware,
                                         metric="Accuracy")
#Predictions
knn.model_ransomware_Ransomware_predict <- predict(knn.model_ransomware_Ransomware, ransomware_cleaned)
nnet.model_ransomware_Ransomware_predict <- predict(nnet.model_ransomware_Ransomware, ransomware_cleaned)
svm.model_ransomware_Ransomware_predict <- predict(svm.model_ransomware_Ransomware, ransomware_cleaned)

#Results
confusionMatrix(knn.model_ransomware_Ransomware_predict, as.factor(ransomware_cleaned$Ransomware), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_Ransomware_predict, as.factor(ransomware_cleaned$Ransomware), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_Ransomware_predict, as.factor(ransomware_cleaned$Ransomware), mode = "prec_recall")

#Results plot
plot(knn.model_ransomware_Ransomware)
plot(nnet.model_ransomware_Ransomware)
plot(svm.model_ransomware_Ransomware)

#Simple plot for as.numeric for sector
plot(knn.model_ransomware_Ransomware_predict)
plot(nnet.model_ransomware_Ransomware_predict)
plot(svm.model_ransomware_Ransomware_predict)

#------------------------------------------------------------------------------------------------------------------------

#revenue

trainctrl_ransomware <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)
set.seed(42)

#knn, nnet, and svm for **revenue*
#knn.model_ransomware_revenue <- train(as.factor(revenue)~., data = ransomware_cleaned, na.action=na.exclude, method="knn",
#                                         tuneLength = 10,
#                                      trControl = trainctrl_ransomware,
#                                        metric="Accuracy")

nnet.model_ransomware_revenue <- train(as.factor(revenue)~., data = ransomware_cleaned, na.action=na.exclude, method="nnet",
                                          tuneLength = 10,
                                          trControl = trainctrl_ransomware,
                                          metric="Accuracy")

svm.model_ransomware_revenue <- train(as.factor(revenue)~., data = ransomware_cleaned, na.action=na.exclude, method="svmRadial",
                                         tuneLength = 10,
                                         trControl = trainctrl_ransomware,
                                         metric="Accuracy")
#Predictions
#knn.model_ransomware_revenue_predict <- predict(knn.model_ransomware_revenue, ransomware_cleaned)
nnet.model_ransomware_revenue_predict <- predict(nnet.model_ransomware_revenue, ransomware_cleaned)
svm.model_ransomware_revenue_predict <- predict(svm.model_ransomware_revenue, ransomware_cleaned)

#Results
#confusionMatrix(knn.model_ransomware_revenue_predict, as.factor(ransomware_cleaned$revenue), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_revenue_predict, as.factor(ransomware_cleaned$revenue), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_revenue_predict, as.factor(ransomware_cleaned$revenue), mode = "prec_recall")

#Results plot
plot(knn.model_ransomware_revenue)
plot(nnet.model_ransomware_revenue)
plot(svm.model_ransomware_revenue)

#Simple plot for as.numeric for sector
plot(knn.model_ransomware_revenue_predict)
plot(nnet.model_ransomware_revenue_predict)
plot(svm.model_ransomware_revenue_predict)

#------------------------------------------------------------------------------------------------------------------------

#ransom.cost

str(ransomware_cleaned)
trainctrl_ransomware <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)
set.seed(42)

#knn, nnet, and svm for **RANSOM.COST**
#knn.model_ransomware_ransom.cost <- train(as.factor(ransom.cost )~., data = ransomware_cleaned, na.action=na.exclude, method="knn",
#                                          tuneLength = 10,
#                                          trControl = trainctrl_ransomware,
#                                          metric="Accuracy")

nnet.model_ransomware_ransom.cost  <- train(as.factor(ransom.cost )~., data = ransomware_cleaned, na.action=na.exclude, method="nnet",
                                           tuneLength = 10,
                                           trControl = trainctrl_ransomware,
                                           metric="Accuracy")

svm.model_ransomware_ransom.cost  <- train(as.factor(ransom.cost )~., data = ransomware_cleaned, na.action=na.exclude, method="svmRadial",
                                          tuneLength = 10,
                                          trControl = trainctrl_ransomware,
                                          metric="Accuracy")

knn.model_ransomware_ransom.cost 
nnet.model_ransomware_ransom.cost 
svm.model_ransomware_ransom.cost   

#Predictions
#knn.model_ransomware_ransom.cost_predict <- predict(knn.model_ransomware_ransom.cost, ransomware_cleaned)
nnet.model_ransomware_ransom.cost_predict <- predict(nnet.model_ransomware_ransom.cost, ransomware_cleaned)
svm.model_ransomware_ransom.cost_predict <- predict(svm.model_ransomware_ransom.cost, ransomware_cleaned)

#Results / Length and Levels are TRUE and confusionMatrix works
identical(length(ransomware_clean[ransomware_cleaned$ransom.cost == "prediction",1]) , length(ransomware_cleaned[ransomware_cleaned$ransom.cost == "real",1]))

#confusionMatrix(knn.model_ransomware_ransom.cost_predict, as.factor(ransomware_cleaned$ransom.cost), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_ransom.cost_predict, as.factor(ransomware_cleaned$ransom.cost), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_ransom.cost_predict, as.factor(ransomware_cleaned$ransom.cost), mode = "prec_recall")

#Results plot
plot(knn.model_ransomware_ransom.cost)
plot(nnet.model_ransomware_ransom.cost)
plot(svm.model_ransomware_ransom.cost)

#Simple plot for as.numeric for ransom.paid
plot(knn.model_ransomware_ransom.cost_predict)
plot(nnet.model_ransomware_ransom.cost_predict)
plot(svm.model_ransomware_ransom.cost_predict)

#-------------------------------------------------------------------------------------------------------------------------------------------------

#Results as matrix

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#ransom.paid
confusionMatrix(knn.model_ransomware_ransom.paid_predict, as.factor(ransomware_cleaned$ransom.paid), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_ransom.paid_predict, as.factor(ransomware_cleaned$ransom.paid), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_ransom.paid_predict, as.factor(ransomware_cleaned$ransom.paid), mode = "prec_recall")

knn_ransom.paid <- confusionMatrix(knn.model_ransomware_ransom.paid_predict, as.factor(ransomware_cleaned$ransom.paid),
                                   mode = "prec_recall")
as.matrix(knn_ransom.paid, what="overall")

nnet_ransom.paid <- confusionMatrix(nnet.model_ransomware_ransom.paid_predict, as.factor(ransomware_cleaned$ransom.paid),
                                   mode = "prec_recall")
as.matrix(nnet_ransom.paid, what="overall")

svm_ransom.paid <- confusionMatrix(svm.model_ransomware_ransom.paid_predict, as.factor(ransomware_cleaned$ransom.paid),
                                   mode = "prec_recall")
as.matrix(svm_ransom.paid, what="overall")

#-------------------------------------------------------------------------------------------------------------------------

#YEAR

confusionMatrix(knn.model_ransomware_year_predict, as.factor(ransomware_cleaned$YEAR), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_year_predict, as.factor(ransomware_cleaned$YEAR), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_year_predict, as.factor(ransomware_cleaned$YEAR), mode = "prec_recall")

knn_YEAR <- confusionMatrix(knn.model_ransomware_year_predict, as.factor(ransomware_cleaned$YEAR), mode = "prec_recall")
as.matrix(knn_YEAR, what="overall")

nnet_YEAR <- confusionMatrix(nnet.model_ransomware_year_predict, as.factor(ransomware_cleaned$YEAR), mode = "prec_recall")
as.matrix(nnet_YEAR, what="overall")

svm_YEAR <- confusionMatrix(svm.model_ransomware_year_predict, as.factor(ransomware_cleaned$YEAR), mode = "prec_recall")
as.matrix(svm_YEAR, what="overall")

nnet_ransom.cost <- confusionMatrix(nnet.model_ransomware_ransom.cost_predict, as.factor(ransomware_cleaned$ransom.cost), 
                mode = "prec_recall") 
as.matrix(nnet_ransom.cost, what="overall")

#-------------------------------------------------------------------------------------------------------------------------

#sector

confusionMatrix(knn.model_ransomware_sector_predict, as.factor(ransomware_cleaned_sector$sector), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_sector_predict, as.factor(ransomware_cleaned_sector$sector), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_sector_predict, as.factor(ransomware_cleaned_sector$sector), mode = "prec_recall")

knn_sector <- confusionMatrix(knn.model_ransomware_sector_predict, as.factor(ransomware_cleaned_sector$sector), mode = "prec_recall")
as.matrix(knn_sector, what="overall")

nnet_sector <- confusionMatrix(nnet.model_ransomware_sector_predict, as.factor(ransomware_cleaned_sector$sector), mode = "prec_recall")
as.matrix(nnet_sector, what="overall")

svm_sector <- confusionMatrix(svm.model_ransomware_sector_predict, as.factor(ransomware_cleaned_sector$sector), mode = "prec_recall")
as.matrix(svm_sector, what="overall")

#-------------------------------------------------------------------------------------------------------------------------

#organisation.size

confusionMatrix(knn.model_ransomware_organisation.size_predict, as.factor(ransomware_cleaned$organisation.size), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_organisation.size_predict, as.factor(ransomware_cleaned$organisation.size), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_organisation.size_predict, as.factor(ransomware_cleaned$organisation.size), mode = "prec_recall")

knn_organisation.size <- confusionMatrix(knn.model_ransomware_organisation.size_predict, as.factor(ransomware_cleaned$organisation.size), 
                                         mode = "prec_recall")
as.matrix(knn_organisation.size, what="overall")

nnet_organisation.size <- confusionMatrix(nnet.model_ransomware_organisation.size_predict, as.factor(ransomware_cleaned$organisation.size), 
                                         mode = "prec_recall")
as.matrix(nnet_organisation.size, what="overall")

svm_organisation.size <- confusionMatrix(svm.model_ransomware_organisation.size_predict, as.factor(ransomware_cleaned$organisation.size), 
                                         mode = "prec_recall")
as.matrix(svm_organisation.size, what="overall")

#-------------------------------------------------------------------------------------------------------------------------

#Ransomware

confusionMatrix(knn.model_ransomware_Ransomware_predict, as.factor(ransomware_cleaned$Ransomware), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_Ransomware_predict, as.factor(ransomware_cleaned$Ransomware), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_Ransomware_predict, as.factor(ransomware_cleaned$Ransomware), mode = "prec_recall")

knn_Ransomware <- confusionMatrix(knn.model_ransomware_Ransomware_predict, as.factor(ransomware_cleaned$Ransomware), 
                                  mode = "prec_recall")
as.matrix(knn_Ransomware, what="overall")

nnet_Ransomware <- confusionMatrix(nnet.model_ransomware_Ransomware_predict, as.factor(ransomware_cleaned$Ransomware), 
                                  mode = "prec_recall")
as.matrix(nnet_Ransomware, what="overall")

svm_Ransomware <- confusionMatrix(svm.model_ransomware_Ransomware_predict, as.factor(ransomware_cleaned$Ransomware), 
                                   mode = "prec_recall")
as.matrix(svm_Ransomware, what="overall")

#-------------------------------------------------------------------------------------------------------------------------

#revenue

#Results
#confusionMatrix(knn.model_ransomware_revenue_predict, as.factor(ransomware_cleaned$revenue), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_revenue_predict, as.factor(ransomware_cleaned$revenue), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_revenue_predict, as.factor(ransomware_cleaned$revenue), mode = "prec_recall")

nnet_revenue <- confusionMatrix(nnet.model_ransomware_revenue_predict, as.factor(ransomware_cleaned$revenue), 
                                mode = "prec_recall")
as.matrix(nnet_revenue, what="overall")

svm_revenue <- confusionMatrix(svm.model_ransomware_revenue_predict, as.factor(ransomware_cleaned$revenue), 
                                mode = "prec_recall")
as.matrix(svm_revenue, what="overall")

#-------------------------------------------------------------------------------------------------------------------------

#ransom.cost

#confusionMatrix(knn.model_ransomware_ransom.cost_predict, as.factor(ransomware_cleaned$ransom.cost), mode = "prec_recall")
confusionMatrix(nnet.model_ransomware_ransom.cost_predict, as.factor(ransomware_cleaned$ransom.cost), mode = "prec_recall")
confusionMatrix(svm.model_ransomware_ransom.cost_predict, as.factor(ransomware_cleaned$ransom.cost), mode = "prec_recall")

nnet_ransom.cost <- confusionMatrix(nnet.model_ransomware_ransom.cost_predict, as.factor(ransomware_cleaned$ransom.cost), 
                                    mode = "prec_recall")
as.matrix(nnet_ransom.cost, what="overall")

svm_ransom.cost <- confusionMatrix(svm.model_ransomware_ransom.cost_predict, as.factor(ransomware_cleaned$ransom.cost), 
                                    mode = "prec_recall")
as.matrix(svm_ransom.cost, what="overall")

#--------------------------------------------------------------------------------------------------------------------------

#Test and Training on organisation.size and revenue

trainIndex_organisation.size <- createDataPartition(ransomware_cleaned$organisation.size, p=0.7, list=FALSE)
train_organisation.size <- as.factor(ransomware_cleaned[trainIndex_organisation.size, ])
test_organisation.size <- as.factor(ransomware_cleaned[ -trainIndex_organisation.size, ])

knn.model_ransomware_organisation.size_predict_test <- predict(knn.model_ransomware_organisation.size, train_organisation.size)
nnet.model_ransomware_organisation.size_predict <- predict(nnet.model_ransomware_organisation.size, ransomware_cleaned)
svm.model_ransomware_organisation.size_predict <- predict(svm.model_ransomware_organisation.size, ransomware_cleaned)

confusionMatrix(as.data.frame(knn.model_ransomware_organisation.size_predict_test), as.data.frame(test_organisation.size$revenue), mode = "prec_recall")




