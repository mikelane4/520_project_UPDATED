install.packages('caret')
install.packages('e1071')
install.packages("randomForest")
install.packages('mlbench')
install.packages('Hmisc')
install.packages('corrplot')
install.packages('ggcorrplot')
install.packages('https://cran.r-project.org/src/contrib/Archive/randomForest/', repos=NULL, type="source")

#load libraries
library(caret)
library(e1071)
library(mlbench)
library(caret)
library(randomForest)
library(Hmisc)
library(corrplot)
library(ggcorrplot)

# ensure the results are repeatable
set.seed(7)

# Get attributes that are highly correlated

# Read the data from the csv file
# DERBY dataset with 33 columns
dataset <- read.csv('./DERBY5.csv' , sep = ',')
dataset_spambase <- read.csv('./spambase.csv' , sep = ',')

# load the data
# calculate correlation matrix
# See library https://www.rdocumentation.org/packages/caret/versions/6.0-93/topics/findCorrelation
dataset_spambase$x <- NULL
correlationMatrix_spambase <- cor(dataset_spambase[,1:58])

# summarize the correlation matrix
print(correlationMatrix)

#%%%%%%%%%%%%%%%%%%%%%%%%%%% un-comment lines 43-47when using spambase dataset %%%%%%%%%%%%%%%%%%%%%%%%%
# change type to numerical (0 for nonspam and 1 for spam)
dataset_spambase[dataset_spambase =='nonspam'] <- as.numeric(0)             
dataset_spambase[dataset_spambase =='spam'] <- as.numeric(1)                
dataset_spambase$type = as.numeric(dataset_spambase$type)                   
str(dataset_spambase)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#get correlation matrix - representation 1
visCorMatrix1 <- corrplot(cor(dataset_spambase))

#get correlation matrix - representation 2
visCorMatrix2<-ggcorrplot(cor(dataset_spambase))
visCorMatrix2

# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5,verbose = FALSE,names = TRUE )
highlyCorrelated_spambase <- findCorrelation(correlationMatrix_spambase, cutoff=0.5,verbose = FALSE,names = TRUE )

# print highly correlated attributes
print(highlyCorrelated_spambase)

# ensure results are repeatable
set.seed(7)

# prepare training scheme
control_spambase <- trainControl(method="repeatedcv", number=10, repeats=3)

# train the model
model_spambase <- train(as.factor(type)~., data=dataset_spambase, method="lvq", preProcess="scale", trControl=control_spambase)

# estimate variable importance
# This will take a minute
importance <- varImp(model_spambase, scale=FALSE)

# summarize importance
# This will take a minute
print(importance)

# plot importance
plot(importance)

#Recursive Feature Elimination (RFE) ---------------------------------
# ensure the results are repeatable
set.seed(7)

# load the library
library(mlbench)
library(caret)

# load the data
# define the control using a random forest selection function
# This will take a moment.FYI.
control_spambaser <- rfeControl(functions=rfFuncs, method="cv", number=10)

# run the RFE algorithm
results <- rfe(dataset_spambase[,1:57], dataset_spambase[,58], sizes=c(1:57), rfeControl=control_spambaser)

# summarize the results
# this will take a hot minute
print(results)
# list the chosen features
predictors(results)
predictors <- data.frame(predictors(results))

# plot the results
plot(results, type=c("g", "o"))

#Question 9 submission

set.seed(7)
trainctrl_spambase <- trainControl(method = 'cv', number = 10, verboseIter = TRUE)

#spambase_all_data <- data.frame[dataset_spambase, importance, predictors] - doesn't work

#knn, nnet, and svm models
knn.model_spambase1 <- train(as.factor(type)~., data = dataset_spambase, method="knn",
                              tuneLength = 10,
                              trControl = trainctrl_spambase,
                              metric="Accuracy")

nnet.model_spambase2 <- train(as.factor(type)~., data = dataset_spambase, method="nnet",
                   tuneLength = 10,
                   trControl = trainctrl_spambase,
                   metric="Accuracy")

svm.model_spambase3 <- train(as.factor(type)~., data = dataset_spambase, method="svmRadial",
                              tuneLength = 10,
                              trControl = trainctrl_spambase,
                              metric="Accuracy")

knn.model_spambase1
nnet.model_spambase2
svm.model_spambase3    

#Predictions
knn.model_spambase1_predict <- predict(knn.model_spambase1, dataset_spambase)
nnet.model_spambase2_predict <- predict(nnet.model_spambase2, dataset_spambase)
svm.model_spambase3_predict <- predict(svm.model_spambase3, dataset_spambase)

#Results
confusionMatrix(knn.model_spambase1_predict, as.factor(dataset_spambase$type), mode = "prec_recall")
confusionMatrix(nnet.model_spambase2_predict, as.factor(dataset_spambase$type), mode = "prec_recall")
confusionMatrix(svm.model_spambase3_predict, as.factor(dataset_spambase$type), mode = "prec_recall")

#Results plot
plot(knn.model_spambase1)
plot(nnet.model_spambase2)
plot(svm.model_spambase3)

#Simple plot for as.numeric(spam(1)/nonspam(0))
plot(knn.model_spambase1_predict)
plot(nnet.model_spambase2_predict)
plot(svm.model_spambase3_predict)

#KNN Results
#Accuracy : 0.8696 - 86.96%
#Recall : 0.8945 - 89.45%

#NNET Results
#Accuracy : 0.9591 - 95.91%
#Recall : 0.9681 - 96.81%

#SVM Results
#Accuracy : 0.9583 - 95.83%
#Recall : 0.9774 -97.74%
