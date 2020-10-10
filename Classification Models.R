library(lattice)
library(ggplot2)
library(caret)
library(plyr)
library(dplyr)
library(C50)
library(kernlab)
library(RWeka)
library(mlbench)
library(foreign)
library(scales)
library(reshape)
library(e1071)
library(klaR)
library(mlr)
library(caretEnsemble)

trainingData <- read.csv("C:\\Users\\ludai\\Desktop\\BAN620\\Assignment3\\bank-training.csv", row.names = 1)
testData<-read.csv("C:\\Users\\ludai\\Desktop\\BAN620\\Assignment3\\bank-test.csv", row.names = 1)
nrow(trainingData)
nrow(testData)
prop.table(table(trainingData$y))
prop.table(table(testData$y))

#1. Classification Methods
#a) Decision Tree
#Training the Model
#After partitioning the data to train and test, use a 10 fold cross validation repeated 5 times to evaluate the model.

TrainingParameters <- trainControl(method = "cv", number = 10, repeats = 5)


DecTreeModel <- caret::train(trainingData[,-20], trainingData$y, 
                      method = "C5.0",
                      trControl= TrainingParameters,
                      na.action = na.omit
                      )
DecTreeModel
summary(DecTreeModel)

#Testing the Model
DTPredictions <-predict(DecTreeModel, testData, na.action = na.pass)
confusionMatrix(DTPredictions, testData$y)
confusionMatrix



# b) Naive Bayes

set.seed(100)

NBModel <- caret::train(trainingData[,-20], trainingData$y, method = "nb",trControl= trainControl(method = "cv", number = 10, repeats = 5))
NBModel

NBPredictions <-predict(NBModel, testData)
confusionMatrix(NBPredictions, testData$y)


# c) Suppor Vector Machines (SVM)

set.seed(120)

svm_model <- caret::train(y~.,data=trainingData,
   method = "svmPoly",
   trControl= trainControl(method = "cv", number = 10, repeats = 5),
   tuneGrid = data.frame(degree = 1,scale = 1,C = 1))
svm_model


SVMPredictions <-predict(svm_model, testData, na.action = na.pass)
confusionMatrix(SVMPredictions, testData$y)

# d) Neural Network


nnmodel <- caret::train(trainingData[,-20], trainingData$y, method = "nnet",
                 trControl= trainControl(method = "cv", number = 10, repeats = 5))
nnmodel

nnetpredictions <-predict(nnmodel, testData, na.action = na.pass)
confusionMatrix(nnetpredictions, testData$y)

t <- matrix(cbind(c(0.8989,0.9935,0.1250,0.9029),c(0.8863,0.9498,0.3661,0.9246),
    c(0.8941,0.9880,0.1250,0.9024),c(0.8960,0.9880,0.1429,0.9042)),
    nrow=4,dimnames=list(c('Accuracy','Sensitivity/Recall','Specificity','Precision'),
    c('DecTree','NB','SVM','NN')))

#Q2
model = c("DecTree","NB","SVM","NN")
recall = c(0.9935,0.9498,0.9880,0.9880)
precision = c(0.9029,0.9246,0.9024,0.9042)
fscore <- 2 * precision * recall / (precision + recall)
fscore_table = data.frame(model,recall,precision,fscore) 
fscore_table
beta=as.numeric(0.5)
x=beta^2
x
beta=as.numeric(2)
x
fmeasure=((1+x)*precision*recall)/(x*precision+recall)
fmeasure_table=data.frame(model,recall,precision,fmeasure)
fmeasure_table


# Q3

library(C50)
cmatrix <- cbind(c(0,1), c(10,0))
cmatrix
trainingData <- read.csv("C:\\Users\\ludai\\Desktop\\BAN620\\Assignment3\\bank-training.csv", row.names = 1)
testData<-read.csv("C:\\Users\\ludai\\Desktop\\BAN620\\Assignment3\\bank-test.csv", row.names = 1)

CostDTModel <- C5.0(y ~., data=trainingData, cost=cmatrix)
CostDTModel
summary(CostDTModel)

TrainingParameters <- trainControl(method = "cv", number = 10)
CostDTModel <- caret::train(y ~., data=trainingData, 
                     method = "C5.0Cost",
                     trControl= TrainingParameters,
                     tuneGrid = data.frame(trials = 10, winnow=TRUE, model = "tree", cost = 10),                     
                     na.action = na.omit
)

CostDTModel  
PredictionCostModel <-predict(CostDTModel, testData,na.action = na.pass)
cm<- confusionMatrix(PredictionCostModel, testData$y)
cm



library(C50)
cmatrix <- cbind(c(0,1), c(10,0))
cmatrix
trainingData <- read.csv("C:\\Users\\ludai\\Desktop\\BAN620\\Assignment3\\bank-training.csv", row.names = 1)
testData<-read.csv("C:\\Users\\ludai\\Desktop\\BAN620\\Assignment3\\bank-test.csv", row.names = 1)
CostDTModel <- C5.0(y ~., data=trainingData,cost=cmatrix)
CostDTModel
summary(CostDTModel)
PredictionCostModel <-predict(CostDTModel, testData,na.action = na.pass)
confusionMatrix(PredictionCostModel, testData$y)


# extra
#1
econtrol <- trainControl(method="cv", number=5, summaryFunction = twoClassSummary, savePredictions=TRUE, classProbs=TRUE) 

models <- caretList(y ~., data=trainingData,
                    methodList=c("nnet", "nb"),
                    trControl = econtrol
)

results <- resamples(models)

summary(results)
mcr <-modelCor(results)
mcr
splom(results)

#2
# subset models to use
smallmodels <- c(models$nb,models$nnet)

# Create stacked models. Try rpart

enstackmodel <- caretStack(models, method = "rpart")
print(enstackmodel)

enstackmodel <- caretStack(models, method = "C5.0", metric="Sens",trControl = trainControl(number = 5, summaryFunction = twoClassSummary,classProbs = TRUE))
print(enstackmodel)

#Predict
enstackpredictions <-predict(enstackmodel, testData, na.action = na.omit)

# Create confusion matrix
cmBank <-confusionMatrix(enstackpredictions, testData$y, mode="everything", positive = "no")
cmBank
cmBank$table
?confusionMatrix
# Lets create ensemble model. Note to use subset replace models with smallmodels 

ensmodel <- caretEnsemble(models,
                          metric = "Accuracy",
                          trControl = trainControl(method="cv", number = 10, classProbs = TRUE)
)
?confusionMatrix
summary(ensmodel)
enstackpredictions <-predict(ensmodel, testData, na.action = na.omit)
cmBank <-confusionMatrix(enstackpredictions, testData$y, mode="everything", positive = "no")
cmBank

#3
# Create models with sensitivity as optimization metric instead of accuracy
ensmodel2 <- caretEnsemble(models,
                           metric = "Sens",
                           trControl = trainControl(number = 2, summaryFunction = twoClassSummary,classProbs = TRUE)
)

summary(ensmodel2)
enstackpredictions <-predict(ensmodel2, testData, na.action = na.omit)
cmBank2 <-confusionMatrix(enstackpredictions, testData$y, mode="everything", positive = "no")
cmBank2

# Cost sensitive 

library(C50)

cmatrix <- cbind(c(0,10), c(1,0))
cmatrix
?C5.0

CostDTModel <- C5.0(y ~., data=trainingData, cost=cmatrix)
CostDTModel



CostDTModel  
PredictionCostModel <-predict(CostDTModel, testData,na.action = na.pass)
confusionMatrix(PredictionCostModel, testData$y)
?trainControl
?train
