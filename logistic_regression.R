library(caret)
library(ROCR)
library(ggplot2)
library(corrplot)
library(e1071)
library(randomForest)

CreditData<-read.csv("UCI_Credit_Card.csv")   #Importing the csv data into a R dataframe

CreditData<-CreditData[,2:ncol(CreditData)]#Removing the ID column from the dataframe

corrplot(cor(CreditData[1:24]))

#Data Exploration

head(CreditData)
str(CreditData)
table(CreditData$SEX,CreditData$default.payment.next.month)
table(CreditData$MARRIAGE,CreditData$default.payment.next.month)
prop1<-prop.table(table(CreditData$default.payment.next.month))
prop1

#Converting SEX, MARRIAGE and EDUCATION as Factors

CreditData$SEX<-factor(CreditData$SEX)
CreditData$EDUCATION<-factor(CreditData$EDUCATION)
CreditData$MARRIAGE<-factor(CreditData$MARRIAGE)
CreditData$default.payment.next.month<-factor(CreditData$default.payment.next.month)
str(CreditData)

#Splitting the Data into training and test data set.

traindata<-CreditData[1:21000,]
testdata<-CreditData[21001:30000,]

#Logistic Regression Model building

model1<-glm(default.payment.next.month~.,data=traindata,family=binomial)
summary(model1)



predauc = predict(model1, type="response") # this returns the probability scores on the training data
predObj = prediction(predauc, traindata$default.payment.next.month) # prediction object needed by ROCR

rocObj = performance(predObj, measure="tpr", x.measure="fpr")  # creates ROC curve obj
aucObj = performance(predObj, measure="auc")  # auc object

auc = aucObj@y.values[[1]]  
auc   # the auc score

corrplot(cor(CreditData[5:23]))
#Re-modelling with only significant values

model2<-glm(default.payment.next.month ~ LIMIT_BAL+SEX+MARRIAGE+EDUCATION+PAY_0+PAY_2+PAY_3+PAY_AMT1+PAY_AMT2+BILL_AMT1+BILL_AMT2, 
            data=traindata,family=binomial)
summary(model2)



#Building the Confusion Matrix
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
model2_fit <- train(default.payment.next.month ~ LIMIT_BAL+SEX+MARRIAGE+EDUCATION+PAY_0+PAY_2+PAY_3+PAY_AMT1+PAY_AMT2+BILL_AMT1+BILL_AMT2, data=traindata,
                    family="binomial",trControl=ctrl,tuneLength=5,method="glm")



testdata$pred = predict(model2_fit, newdata=testdata)
confusionMatrix(data=pred, testdata$default.payment.next.month)

#ROC curve

predauc1 = predict(model2, type="response") # this returns the probability scores on the training data
predObj = prediction(predauc1, train$default.payment.next.month) # prediction object needed by ROCR

rocObj = performance(predObj, measure="tpr", x.measure="fpr")  # creates ROC curve obj
aucObj = performance(predObj, measure="auc")  # auc object

auc = aucObj@y.values[[1]]  
auc   # the auc score

# plot the roc curve
plot(rocObj, main = paste("Area under the curve:", auc))
abline(a=0,b=1,col="red")

######################################################################################################

#NB Classifier

library(e1071)
model <- naiveBayes(default.payment.next.month ~.,traindata,laplace=.01)
model
results <- predict (model,testdata)
#results
#results1 <- predict (model,testdata,type="raw")
#results1

confusionMatrix(data=results,testdata$default.payment.next.month)

# Random Forest 

rfmodel <- randomForest(default.payment.next.month~ .,data = traindata)
summary(rfmodel)

rfpred <- predict(rfmodel, newdata = testdata)
table(rfpred, testdata$default.payment.next.month)

confusionMatrix(data=rfpred,testdata$default.payment.next.month)



#rf_fit <- train(default.payment.next.month ~ LIMIT_BAL+SEX+MARRIAGE+EDUCATION+PAY_0+PAY_2+PAY_3+PAY_AMT1+PAY_AMT2+BILL_AMT1+BILL_AMT2, data=traindata,
#                    family="binomial",trControl=ctrl,tuneLength=5,method="rf")

#predrf = predict(rf_fit, newdata=testdata)
#confusionMatrix(data=predrf, testdata$default.payment.next.month)
#summary(testdata$default.payment.next.month)
