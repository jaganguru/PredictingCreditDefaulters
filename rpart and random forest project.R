setwd("~/LAB09")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RcolorBrewer")
library("rpart")
library("rpart.plot")
library("rattle")
library(RColorBrewer)

#Read the data

DTcreditdf<-read.csv("dtcreditnew.csv")
str(DTcreditdf)
DTcreditdf$default.payment.next.month[DTcreditdf$default.payment.next.month==0]<-"NO"
DTcreditdf$default.payment.next.month[DTcreditdf$default.payment.next.month==1]<-"YES"

DTcreditdf$default.payment.next.month<-factor(DTcreditdf$default.payment.next.month)
DTcreditdf$EDUCATION<-factor(DTcreditdf$EDUCATION)

DTtraining<-DTcreditdf[1:21000,]
DTtesting<-DTcreditdf[21001:30000,]

#Build the tree to "fit" the model


fit<-rpart(default.payment.next.month~.,data=DTtraining,minsplit=2,minbucket=1,method="class",cp=0.002)


#Plot the tree
fancyRpartPlot(fit,main="Rpart Decision Tree")

#Predict if a customer is going default his/her payment next month.

newdata<-DTtesting

prediction<-predict(fit,newdata=newdata,type=c("class"))

#validation of the rpart tree

confusionMatrix(DTtesting$default.payment.next.month,data=prediction)

#ROC Curve & AUC

dt_fit = predict(fit, type="prob") # this returns the probability scores on the training data
dtObj = prediction(dt_fit[,2], DTtraining$default.payment.next.month) # prediction object needed by ROCR

dtrocObj = performance(dtObj, measure="tpr", x.measure="fpr")  # creates ROC curve obj
dtaucObj = performance(dtObj, measure="auc")  # auc object

dtauc = dtaucObj@y.values[[1]]  
dtauc   # the auc score

# Plot the ROC curve
plot(dtrocObj, col = "blue", lwd = 1, main=paste("AUC:",dtauc))
abline(a=0,b=1)




#######################################################################################################

#Random Forest

DTcreditdf$PAY_0<-factor(DTcreditdf$PAY_0)
DTcreditdf$PAY_2<-factor(DTcreditdf$PAY_2)
DTcreditdf$PAY_3<-factor(DTcreditdf$PAY_3)
DTcreditdf$PAY_4<-factor(DTcreditdf$PAY_4)
DTcreditdf$PAY_5<-factor(DTcreditdf$PAY_5)
DTcreditdf$PAY_6<-factor(DTcreditdf$PAY_6)

rfmodel <- randomForest(default.payment.next.month~ .,data = DTtraining)
summary(rfmodel)

rfpred <- predict(rfmodel, newdata = DTtesting,type="response")


confusionMatrix(data=rfpred,DTtesting$default.payment.next.month)

#ROC Curve & AUC

rf_fit = predict(rfmodel, type="prob") # this returns the probability scores on the training data
rfObj = prediction(rf_fit[,2], DTtraining$default.payment.next.month) # prediction object needed by ROCR

rfrocObj = performance(rfObj, measure="tpr", x.measure="fpr")  # creates ROC curve obj
rfaucObj = performance(rfObj, measure="auc")  # auc object

rfauc = rfaucObj@y.values[[1]]  
rfauc   # the auc score

# Plot the ROC curve
plot(rfrocObj, col = "blue", lwd = 1, main=paste("AUC:",rfauc))
abline(a=0,b=1)

#######################################################################################################
#Naive Bayes Classifier

library(e1071)
nbmodel <- naiveBayes(default.payment.next.month ~.,DTtraining,laplace=.01)
nbmodel
results <- predict (nbmodel,DTtesting)
results

confusionMatrix(data=results,DTtesting$default.payment.next.month)

