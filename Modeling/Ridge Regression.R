library(lattice)
library(ggplot2)
library(mlbench)
library(caret)
library(arm)
library(glmnet)
library(dplyr)

#Load Data
set.seed(12345)
airbnb=airbnb_train_final[,-1]

#Normalization in training
airbnb$cate_extra_people=as.factor(airbnb$cate_extra_people)
airbnb$is_business_travel_ready=as.factor(airbnb$is_business_travel_ready)

#Partition
test_insts = sample(nrow(airbnb_3), .3*nrow(airbnb_3))
airbnb_test = airbnb_3[test_insts,]
airbnb_train = airbnb_3[-test_insts,]


#Define Parameters
grid <- 10^seq(10,-2,length=100)
k=10
x_train= model.matrix(high_booking_rate~.,airbnb_train)
x_test= model.matrix(high_booking_rate~.,airbnb_test)
y_train= airbnb_train$high_booking_rate
y_test=airbnb_test$high_booking_rate

##Ridge Regression Model
cv.out <- cv.glmnet(x_train,y_train, family="binomial", alpha=1, lambda=grid, nfolds=k)
bestlam <- cv.out$lambda.min
pred = predict(cv.out, s=bestlam, newx = x_test,type="response")

##Functions
confusion_matrix <- function(preds, actuals, cutoff){
  
  classifications <- ifelse(preds>cutoff,1,0)
  
  confusion_matrix <- table(actuals,classifications)
}

##Classification Function
class_performance <- function(confusion_matrix){
  
  TP <- confusion_matrix[2,2]
  TN <- confusion_matrix[1,1]
  FP <- confusion_matrix[1,2]
  FN <- confusion_matrix[2,1]
  
  acc <- (TP+TN)/(TP+TN+FP+FN)
  
  return(c(acc))
}

##Get the accuracy
cm=confusion_matrix(pred,y_test,0.5)
cv2=class_performance(cm) 
cv2


