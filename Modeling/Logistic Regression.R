library(lattice)
library(ggplot2)
library(mlbench)
library(caret)
library(arm)
library(glmnet)
library(dplyr)
library(boot)

#Load Data
set.seed(12345)
airbnb=airbnb_train_final[,-1]

#Normalization in training
airbnb$accommodates<-scale(airbnb$accommodates,center = TRUE, scale = TRUE)
airbnb$availability_30<-scale(airbnb$availability_30,center = TRUE, scale = TRUE)
airbnb$availability_365<-scale(airbnb$availability_365,center = TRUE, scale = TRUE)
airbnb$availability_60<-scale(airbnb$availability_60,center = TRUE, scale = TRUE)
airbnb$availability_90<-scale(airbnb$availability_90,center = TRUE, scale = TRUE)
airbnb$bedrooms<-scale(airbnb$bedrooms,center = TRUE, scale = TRUE)
airbnb$beds<-scale(airbnb$beds,center = TRUE, scale = TRUE)
airbnb$bathrooms<-scale(airbnb$bathrooms,center = TRUE, scale = TRUE)
airbnb$cleaning_fee<-scale(airbnb$cleaning_fee,center = TRUE, scale = TRUE)
airbnb$maximum_nights<-scale(airbnb$maximum_nights,center = TRUE, scale = TRUE)
airbnb$minimum_nights<-scale(airbnb$minimum_nights,center = TRUE, scale = TRUE)
airbnb$price<-scale(airbnb$price,center = TRUE, scale = TRUE)
airbnb$security_deposit<-scale(airbnb$security_deposit,center = TRUE, scale = TRUE)
airbnb$number_of_amenities<-scale(airbnb$number_of_amenities,center = TRUE, scale = TRUE)
airbnb$number_of_host_verifications<-scale(airbnb$number_of_host_verifications,center = TRUE, scale = TRUE)
airbnb$first_review_days<-scale(airbnb$first_review_days,center = TRUE, scale = TRUE)
airbnb$guests_included<-scale(airbnb$guests_included,center = TRUE, scale = TRUE)
airbnb$host_total_listings_count<-scale(airbnb$host_total_listings_count,center = TRUE, scale = TRUE)
airbnb$cate_extra_people=as.factor(airbnb$cate_extra_people)
airbnb$is_business_travel_ready=as.factor(airbnb$is_business_travel_ready)
#Factor
airbnb$bed_type=as.factor(airbnb$bed_type)
airbnb$cancellation_policy=as.factor(airbnb$cancellation_policy)
airbnb$host_response_time=as.factor(airbnb$host_response_time)
airbnb$property_type=as.factor(airbnb$property_type)
airbnb$room_type=as.factor(airbnb$room_type)
airbnb$Topic_Keywords=as.factor(airbnb$Topic_Keywords)
airbnb$is_business_travel_ready=as.factor(airbnb$is_business_travel_ready)
airbnb=airbnb[,-which(colnames(airbnb)%in%c('property_type'))]

num_cols=c('accommodates','availability_30','availability_365','availability_60','availability_90','bedrooms','beds','bathrooms'
           ,'cleaning_fee','maximum_nights','minimum_nights','price','security_deposit','number_of_amenities','number_of_host_verifications',
           'first_review_days','guests_included','host_total_listings_count')
airbnb_num=airbnb[,num_cols]
airbnb_rest=airbnb[,-which(colnames(airbnb)%in%num_cols)]

#PCA
PCAAnalysis=prcomp(airbnb_num, scale.=FALSE)
summary(PCAAnalysis)
airbnb_X=PCAAnalysis$x[,1:14]
airbnb=cbind(airbnb_X,airbnb_rest)


airbnb_y=airbnb[,35]
airbnb_X=airbnb[,-35]
str(airbnb_X)

#Partition
test_insts = sample(nrow(airbnb), .3*nrow(airbnb))
airbnb_test = airbnb[test_insts,]
airbnb_train = airbnb[-test_insts,]

##
model1=glm(high_booking_rate~.,data=airbnb_train,family='binomial')

model1.reduced = step(model1)

pred=predict(model1.reduced,newdata = airbnb_test,type='response')

#Function
confusion_matrix <- function(preds, actuals, cutoff){
  
  classifications <- ifelse(preds>cutoff,1,0)
  
  confusion_matrix <- table(actuals,classifications)
}

class_performance <- function(confusion_matrix){
  
  TP <- confusion_matrix[2,2]
  TN <- confusion_matrix[1,1]
  FP <- confusion_matrix[1,2]
  FN <- confusion_matrix[2,1]
  
  acc <- (TP+TN)/(TP+TN+FP+FN)
  
  return(c(acc))
}

#Evaluation
matrix=confusion_matrix(pred,airbnb_test$high_booking_rate,.5)
performance=class_performance(matrix)
performance
matrix











