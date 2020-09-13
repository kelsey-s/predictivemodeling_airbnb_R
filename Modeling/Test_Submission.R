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
airbnb_test=airbnb_test_final[,-1]

#Data Scalization in training
airbnb_test$accommodates<-scale(airbnb_test$accommodates,center = TRUE, scale = TRUE)
airbnb_test$availability_30<-scale(airbnb_test$availability_30,center = TRUE, scale = TRUE)
airbnb_test$availability_365<-scale(airbnb_test$availability_365,center = TRUE, scale = TRUE)
airbnb_test$availability_60<-scale(airbnb_test$availability_60,center = TRUE, scale = TRUE)
airbnb_test$availability_90<-scale(airbnb_test$availability_90,center = TRUE, scale = TRUE)
airbnb_test$bedrooms<-scale(airbnb_test$bedrooms,center = TRUE, scale = TRUE)
airbnb_test$beds<-scale(airbnb_test$beds,center = TRUE, scale = TRUE)
airbnb_test$bathrooms<-scale(airbnb_test$bathrooms,center = TRUE, scale = TRUE)
airbnb_test$cleaning_fee<-scale(airbnb_test$cleaning_fee,center = TRUE, scale = TRUE)
airbnb_test$maximum_nights<-scale(airbnb_test$maximum_nights,center = TRUE, scale = TRUE)
airbnb_test$minimum_nights<-scale(airbnb_test$minimum_nights,center = TRUE, scale = TRUE)
airbnb_test$price<-scale(airbnb_test$price,center = TRUE, scale = TRUE)
airbnb_test$security_deposit<-scale(airbnb_test$security_deposit,center = TRUE, scale = TRUE)
airbnb_test$number_of_amenities<-scale(airbnb_test$number_of_amenities,center = TRUE, scale = TRUE)
airbnb_test$number_of_host_verifications<-scale(airbnb_test$number_of_host_verifications,center = TRUE, scale = TRUE)
airbnb_test$first_review_days<-scale(airbnb_test$first_review_days,center = TRUE, scale = TRUE)
airbnb_test$guests_included<-scale(airbnb_test$guests_included,center = TRUE, scale = TRUE)
airbnb_test$host_total_listings_count<-scale(airbnb_test$host_total_listings_count,center = TRUE, scale = TRUE)

airbnb_test$cate_extra_people=as.factor(airbnb_test$cate_extra_people)
airbnb_test$is_business_travel_ready=as.factor(airbnb_test$is_business_travel_ready)

#Dummies if needed



##Model
cv.out <- cv.glmnet(x_train,y_train, family="binomial", alpha=1, lambda=grid, nfolds=k)
bestlam <- cv.out$lambda.min

airbnb_test_final
pred_final = predict(cv.out, s=bestlam, newx = x_test,type="response")
