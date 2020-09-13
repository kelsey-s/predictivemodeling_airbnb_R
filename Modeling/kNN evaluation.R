set.seed(12345)
airbnb_test=read.csv('airbnb_test_final.csv')
airbnb_test=airbnb_test_final[,-1]

#Normalization in training
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
airbnb_test$cate_extra_people=as.integer(as.factor(airbnb_test$cate_extra_people))
airbnb_test$is_business_travel_ready=as.integer(as.factor(airbnb_test$is_business_travel_ready))

#Factor
airbnb_test$bed_type=as.integer(as.factor(airbnb_test$bed_type))
airbnb_test$cancellation_policy=as.integer(as.factor(airbnb_test$cancellation_policy))
airbnb_test$host_response_time=as.integer(as.factor(airbnb_test$host_response_time))
airbnb_test$property_type=as.integer(as.factor(airbnb_test$property_type))
airbnb_test$room_type=as.integer(as.factor(airbnb_test$room_type))
airbnb_test$Topic_Keywords=as.integer(as.factor(airbnb_test$Topic_Keywords))
airbnb_test$is_business_travel_ready=as.integer(as.factor(airbnb_test$is_business_travel_ready))
airbnb_test$is_location_exact=as.integer(as.factor(airbnb_test$is_location_exact))

str(airbnb_test)


#PCA
PCAAnalysis_test=prcomp(airbnb_test, scale.=TRUE)
summary(PCAAnalysis_test)
airbnb_test=PCAAnalysis_test$x[,1:29]

airbnb_train_X=airbnb[,-30]
airbnb_train_y=as.numeric(airbnb$airbnb_y)

knn_pred_final=knn(airbnb_train_X,airbnb_test,airbnb_train_y,k=21)
file=data.frame(knn_pred_final)
colnames(file)='high_booking_rate'
write.csv(knn_pred_final,'Submission_Group6.csv')


