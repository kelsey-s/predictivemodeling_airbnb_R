library(e1071)

set.seed(12345)
airbnb=airbnb_train_final[,-1]
table(airbnb$high_booking_rate)
74758/25091+74758

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
airbnb$cate_extra_people=as.integer(as.factor(airbnb$cate_extra_people))
airbnb$is_business_travel_ready=as.integer(as.factor(airbnb$is_business_travel_ready))

#Factor
airbnb$bed_type=as.integer(as.factor(airbnb$bed_type))
airbnb$cancellation_policy=as.integer(as.factor(airbnb$cancellation_policy))
airbnb$host_response_time=as.integer(as.factor(airbnb$host_response_time))
airbnb$property_type=as.integer(as.factor(airbnb$property_type))
airbnb$room_type=as.integer(as.factor(airbnb$room_type))
airbnb$Topic_Keywords=as.integer(as.factor(airbnb$Topic_Keywords))
airbnb$is_business_travel_ready=as.integer(as.factor(airbnb$is_business_travel_ready))

airbnb_y=airbnb[,41]
airbnb_X=airbnb[,-41]
str(airbnb_X)

PCAAnalysis=prcomp(airbnb_X, scale.=TRUE)
summary(PCAAnalysis)
airbnb_X=PCAAnalysis$x[,1:29]
airbnb=cbind(airbnb_X,airbnb_y)

#Partition
test_insts = sample(nrow(airbnb), .3*nrow(airbnb))
airbnb_test = airbnb[test_insts,]
airbnb_train = airbnb[-test_insts,]
insts = sample(nrow(airbnb_train), .5*nrow(airbnb_train))
test=airbnb_train[insts,]


k=5
svm.tune.out <- tune.svm(airbnb_y~.,data=test, kernel='linear',cost=c(.001,.01,.1,1,10,100,1000),cross=k,probability=TRUE)
svm.tune.out$performances

svm.tune.out$best.parameters
svm.tune.out$best.performance