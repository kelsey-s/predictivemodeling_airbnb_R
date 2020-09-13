library(class)

set.seed(12345)
airbnb=read.csv('airbnb_train_final.csv')
airbnb=airbnb[,-1]

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
airbnb=data.frame(airbnb)

#Partition
test_insts = sample(nrow(airbnb), .3*nrow(airbnb))
airbnb_test = airbnb[test_insts,]
airbnb_train = airbnb[-test_insts,]

train_X=airbnb_train[,-30]
test_X=airbnb_test[,-30]
train_y=as.numeric(airbnb_train$airbnb_y)
test_y=as.numeric(airbnb_test$airbnb_y)

#Cross Validation Tunning
grid_knn <- c(11,21,31,41,51,61) #define values for k (of kNN)
num_folds <- 5 #how many folds you want
folds <- cut(seq(1,nrow(train_X)),breaks=num_folds,labels=FALSE) ##cuts your data into num_folds folds

for (kval in grid_knn){ #for each value in the grid
  total_acc <- 0
  for(i in 1:num_folds){ #repeat for each fold
    ##divide data into training and validation
    valid_insts <- which(folds==i,arr.ind=TRUE) 
    valid_data <- train_X[valid_insts,]
    train_data <- train_X[-valid_insts,]
    sub_valid_y <- train_y[valid_insts]
    sub_train_y <- train_y[-valid_insts]
    
    ##learn your knn on the training data and make predictions on the validation data
    knn.pred_va <- knn(train_data,valid_data,sub_train_y,k=kval)
    
    ##measure the performance
    correct <- sum(ifelse(knn.pred_va==sub_valid_y,1,0))
    accuracy <- (1.0*correct)/nrow(valid_data)
    
    ##sum the accuracy over the folds
    total_acc = total_acc+accuracy
  }
  ##print the complexity vs. the mean accuracy
  print(c("k = ",kval,", mean accuracy = ",total_acc/num_folds))
  
}

##Best k=21
summary(test_X)
knn_pred_final=knn(train_X,test_X,train_y,k=21)
correct_final = sum(ifelse(knn_pred_final==test_y,1,0))
accuracy_final = (1.0*correct_final)/nrow(test_X)
accuracy_final


