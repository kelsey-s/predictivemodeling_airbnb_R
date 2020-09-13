#install.packages('xgboost')
#install.packages('mlr')

install.packages("caret")
install.packages("car")
install.packages('carData')

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(carData)
library(car)
library(Matrix)
library(dplyr)
library(stats)
library(data.table)
library(mlr)

airbnb_train_final=airbnb_train_final[,-1]
airbnb_test_final=airbnb_test_final[,-1]
airbnb_test_final$is_location_exact[is.na(airbnb_test_final$is_location_exact)]=1

set.seed(12345)
test_insts = sample(nrow(airbnb_train_final), .3*nrow(airbnb_train_final))
test = airbnb_train_final[test_insts,]
train = airbnb_train_final[-test_insts,]

setDT(train) 
setDT(test)
setDT(airbnb_test_final)

#Train_train &Train_test label
tune_train_labels = train$high_booking_rate
tune_train_labels = as.numeric(tune_train_labels)
ts_tune_label <- test$high_booking_rate
ts_tune_label = as.numeric(ts_tune_label)
all_label=airbnb_train_final$high_booking_rate
all_label = as.numeric(all_label)

all=rbind(train[,-41],test[,-41],airbnb_test_final)
all_matrix=model.matrix(~.+0,data = all)
new_tr=all_matrix[1:69895,]
new_ts=all_matrix[69896:99849,]
new_final_ts=all_matrix[99850:112057,]
new_final_train=all_matrix[1:99849,]


dtrain <- xgb.DMatrix(data = new_tr,label = tune_train_labels) 
dtest <- xgb.DMatrix(data = new_ts,label=ts_tune_label)
new_final_ts <- xgb.DMatrix(data = new_final_ts)
new_final_train=xgb.DMatrix(data = new_final_train,label=all_label)

###
best_param = list()
best_seednumber = 12345
best_error = Inf
best_error_index = 0

for (iter in 1:50) {
  param = list(objective = "multi:softprob",
               eval_metric = 'merror',
               num_class = 2,
               max_depth = sample(6:10, 1),
               eta = runif(1, .01, .3),
               gamma = runif(1, 0.0, 0.2), 
               subsample = runif(1, .6, .9),
               colsample_bytree = runif(1, .5, .8), 
               min_child_weight = sample(1:40, 1),
               max_delta_step = sample(1:10, 1)
  )
  cv.nround = 100
  cv.nfold = 5
  seed.number = sample.int(100, 1)[[1]]
  set.seed(seed.number)
  mdcv = xgb.cv(data=dtrain, booster="gbtree",params = param, nthread=6, 
                nfold=cv.nfold, nrounds=cv.nround,
                verbose = T, early_stopping_rounds=8, maximize=FALSE)
  
  min_error = min(mdcv[, test.merror.mean])
  min_error_index = which.min(mdcv$evaluation_log[,test_error_mean])
  
  if (min_error < best_error) {
    best_error = min_error
    best_error_index = min_error_index
    best_seednumber = seed.number
    best_param = param
  }
}

nround = best_error_index
set.seed(best_seednumber)
md = xgb.train(data=dtrain, params=best_param, nrounds=nround, nthread=6)
pred = predict(md,dtest)
pred

#Final
for (iter in 1:50) {
  param = list(objective = "multi:softmax",
               eval_metric = 'merror',
               num_class = 2,
               max_depth = sample(6:10, 1),
               eta = runif(1, .01, .3),
               gamma = runif(1, 0.0, 0.2), 
               subsample = runif(1, .6, .9),
               colsample_bytree = runif(1, .5, .8), 
               min_child_weight = sample(1:40, 1),
               max_delta_step = sample(1:10, 1)
  )
  cv.nround = 100
  cv.nfold = 5
  seed.number = sample.int(100, 1)[[1]]
  set.seed(seed.number)
  mdcv = xgb.cv(data=new_final_train, booster="gbtree",params = param, nthread=6, 
                nfold=cv.nfold, nrounds=cv.nround,
                verbose = T, early_stopping_rounds=8, maximize=FALSE)
  
  min_error = min(mdcv[, test.merror.mean])
  min_error_index = which.min(mdcv$evaluation_log[,test_error_mean])
  
  if (min_error < best_error) {
    best_error = min_error
    best_error_index = min_error_index
    best_seednumber = seed.number
    best_param = param
  }
}

nround = best_error_index
set.seed(best_seednumber)
md = xgb.train(data=new_final_train, params=best_param, nrounds=nround, nthread=6)
pred = predict(md,new_final_ts)
pred










params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
##best iteration = 76
xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)

#first default - model training
xgb1 <- xgb.train (params = params, data = new_final_train, nrounds = 79, print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")
prediction=predict(xgb1,new_final_ts)

#
xgb2 <- xgb.train (params = params, data = dtrain, nrounds = 91, print.every.n = 10, early.stop.round = 10, maximize = F , eval_metric = "error")
prediction2=predict(xgb1,dtest)


xbs2$
xgb1

##Train model
best_param = list()
best_seednumber = 12345
best_error = Inf
best_error_index = 0

for (iter in 1:50) {
  params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
  cv.nround = 100
  cv.nfold = 5
  seed.number = sample.int(100, 1)[[1]]
  set.seed(seed.number)
  xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = cv.nround, nfold = cv.nfold, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)
  min_error = min(xgbcv[,test.merror.mean])
  min_error_index = which.min(mdcv[, test.merror.mean])
  
  if (min_error < best_error) {
    best_error = min_error
    best_error_index = min_error_index
    best_seednumber = seed.number
    best_param = param
  }
}

nround = best_error_index
set.seed(best_seednumber)
md = xgb.train(data=dtrain_tune, params=best_param, nrounds=nround, nthread=6)
pred = predict(md,dtest_tune)
confusionMatrix(pred$class, airbnb_tune_test_y_m)