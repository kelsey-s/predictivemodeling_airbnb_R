#install.packages('xgboost')
#install.packages('mlr')

library(xgboost)
library(stringr)
library(caret)
library(car)
library(stats)
library(data.table)
library(mlr)
library(dplyr)
library(mlr)
library(parallel)
library(parallelMap) 

set.seed(12345)

airbnb_train_final=airbnb_train_final[,-1]
airbnb_test_final=airbnb_test_final[,-1]

##Convert Train Type
airbnb_train_final=airbnb_train_final %>% mutate_if(is.character, as.factor)
airbnb_test_final=airbnb_test_final %>% mutate_if(is.character, as.factor)
airbnb_train_final$high_booking_rate=as.factor(airbnb_train_final$high_booking_rate)

airbnb_train_X=airbnb_train_final[,-41]
airbnb_train_y=airbnb_train_final[,41]

airbnb=rbind(airbnb_train_X,airbnb_test_final)

#Create Dummy
airbnb_dummy = createDummyFeatures (obj = airbnb) 
airbnb_train_X_new=airbnb_dummy[1:99849,]
airbnb_train=cbind(airbnb_train_X_new,airbnb_train_y)

#Final Test Dataset
airbnb_test=airbnb_dummy[99850:112057,]

#Train-test-split
test_insts = sample(nrow(airbnb_train), .3*nrow(airbnb_train))
airbnb_tune_test = airbnb_train[test_insts,]
airbnb_tune_train = airbnb_train[-test_insts,]

#ClassifTask
traintask = makeClassifTask (data = airbnb_tune_train,target = "high_booking_rate")
testtask = makeClassifTask (data = airbnb_tune_test,target = "high_booking_rate")

#create learner
lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="error", nrounds=100L, eta=0.1)

#set parameter space
params <- makeParamSet( makeDiscreteParam("booster",values = c("gbtree","dart")), makeIntegerParam("max_depth",lower = 3L,upper = 10L), makeNumericParam("min_child_weight",lower = 1L,upper = 10L), makeNumericParam("subsample",lower = 0.5,upper = 1), makeNumericParam("colsample_bytree",lower = 0.5,upper = 1))

#set resampling strategy
rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)
ctrl <- makeTuneControlRandom(maxit = 10L)

parallelStartSocket(cpus = detectCores())

#parameter tuning
mytune <- tuneParams(learner = lrn, task = traintask, resampling = rdesc, measures = acc, par.set = params, control = ctrl, show.info = T)
mytune$y 

#set hyperparameters
lrn_tune <- setHyperPars(lrn,par.vals = mytune$x)

#train model
xgmodel <- train(learner = lrn_tune,task = traintask)

#predict model
xgpred <- predict(xgmodel,testtask)
   
confusionMatrix(xgpred$data$response,xgpred$data$truth)
#0.8406

#Final Prediction
final_traintask = makeClassifTask (data = airbnb_train, target = "high_booking_rate")
final_testtask = makeClassifTask(data=airbnb_test,target=)

xgmodel_final = train(learner = lrn_tune, data = airbnb_train[,-146],label=airbnb_train[,146])
xgpred_final = predict(xgmodel,final_testtask)


bike_model_xgb <- xgboost(data = as.matrix(airbnb_train_final.treat), # training data as matrix
                          label = airbnb_train_final$high_booking_rate,  # column of outcomes
                          nrounds = 1,       # number of trees to build
                          eta = 0.1,
                          max_depth = 15, 
                          nround=25, 
                          subsample = 0.5,
                          colsample_bytree = 0.5,
                          seed = 1,
                          eval_metric = "error",
                          objective = "multi:softprob",
                          num_class = 12
)


#Save csv
file=data.frame(xgpred_final)
colnames(file)='high_booking_rate'
write.csv(file,'Submission_XGBoost_Group6.csv')



