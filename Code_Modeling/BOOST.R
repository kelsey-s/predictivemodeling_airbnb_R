library(gbm)
library(dplyr)
library(ROCR)
#Gradient boosting is a machine learning technique for regression and classification problems, which produces a prediction model in the form of an ensemble of weak prediction models, typically decision trees. It builds the model in a stage-wise fashion like other boosting methods do, and it generalizes them by allowing optimization of an arbitrary differentiable loss function. [wiki]

##gbm(formula = formula(data),
#distribution = "bernoulli",
#data = list(),
#weights,
#var.monotone = NULL,
#n.trees = 100,
#interaction.depth = 1,
#n.minobsinnode = 10,
#shrinkage = 0.001,
#bag.fraction = 0.5,
#train.fraction = 1.0,
#cv.folds=0,
#keep.data = TRUE,
#verbose = "CV",
#class.stratify.cv=NULL,
#n.cores = NULL)
##Currently available options are
#"gaussian" (squared error),
#"laplace" (absolute loss), 
#"tdist" (t-distribution loss),
#"bernoulli" (logistic regression for 0-1 outcomes),
#"huberized" (huberized hinge loss for 0-1 outcomes),
#"multinomial" (classification when there are more than 2 classes), 
#"adaboost" (the AdaBoost exponential loss for 0-1 outcomes), 
#"poisson" (count outcomes), 
#"coxph" (right censored observations), "quantile", or "pairwise" (ranking measure using the LambdaMart algorithm).

##boosting 
boost_data <- airbnb_train_final
#PCA
#PCAAnalysis_test=prcomp(airbnb_test, scale.=TRUE)
#summary(PCAAnalysis_test)
#airbnb_test=PCAAnalysis_test$x[,1:29]
#Convert character into factor
boost_data=boost_data %>% mutate_if(is.character, as.factor)
test_final=test_final %>% mutate_if(is.character, as.factor)
#Split data
set.seed(12345)
test_insts = sample(nrow(boost_data), .3*nrow(boost_data))
airbnb_test = boost_data[test_insts,]
airbnb_train = boost_data[-test_insts,]
airbnb_test1 = airbnb_test[,-42]

test_size <- nrow(airbnb_test)

#interaction.depth refers to the maximum depth of tree allowed
boost.mod <- gbm(high_booking_rate~.,
                 data=airbnb_train,
                 distribution="bernoulli",
                 n.trees=20000,
                 shrinkage=0.01,
                 interaction.depth=3,cv.folds=5,keep.data = TRUE)
##to see the influence of the features(in plot)
summary(boost.mod)
gbm.perf(boost.mod)#see the best times of trees
boost_preds <- predict(boost.mod,newdata=airbnb_test1,type='response',n.trees=gbm.perf(boost.mod))

#see the performance using rocr
pred <- prediction(boost_preds,airbnb_test$high_booking_rate)
tpr.perf = performance(pred, measure = "tpr")
tnr.perf = performance(pred, measure = "tnr")
acc =  performance(pred, measure = "acc")
plot(tpr.perf,ylim=c(0,1))
plot(tnr.perf, add=T)

#find the best cutoff
best = which.max(slot(acc,"y.values")[[1]])
max.acc = slot(acc,"y.values")[[1]][best]
max.cutoff = slot(acc,"x.values")[[1]][best]
print(c(accuracy= max.acc, cutoff = max.cutoff))

#classify with a cutoff and compute accuracy
boost_class <- ifelse(boost_preds>.5,1,0)
boost_acc <- sum(ifelse(boost_class==airbnb_test$high_booking_rate,1,0))/nrow(airbnb_test)
boost_acc
##For test
test_final$is_location_exact[is.na(test_final$is_location_exact)]=1
boost_preds1 <- predict(boost.mod,newdata=test_final,type='response',n.trees=gbm.perf(boost.mod))
boost_class1 <- ifelse(boost_preds1>0.5241141 ,1,0)#manualy set

file=data.frame(boost_class1)
table(file)
colnames(file)='high_booking_rate'
write.csv(file,'Submission_Group6.csv')

