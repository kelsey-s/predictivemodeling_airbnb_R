##Random Forest
install.packages('randomForest')
install.packages('party')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('Gifi')
library(randomForest)
library(dplyr)
library(party)
library(rpart)
library(rpart.plot)

set.seed(12345)
airbnb=airbnb_train_final[,-1]

airbnb=airbnb %>% mutate_if(is.character, as.factor)
airbnb$high_booking_rate=factor(airbnb$high_booking_rate)


#Split data
test_insts = sample(nrow(airbnb), .3*nrow(airbnb))
airbnb_test = airbnb[test_insts,]
airbnb_train = airbnb[-test_insts,]


#Tree
tree = rpart(high_booking_rate ~ ., data=airbnb_train, method="class", parms=list(split="information"))

tree$cptable #CP=0.01,xstd minimum
plotcp(tree)

tree.pruned <- prune(tree, cp=.01)
rpart.plot(tree.pruned, type=2, extra=104,fallen.leaves = TRUE, main="Decision Tree")

tree.pred <- predict(tree.pruned, airbnb_test, type="class")
tree.perf <- table(airbnb_test$high_booking_rate, tree.pred,dnn=c("Actual", "Predicted"))
tree.perf
#Predicted
#Actual     0     1
#0       20081  2388
#1        4060  3425
tree_acc=(20081+3425)/(20081+2388+4060+3425)
tree_acc


#Conditional Tree
ctree_model <- ctree(high_booking_rate ~ ., data=airbnb_train)
plot(ctree_model, main="Conditional Inference Tree")

ctree.pred <- predict(ctree_model, airbnb_test, type="response")
ctree.perf <- table(airbnb_test$high_booking_rate, ctree.pred, dnn=c("Actual", "Predicted"))
ctree.perf
#  Predicted
#Actual     0     1
#0        20559  1910
#1        4478  3007
ctree_acc=(20559+1910)/(20559+1910+4478+3007)
ctree_acc


#Random Forest
set.seed(1234)
inst=sample(nrow(airbnb_train),0.5*nrow(airbnb_train))
airbnb_subset=airbnb_train[inst,]

#Using a subset to train best mtry
n<-length(names(airbnb_subset))
rate=1
for(i in 1:(n-1)){
  set.seed(1234)
  rf_train<-randomForest(airbnb_subset$high_booking_rate~.,data=airbnb_subset,mtry=i,ntree=1000)
  rate[i]<-mean(rf_train$err.rate)   #Calculate the mean of error rate based on OOB
  print(rf_train)    
}
##Best mtry=6 with OOB estimate of error rate: 17.38%
plot(rate)


##Random Forest
rf_train<-randomForest(airbnb_train$high_booking_rate~.,data=airbnb_train,mtry=6,ntree=1000)
plot(rf_train)
legend(800,0.02,"high_booking_rate=0",cex=0.9,bty="n")    
legend(800,0.0245,"total",cex=0.09,bty="n") 
##Best ntree=1000

#Final Tree
rf_preds = predict(rf_train,newdata=airbnb_test[,-41])
rf_acc=sum(ifelse(rf_preds==airbnb_test$high_booking_rate,1,0))/nrow(airbnb_test)
rf_acc##0.8320091

#Evaluation
test=airbnb_test_final[,-1]
test=test %>% mutate_if(is.character, as.factor)
test$is_location_exact[is.na(test$is_location_exact)]=1
levels(test$Topic_Keywords) <- levels(airbnb$Topic_Keywords)
levels(test$property_type) <- levels(airbnb$property_type)
levels(test$cancellation_policy) <- levels(airbnb$cancellation_policy)


##
rf_final = randomForest(high_booking_rate~.,data=airbnb,mtry=6,ntree=1000)#修改n
rf_preds_final = predict(rf_final,newdata=test)
file=data.frame(rf_preds_final)
colnames(file)='high_booking_rate'
write.csv(file,'Submission_Group6.csv')




