library("caret") #confusion matrix
library("LiblineaR")

nba_lib2 = read.csv("NBA_Lib2.csv")
nba_lib2$Playoff = as.factor(nba_lib2$Playoff)
nba_lib2$Playoff = relevel(nba_lib2$Playoff,ref = "1")
nba_x = nba_lib2[,-1] # All of the lib data, minus the Playoff column
nba_y = nba_lib2$Playoff # Just the Playoff column

train_idx = sample(c(1:359),300,rep=F) # Sample 300 of the data points
nba_train = nba_lib2[train_idx,]
nba_train_x = nba_x[train_idx,]
nba_train_y = nba_y[train_idx]

nba_test_x = nba_x[-train_idx,]
nba_test_y = nba_y[-train_idx]

log_cost_list = seq(-5,5,.1)
cost_list = 10^(log_cost_list)

K = 10
subSize = 300/K # Size of the cross-validation subsets
mean_acc_train = c()
mean_acc_val = c()
acc_train = c()
acc_val = c()

# K-fold cross-validation to find the optimal cost for L1-Regularization
for(c in 1:length(cost_list)) {
  co = cost_list[c]
  for(iter in 1:K) {
    subsetStart = subSize*(iter-1) + 1
    subsetEnd = subsetStart + subSize - 1
    validationSet = nba_train_x[subsetStart:subsetEnd,]
    validationTarget = as.numeric(nba_train_y[subsetStart:subsetEnd])-1
    ktrainingSet = nba_train_x[-(subsetStart:subsetEnd),]
    ktrainingTarget = as.numeric(nba_train_y[-(subsetStart:subsetEnd)])-1
    model=LiblineaR(data=ktrainingSet,target=ktrainingTarget,type=6,cost=co,cross=0)
    pred_train = factor(predict(model,ktrainingSet,type="class")$predictions,levels = c(0,1))
    pred_val = factor(predict(model,validationSet,type="class")$predictions,levels = c(0,1))
    
    acc_train[iter] = confusionMatrix(pred_train,ktrainingTarget)$byClass[8]
    acc_val[iter] = confusionMatrix(pred_val,validationTarget)$byClass[8]
  }
  mean_acc_train[c] = mean(acc_train)
  mean_acc_val[c] = mean(acc_val)
}
# Graph the Training and Validation Accuracies for each log-cost
plot(log_cost_list, mean_acc_train,'l',main="10-Fold Cross-Validation with L1-Regularization",xlab = "Log Costs",ylab="Balanced Accuracy",ylim=c(.5,.95))
lines(log_cost_list, mean_acc_val,col="blue")
legend(0,.7,c("Training","Validation"),lty=c(1,1),lwd=c(2.5,2.5),col=c("black","blue"))

#find cost that gives largest validation accuracy
best_cost_ind = which(mean_acc_val==max(mean_acc_val))[1] #To break ties, take the lowest cost
best_cost = cost_list[best_cost_ind]
best_log_cost = log_cost_list[best_cost_ind]

# Compare to remaining test data
# Train a model using the optimal cost
best_model=LiblineaR(data=nba_train_x,target=nba_train_y,type=6,cost=best_cost,cross=0)
# Make predictions on the remaining 59 test observations
pred_best = factor(predict(best_model,nba_test_x,type="response")$predictions,levels=c(1,0))
best_model_acc = confusionMatrix(unlist(pred_best),nba_test_y)$byClass[8]
best_model$W

# Average accuracy of model
best_model_acc = c()
for (j in 1:100) {
  train_idx = sample(c(1:359),300,rep=F) # Sample 300 of the data points
  nba_train = nba_lib2[train_idx,]
  nba_train_x = nba_x[train_idx,]
  nba_train_y = nba_y[train_idx]
  nba_test_x = nba_x[-train_idx,]
  nba_test_y = nba_y[-train_idx]
  
  best_model=LiblineaR(data=nba_train_x,target=nba_train_y,type=6,cost=best_cost,cross=0)
  pred_best = factor(predict(best_model,nba_test_x,type="response")$predictions,levels=c(1,0))
  best_model_acc[j] = confusionMatrix(unlist(pred_best),nba_test_y)$byClass[8]
  
}
average_model_acc = mean(best_model_acc)

#### Decision Tree playoff prediction ###
library(rpart)
library(rpart.plot)

## Cross validation for optimal depth
acc_train_tree = c()
acc_val_tree = c()
mean_acc_train_tree = c()
mean_acc_val_tree = c()
K = 10
subSize = 300/K 
for(d in 1:20) {
  for(iter in 1:K) {
    subsetStart = subSize*(iter-1) + 1
    subsetEnd = subsetStart + subSize - 1
    validationSet = nba_train_x[subsetStart:subsetEnd,]
    validationTarget = as.numeric(nba_train_y[subsetStart:subsetEnd])-1
    ktrainingSet = nba_train[-(subsetStart:subsetEnd),]
    ktrainingTarget = as.numeric(nba_train_y[-(subsetStart:subsetEnd)])-1
    fit = rpart(Playoff ~.,data=ktrainingSet,method="class",maxdepth=d,minsplit=2,cp=0)
    pred_train_tree = as.numeric(predict(fit,ktrainingSet,type="class")) -1
    pred_val_tree = as.numeric(predict(fit,validationSet,type="class"))-1 
    
    acc_train_tree[iter] = confusionMatrix(pred_train_tree,ktrainingTarget)$byClass[8]
    acc_val_tree[iter] = confusionMatrix(pred_val_tree,validationTarget)$byClass[8]
  }
  mean_acc_train_tree[d] = mean(acc_train_tree)
  mean_acc_val_tree[d] = mean(acc_val_tree)
}

plot(seq(1,20,1), mean_acc_train_tree,'l',xlab = "Tree Depth",ylab="Balanced Accuracy",main="Balanced Accuracy of Decision Tree",ylim=c(.65,1))
lines(seq(1,20,1), mean_acc_val_tree,col="blue")
legend(10,.93,c("Training","Validation"),lty=c(1,1),lwd=c(2.5,2.5),col=c("black","blue"))

#Find tree depth that gives highest validation accuracy
best_tree_depth = which(mean_acc_val_tree==max(mean_acc_val_tree))[1]
#test on test data
tree_best = rpart(Playoff ~.,data=nba_train,method="class",maxdepth=best_tree_depth,minsplit=2,cp=0)
pred_test = as.numeric(predict(tree_best,nba_test_x,type="class"))-1
tree_best_acc = confusionMatrix(pred_test,as.numeric(nba_test_y)-1)$byClass[8]
simple_model = rpart(Playoff~.,data=nba_train,method="class",maxdepth=1,minsplit=2,cp=0)
rpart.plot(simple_model)

pdf("Best_Tree_Lib2_Data.pdf")
rpart.plot(tree_best)
dev.off()
dev.new()

##Find average balanced accuracies of models
simple_tree_acc = c()
best_tree_model_acc = c()
for (j in 1:100) {
  train_idx = sample(c(1:359),300,rep=F) # Sample 300 of the data points
  nba_train = nba_lib2[train_idx,]
  nba_train_x = nba_x[train_idx,]
  nba_train_y = nba_y[train_idx]
  nba_test_x = nba_x[-train_idx,]
  nba_test_y = nba_y[-train_idx]
  
  best_tree_model=rpart(Playoff ~.,data=nba_train,method="class",maxdepth=best_tree_depth,minsplit=2,cp=0)
  simple_model = rpart(Playoff~.,data=nba_train,method="class",maxdepth=1,minsplit=2,cp=0)
  pred_tree_best = as.numeric(predict(best_tree_model,nba_test_x,type="class"))-1
  pred_simple = as.numeric(predict(simple_model,nba_test_x,type="class"))-1
  
  best_tree_model_acc[j] = confusionMatrix(unlist(pred_tree_best),as.numeric(nba_test_y)-1)$byClass[8]
  simple_tree_acc[j] = confusionMatrix(unlist(pred_simple),as.numeric(nba_test_y)-1)$byClass[8]
}
average_tree_model_acc = mean(best_tree_model_acc)
average_simple_acc = mean(simple_tree_acc)

#Forest
library("randomForest")

best_forest_model_acc = c()
for (j in 1:100) {
  train_idx = sample(c(1:359),300,rep=F) 
  nba_train = nba_lib2[train_idx,]
  nba_train_x = nba_x[train_idx,]
  nba_train_y = nba_y[train_idx]
  nba_test_x = nba_x[-train_idx,]
  nba_test_y = nba_y[-train_idx]
  fit_forest = randomForest(nba_train_x,nba_train_y,ntree=500)
  pred_forest = predict(fit_forest,nba_test_x,type="class")
  best_forest_model_acc[j] = confusionMatrix(as.numeric(pred_forest)-1,as.numeric(nba_test_y)-1)$byClass[8]
}
average_forest_model_acc = mean(best_forest_model_acc) # close to 100%

# Balanced accuracy using only two criteria (threshold =.5) and glm
nba_train$Playoff = relevel(nba_train$Playoff,ref="1")
model_small = glm(Playoff~eFGperOp+TOVperOp,family="binomial",data=nba_train)
pred_small = predict(model_small,nba_test_x,type="response")
pred_small_bin = pred_small >.5
pred_small_bin = as.numeric(pred_small_bin)
confusionMatrix(unlist(pred_small_bin),as.numeric(nba_test_y)-1)
summary(model_small)

small_model_acc = c()
for (j in 1:100) {
  train_idx = sample(c(1:359),300,rep=F) # Sample 300 of the data points
  nba_train = nba_lib2[train_idx,]
  nba_train_x = nba_x[train_idx,]
  nba_train_y = nba_y[train_idx]
  nba_test_x = nba_x[-train_idx,]
  nba_test_y = nba_y[-train_idx]
  
  small_model=glm(Playoff~eFGperOp+TOVperOp,family="binomial",data=nba_train)
  pred_small = predict(model_small,nba_test_x,type="response")
  pred_small_bin = pred_small >.5
  pred_small_bin = as.numeric(pred_small_bin)
  small_model_acc[j]=confusionMatrix(unlist(pred_small_bin),as.numeric(nba_test_y)-1)$byClass[8]
  
}
average_small_model_acc = mean(small_model_acc)





