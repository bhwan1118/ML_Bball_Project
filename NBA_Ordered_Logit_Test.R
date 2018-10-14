#Set directory
setwd("C:/Users/Benjamin/Desktop/NBA Data Mining")

#load packages

library(ordinal)
library(MASS)
library(caret)

#Read in file
NBA_data <- read.csv('NBA_Ordered_Logit.csv',header=T)

#Teams currently in playoffs
Current_NBA <- NBA_data[1:16,]
Current_NBA <-Current_NBA[,!colnames(Current_NBA) 
                  %in% c('Season', "Playoff", "G","FG","FGA", "MP", "Team",
                         'Three_P',"Three_PA","Two_P","Two_PA","FT","FTA","ORB",
                         "DRB","TRB","AST","STL","BLK","TOV","PF","PTS","Attendance","Wins","PW","PL","MOV","Three_PA_G")]

NBA_data_complete <- NBA_data[complete.cases(NBA_data),]

#Trim excess columns, keep team in?
NBA_data_complete <- NBA_data_complete[,!colnames(NBA_data_complete) 
                                       %in% c('Season', "Playoff", "G","FG","FGA", "MP", "Team",
                                              'Three_P',"Three_PA","Two_P","Two_PA","FT","FTA","ORB",
                                              "DRB","TRB","AST","STL","BLK","TOV","PF","PTS","Attendance","Wins","PW","PL","MOV","Three_PA_G")]
#set finish as factor
NBA_data_complete$Finish <- as.factor(NBA_data_complete$Finish)
nba_data2 = NBA_data_complete[,-1:-2]

eigen(t(as.matrix(nba_data2))%*%as.matrix(nba_data2))

#Training/testing data
train_ind <- sample(1:176,0.7*176,rep=F)

NBA_train <- NBA_data_complete[train_ind,]
NBA_train_x <- NBA_train
NBA_train_y <- NBA_train_x$Finish

NBA_test <- NBA_data_complete[-train_ind,]
NBA_test_x <- NBA_test
NBA_test_y <- NBA_test_x$Finish

#Testing Models
model <- polr(NBA_data_complete$Finish ~.,data=NBA_data_complete[,c(1,3:20)])
#most number of columns
model <- polr(NBA_train$Finish ~.,data=NBA_train[,c(1:21,24:25)])

#Without advanced statistics
model_basic <- polr(NBA_train$Finish~.,data=NBA_train[,c(1:21)])

#only with advanced stats
model_advanced <- polr(NBA_train$Finish ~ ., data=NBA_train[,c(21:38)])


model <- polr(NBA_train$Finish ~.,data=NBA_train)


#Predictions
train_predictions<-predict(model,NBA_train)
test_predictions<-predict(model,NBA_test)

train_basic_predictions<-predict(model_basic,NBA_train)
test_basic_predictions<-predict(model_basic,NBA_test)


train_advanced_predictions<-predict(model_advanced,NBA_train)
test_advanced_predictions<-predict(model_advanced,NBA_test)
# Confusion Matrices
confusionMatrix(train_predictions,NBA_train$Finish)
confusionMatrix(test_predictions,NBA_test$Finish)

confusionMatrix(train_basic_predictions,NBA_train$Finish)
confusionMatrix(test_basic_predictions,NBA_test$Finish)

confusionMatrix(train_advanced_predictions,NBA_train$Finish)
confusionMatrix(test_advanced_predictions,NBA_test$Finish)

#Testing average accuracies by looping through different training set
mix_acc_train_1 <-c()
mix_acc_train_2 <-c()
mix_acc_train_3 <-c()
mix_acc_train_4 <-c()
mix_acc_train_5 <-c()

mix_acc_test_1 <- c()
mix_acc_test_2 <- c()
mix_acc_test_3 <- c()
mix_acc_test_4 <- c()
mix_acc_test_5 <- c()

basic_acc_train_1 <- c()
basic_acc_train_2 <- c()
basic_acc_train_3 <- c()
basic_acc_train_4 <- c()
basic_acc_train_5 <- c()

basic_acc_test_1 <- c()
basic_acc_test_2 <- c()
basic_acc_test_3 <- c()
basic_acc_test_4 <- c()
basic_acc_test_5 <- c()

adv_acc_train_1 <- c()
adv_acc_train_2 <- c()
adv_acc_train_3 <- c()
adv_acc_train_4 <- c()
adv_acc_train_5 <- c()

adv_acc_test_1 <- c()
adv_acc_test_2 <- c()
adv_acc_test_3 <- c()
adv_acc_test_4 <- c()
adv_acc_test_5 <- c()

for(i in 1:10){
  #resample training and test
  train_ind <- sample(1:176,0.7*176,rep=F)
  NBA_train <- NBA_data_complete[train_ind,]
  NBA_test <- NBA_data_complete[-train_ind,]
  
  #Different models
  
  #Basic stats
  model_basic <- polr(NBA_train$Finish~.,data=NBA_train[,c(1:21)])
  
  #Advanced stats
  model_advanced <- polr(NBA_train$Finish ~ ., data=NBA_train[,c(21:38)])
  

  #Predicitons of each model 
  
  train_basic_predictions<-predict(model_basic,NBA_train)
  test_basic_predictions<-predict(model_basic,NBA_test)
  
  train_advanced_predictions<-predict(model_advanced,NBA_train)
  test_advanced_predictions<-predict(model_advanced,NBA_test)
  
  #Store accuracies for each class
  
  #Basic Model
  basic_acc_train_1[i] = confusionMatrix(train_basic_predictions,NBA_train$Finish)$byClass[1,8]
  basic_acc_train_2[i] = confusionMatrix(train_basic_predictions,NBA_train$Finish)$byClass[2,8]
  basic_acc_train_3[i] = confusionMatrix(train_basic_predictions,NBA_train$Finish)$byClass[3,8]
  basic_acc_train_4[i] = confusionMatrix(train_basic_predictions,NBA_train$Finish)$byClass[4,8]
  basic_acc_train_5[i] = confusionMatrix(train_basic_predictions,NBA_train$Finish)$byClass[5,8]
  
  basic_acc_test_1[i] = confusionMatrix(test_basic_predictions,NBA_test$Finish)$byClass[1,8]
  basic_acc_test_2[i] = confusionMatrix(test_basic_predictions,NBA_test$Finish)$byClass[2,8]
  basic_acc_test_3[i] = confusionMatrix(test_basic_predictions,NBA_test$Finish)$byClass[3,8]
  basic_acc_test_4[i] = confusionMatrix(test_basic_predictions,NBA_test$Finish)$byClass[4,8]
  basic_acc_test_5[i] = confusionMatrix(test_basic_predictions,NBA_test$Finish)$byClass[5,8]
  
  #Advanced Model
  adv_acc_train_1[i] = confusionMatrix(train_advanced_predictions,NBA_train$Finish)$byClass[1,8]
  adv_acc_train_2[i] = confusionMatrix(train_advanced_predictions,NBA_train$Finish)$byClass[2,8]
  adv_acc_train_3[i] = confusionMatrix(train_advanced_predictions,NBA_train$Finish)$byClass[3,8]
  adv_acc_train_4[i] = confusionMatrix(train_advanced_predictions,NBA_train$Finish)$byClass[4,8]
  adv_acc_train_5[i] = confusionMatrix(train_advanced_predictions,NBA_train$Finish)$byClass[5,8]
  
  adv_acc_test_1[i] = confusionMatrix(test_advanced_predictions,NBA_test$Finish)$byClass[1,8]
  adv_acc_test_2[i] = confusionMatrix(test_advanced_predictions,NBA_test$Finish)$byClass[2,8]
  adv_acc_test_3[i] = confusionMatrix(test_advanced_predictions,NBA_test$Finish)$byClass[3,8]
  adv_acc_test_4[i] = confusionMatrix(test_advanced_predictions,NBA_test$Finish)$byClass[4,8]
  adv_acc_test_5[i] = confusionMatrix(test_advanced_predictions,NBA_test$Finish)$byClass[5,8]
}

mean(mix_acc_train_1)
mean(mix_acc_test_1)

mean(basic_acc_train_1)
mean(basic_acc_test_1)

mean(adv_acc_train_1)
mean(adv_acc_test_1)

#Cross validation...Unfortunately crashed most of the time making it unusable
K=10
subSize = length(NBA_train[,1])/K
mean_acc_train = c()
mean_acc_val = c()
acc_train_1 = c()
acc_train_2 = c()
acc_train_3 = c()
acc_train_4 = c()
acc_train_5 = c()
acc_val_1 = c()
acc_val_2 = c()
acc_val_3 = c()
acc_val_4 = c()
acc_val_5 = c()

  for(iter in 1:K) {
    
    iter=1
    subsetStart = subSize*(iter-1) + 1
    subsetEnd = subsetStart + subSize - 1
    validationSet = NBA_train_x[subsetStart:subsetEnd,]
    validationTarget = NBA_train_y[subsetStart:subsetEnd]
    
    ktrainingSet = NBA_train_x[-(subsetStart:subsetEnd),]
    ktrainingTarget = NBA_train_y[-(subsetStart:subsetEnd)]
    model <- polr(ktrainingSet$Finish~.,data=ktrainingSet[,1:20])      
    predict_train <- factor(predict(model,ktrainingSet),levels=c(1,2,3,4,5))
    predict_validate <- factor(predict(model,validationSet),levels=c(1,2,3,4,5))          
    
    acc_train_1[iter] = confusionMatrix(predict_train,as.factor(ktrainingTarget))$byClass[1,8]
    acc_train_2[iter] = confusionMatrix(predict_train,as.factor(ktrainingTarget))$byClass[2,8]
    acc_train_3[iter] = confusionMatrix(predict_train,as.factor(ktrainingTarget))$byClass[3,8]
    acc_train_4[iter] = confusionMatrix(predict_train,as.factor(ktrainingTarget))$byClass[4,8]
    acc_train_5[iter] = confusionMatrix(predict_train,as.factor(ktrainingTarget))$byClass[5,8]

    acc_val_1[iter] = confusionMatrix(predict_validate,validationTarget)$byClass[1,8]
    acc_val_2[iter] = confusionMatrix(predict_validate,validationTarget)$byClass[2,8]
    acc_val_3[iter] = confusionMatrix(predict_validate,validationTarget)$byClass[3,8]
    acc_val_4[iter] = confusionMatrix(predict_validate,validationTarget)$byClass[4,8]
    acc_val_5[iter] = confusionMatrix(predict_validate,validationTarget)$byClass[5,8]
  }

#Current Champion Prediction
  Final_Adv_model <- polr(NBA_data_complete$Finish ~. , data= NBA_data_complete[,21:38])
  Playoff_results_prediction <- predict(Final_Adv_model,Current_NBA)

  Final_basic_model <- polr(NBA_data_complete$Finish ~. , data= NBA_data_complete[,1:21])
  Playoff_results_basic <- predict(Final_basic_model,Current_NBA)
#PCA analysis
NBA_data_reorder<-NBA_data_complete[,c(2,3,1,4:length(NBA_data_complete[1,]))]
NBA_names <- NBA_data_complete[,1]
NBA_PCA <- prcomp(NBA_data_reorder[,c(4:length(NBA_data_complete[1,]))],Center=T,Scale=T)
pdf('NBA_PCA.pdf')
biplot(NBA_PCA,scale=0,cex = 0.2)
dev.off()

summary(NBA_PCA)
