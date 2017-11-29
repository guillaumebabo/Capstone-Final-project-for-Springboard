#load my packages
library(devtools)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(unbalanced)
library(tibble)
library(mlr)
library(ROCR)
library(rpart)
library(rpart.plot)
set.seed(42)
#Load data file and determine the number of rows in the training and test data
Kaggle_Training_Dataset_v2 <- read_csv("~/Documents/predict-bo-trial/Kaggle_Training_Dataset_v2.csv")
Kaggle_Test_Dataset_v2 <- read_csv("~/Documents/predict-bo-trial/Kaggle_Test_Dataset_v2.csv")
Training <- Kaggle_Training_Dataset_v2    
Test_data <-Kaggle_Test_Dataset_v2
nrow(Kaggle_Training_Dataset_v2)
nrow(Kaggle_Test_Dataset_v2)
#Combine training an test data
Data <- rbind(Training,Test_data)
#seeking NA values (Let's see if there are missing values within the dataset)
any(is.na(Training))

#Let's replace the missing values by -99
Data[is.na(Data)] <- -99

#Creating dummy variables : replacing 'No/Yes' to 0/1
Data$potential_issue<-ifelse(Data$potential_issue=='Yes', 1,0)
Data$went_on_backorder<-ifelse(Data$went_on_backorder=='Yes', 1,0)
Data$deck_risk<-ifelse(Data$deck_risk=='Yes', 1,0)
Data$oe_constraint<-ifelse(Data$oe_constraint=='Yes', 1,0)
Data$ppap_risk<-ifelse(Data$ppap_risk=='Yes', 1,0)
Data$stop_auto_buy<-ifelse(Data$stop_auto_buy=='Yes', 1,0)
Data$rev_stop<-ifelse(Data$rev_stop=='Yes', 1,0)

#view cleaned data
str(Data)

#Count : how many parts went on backorders
table(Data['went_on_backorder'])

#Create a Bar Chart
    # Create data for the graph.
     x <-  c(1676568,11293)
     barplot(x,width = 1, main= "Bar Chart", ylab= "Went on Backorder",names.arg= c("No","Yes"),col="blue")
# Create Box plot
    boxplot(Data$lead_time,Data$went_on_backorder, ylim= c(0,60), ylab= "Lead Time", main= "Boxplot")
    boxplot(Data$forecast_3_month,Data$went_on_backorder, ylab= "forecast_3_month", main= "Boxplot")
    boxplot(Data$forecast_6_month,Data$went_on_backorder, ylab= "forecast_6_month", main= "Boxplot")
    boxplot(Data$forecast_9_month,Data$went_on_backorder, ylab= "forecast_9_month", main= "Boxplot")
    
# split the data to training and test
    training_split <- Data[c(1:1687861),c(1:23)]
    test_split <- Data[c(1687862:1929937), c(1:23)]
    View(training_split)
# Use SMOTE sampling to balance the dataset
    training_split$went_on_backorder <- as.factor(training_split$went_on_backorder)
    input  <- training_split %>% select(-went_on_backorder)
    output <- training_split$went_on_backorder 
    training_balanced <- ubSMOTE (input,output,  perc.over = 200, perc.under = 200, k = 5)
# Recombine the synthetic balanced data
    training_df <- bind_cols(as.tibble(training_balanced$X), tibble(went_on_backorder = training_balanced$Y))
    
# count new dataset after applying SMOTE
    View(training_df)
    table(training_df['went_on_backorder'])
    Y <- c(45172,33879 )
    percent<- round(100*Y/sum(Y), 1)
    percent
# Barplot to vizualize dataset after applying SMOTE
   barplot(Y,width = 1, main= "Bar Chart", ylab= "Went on Backorder",names.arg= c("No","Yes"),col="red")
    
# LOGISTIC REGRESSION MODEL
   logistic_regression= glm(went_on_backorder~lead_time+national_inv+forecast_6_month+forecast_3_month+forecast_9_month+
                              sales_1_month+sales_3_month+sales_6_month+sales_9_month+min_bank +
                              local_bo_qty+perf_12_month_avg+potential_issue+deck_risk+oe_constraint
                            +stop_auto_buy, data=training_df, family= binomial)
   summary(logistic_regression)
# predict the outcoume on the test data   
   predict_test <- predict(logistic_regression, test_split, type= "response")
   summary(predict_test)
   tapply(predict_test,test_split$went_on_backorder, mean)

#ROC curve to find accurate threshold
ROCR_pred= prediction(predict_test,test_split$went_on_backorder)
ROCR_perf= performance(ROCR_pred, "tpr","fpr")
plot(ROCR_perf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1,7))

#Confusion matrix
   table(test_split$went_on_backorder,predict_test>0.47)
  
# AUC
  as.numeric(performance(ROCR_pred, "auc")@y.values)
   
# CART : REGRESSION TREE MODEL
  
   tree_model= rpart(went_on_backorder~lead_time+national_inv+forecast_6_month+forecast_3_month+forecast_9_month+
                       sales_1_month+sales_3_month+sales_6_month+sales_9_month+min_bank +
                       local_bo_qty+perf_12_month_avg+potential_issue+deck_risk+oe_constraint
                     +stop_auto_buy, data= training_df, method = "class", control = rpart.control(minbucket = 25))
   prp(tree_model)
  # prediction
   predictCART = predict(tree_model,newdata=test_split, type= "class" )
   table(test_split$went_on_backorder,predictCART )
  
   #ROC curve (CART model)
   predict_ROC=predict(tree_model, newdata= test_split)
   ROCRCart_pred=prediction(predict_ROC[,2],test_split$went_on_backorder)
   ROCRCart_perf= performance(ROCRCart_pred, "tpr","fpr")
   plot(ROCRCart_perf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1,7))
   
   #AUC (CART model)
   as.numeric(performance(ROCRCart_pred, "auc")@y.values)
  
# RANDOM FOREST MODEL
   install.packages("randomForest")
   library(randomForest)
   training_df$went_on_backorder= as.factor(training_df$went_on_backorder)
   test_split$went_on_backorder= as.factor(test_split$went_on_backorder)
   randomforest_model= randomForest(went_on_backorder~lead_time+national_inv+forecast_6_month+forecast_3_month+forecast_9_month+
                       sales_1_month+sales_3_month+sales_6_month+sales_9_month+min_bank +
                       local_bo_qty+perf_12_month_avg+potential_issue+deck_risk+oe_constraint
                     +stop_auto_buy, data= training_df, nodesize= 25, ntree= 200)
   predict_forest= predict(randomforest_model,newdata = test_split)
   table(test_split$went_on_backorder,predict_forest)
   #Cross Validation
  install.packages("caret")
  library(caret)
  install.packages("e1071")
  library(e1071)
  fitControl= trainControl(method="cv", number= 10)
  cartGrid= expand.grid(.cp=(1:50)*0.01)
  train(went_on_backorder~lead_time+national_inv+forecast_6_month+forecast_3_month+forecast_9_month+
          sales_1_month+sales_3_month+sales_6_month+sales_9_month+min_bank +
          local_bo_qty+perf_12_month_avg+potential_issue+deck_risk+oe_constraint
        +stop_auto_buy,data= training_df, method= "rpart", trControl=fitControl, tuneGrid=cartGrid)
  #Cross Validation included
  randomforestCV_model= rpart(went_on_backorder~lead_time+national_inv+forecast_6_month+forecast_3_month+forecast_9_month+
                                sales_1_month+sales_3_month+sales_6_month+sales_9_month+min_bank +
                                local_bo_qty+perf_12_month_avg+potential_issue+deck_risk+oe_constraint
                              +stop_auto_buy,data= training_df, method= "class", control=rpart.control(cp = 0.01)) 
  ##Prediction
  predictCV= predict(randomforestCV_model,newdata=test_split, type= "class")
  table(test_split$went_on_backorder,predictCV)
  
  #ROC curve (Random Forest model)
  predictRF_ROC=predict(tree_model, newdata= test_split)
  ROCRRF_pred=prediction(predictRF_ROC[,2],test_split$went_on_backorder)
  ROCRRF_perf= performance(ROCRRF_pred, "tpr","fpr")
  plot(ROCRRF_perf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1,7))
  
  #AUC (Random Forest model)
  as.numeric(performance(ROCRCart_pred, "auc")@y.values)
  
  #Important variable plot
  varImpPlot(randomforest_model)
  