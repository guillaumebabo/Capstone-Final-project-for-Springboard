#load my packages
library(devtools)
library(dplyr)
library(tidyr)
library(readr)
#Load data file
Test_data <- Kaggle_Test_Dataset_v2
#seeking NA values
any(is.na(Test_data))
#there are some NA values so we need to replace them
Test_data[is.na(Test_data)] <- 0
#Creating dummy variables : replacing 'No/Yes' to 0/1
Test_data$potential_issue<-ifelse(Test_data$potential_issue=='Yes', 1,0)
Test_data$went_on_backorder<-ifelse(Test_data$went_on_backorder=='Yes', 1,0)
Test_data$deck_risk<-ifelse(Test_data$deck_risk=='Yes', 1,0)
Test_data$oe_constraint<-ifelse(Test_data$oe_constraint=='Yes', 1,0)
Test_data$ppap_risk<-ifelse(Test_data$ppap_risk=='Yes', 1,0)
Test_data$stop_auto_buy<-ifelse(Test_data$stop_auto_buy=='Yes', 1,0)
Test_data$rev_stop<-ifelse(Test_data$rev_stop=='Yes', 1,0)
#view cleaned data
View(Test_data)





