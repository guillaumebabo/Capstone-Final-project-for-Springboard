#load my packages
library(devtools)
library(dplyr)
library(tidyr)
library(readr)
#Load data file
Training <- Kaggle_Training_Dataset_v2
#seeking NA values
any(is.na(Training))
#there are some NA values so we need to replace them
Training[is.na(Training)] <- 0
#Creating dummy variables : replacing 'No/Yes' to 0/1
Training$potential_issue<-ifelse(Training$potential_issue=='Yes', 1,0)
Training$went_on_backorder<-ifelse(Training$went_on_backorder=='Yes', 1,0)
Training$deck_risk<-ifelse(Training$deck_risk=='Yes', 1,0)
Training$oe_constraint<-ifelse(Training$oe_constraint=='Yes', 1,0)
Training$ppap_risk<-ifelse(Training$ppap_risk=='Yes', 1,0)
Training$stop_auto_buy<-ifelse(Training$stop_auto_buy=='Yes', 1,0)
Training$rev_stop<-ifelse(Training$rev_stop=='Yes', 1,0)




