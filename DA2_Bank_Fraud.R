#IN DEV PHASE
#Remember to keep up with updated main data set name.
library(tidyverse)
library(caret)
Fraud_Data <- read.csv(file = 'New_Alliance_data_.csv', header = T)

library(skimr)
summaryStats <- skim(Fraud_Data)
summaryStats

#Clean dates here
library(lubridate)
date_format <- as.Date(Fraud_Data$TRAN_TS)

Fraud_Data <- mutate(Fraud_Data, ts_month = month(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data, ts_day = day(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data, ts_year = year(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')))
#fraud_data <- mutate(fraud_data, ts_hour = hour(as.Date(fraud_data$TRAN_TS)))#, '%m/%d/%Y %H:%M')))
#fraud_data <- mutate(fraud_data, ts_minute = minute(as.POSIXct(fraud_data$TRAN_TS),'%H:%M'))

#fraud_data <- mutate(fraud_month, ts_day = day(as.Date(fraud_data$TRAN_TS, '%m/%d/%Y')))
#fraud_data <- mutate(fraud_day, ts_year = year(as.Date(fraud_data$TRAN_TS, '%m/%d/%Y')))
  
#Alliance_Fraud_Data$FRAUD_NONFRAUD <- as.factor(Alliance_Fraud_Data$FRAUD_NONFRAUD)
  
#Some Features?
Fraud_Data$Can_Afford <- c()
Fraud_Data$Can_Afford <- ifelse(Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL >= Fraud_Data$TRAN_AMT,1,0)


#FIX dataset name (Alliance fraud to fraud_month or whatever)
Fraud_Data$AgeBin <- c()
Fraud_Data$AgeBin[Fraud_Data$CUST_AGE <= 25] <- "25 and Younger"
Fraud_Data$AgeBin[Fraud_Data$CUST_AGE > 25 & Fraud_Data$CUST_AGE <= 49] <- "26 to 49"
Fraud_Data$AgeBin[Fraud_Data$CUST_AGE > 49 & Fraud_Data$CUST_AGE <= 70] <- "50 to 70"
Fraud_Data$AgeBin[Fraud_Data$CUST_AGE > 70] <- "Over 70"



Fraud_Data$NonAccountTrans <- c()
Fraud_Data$NonAccountTrans <- ifelse(Fraud_Data$OPEN_ACCT_CT == 0, "Yes","No")




Fraud_Data$NumbAccountBin <- c()
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT == 0] <- "None"
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT > 0 & Fraud_Data$OPEN_ACCT_CT <= 3] <- "1 to 3"
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT > 3 & Fraud_Data$OPEN_ACCT_CT <= 7] <- "4 to 7"
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT > 7 & Fraud_Data$OPEN_ACCT_CT <= 10] <- "8 to 10"
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT > 10] <- "Over 10"

Fraud_Data$Foreign <- ifelse(Fraud_Data$STATE_PRVNC_TXT %in% c("alabama","texas"),0,1)
