#IN DEV PHASE
#Remember to keep up with updated main data set name.
library(caret)
fraud_data <- read.csv(file = 'New_Alliance_data_.csv', header = T)

library(skimr)
summaryStats <- skim(fraud_data)
summaryStats

#Clean dates here
library(lubridate)
date_format <- as.Date(fraud_data$TRAN_TS)

fraud_month <- mutate(fraud_data, ts_month = month(as.Date(fraud_data$TRAN_TS, '%m/%d/%Y')))
fraud_day <- mutate(fraud_month, ts_day = day(as.Date(fraud_data$TRAN_TS, '%m/%d/%Y')))
fraud_year <- 
#Add New Vars here
#Alliance_Fraud_Data$FRAUD_NONFRAUD <- as.factor(Alliance_Fraud_Data$FRAUD_NONFRAUD)

#Some Features?
fraud_month$Can_Afford <- c()
fraud_month$Can_Afford <- ifelse(fraud_month$ACCT_PRE_TRAN_AVAIL_BAL >= fraud_month$TRAN_AMT,1,0)


#FIX dataset name (Alliance fraud to fraud_month or whatever)
Alliance_Fraud_Data$AgeBin <- c()
Alliance_Fraud_Data$AgeBin[Alliance_Fraud_Data$CUST_AGE <= 25] <- "25 and Younger"
Alliance_Fraud_Data$AgeBin[Alliance_Fraud_Data$CUST_AGE > 25 & Alliance_Fraud_Data$CUST_AGE <= 49] <- "26 to 49"
Alliance_Fraud_Data$AgeBin[Alliance_Fraud_Data$CUST_AGE > 49 & Alliance_Fraud_Data$CUST_AGE <= 70] <- "50 to 70"
Alliance_Fraud_Data$AgeBin[Alliance_Fraud_Data$CUST_AGE > 70] <- "Over 70"



Alliance_Fraud_Data$NonAccountTrans <- c()
Alliance_Fraud_Data$NonAccountTrans <- ifelse(Alliance_Fraud_Data$OPEN_ACCT_CT == 0, "Yes","No")




Alliance_Fraud_Data$NumbAccountBin <- c()
Alliance_Fraud_Data$NumbAccountBin[Alliance_Fraud_Data$OPEN_ACCT_CT == 0] <- "None"
Alliance_Fraud_Data$NumbAccountBin[Alliance_Fraud_Data$OPEN_ACCT_CT > 0 & Alliance_Fraud_Data$OPEN_ACCT_CT <= 3] <- "1 to 3"
Alliance_Fraud_Data$NumbAccountBin[Alliance_Fraud_Data$OPEN_ACCT_CT > 3 & Alliance_Fraud_Data$OPEN_ACCT_CT <= 7] <- "4 to 7"
Alliance_Fraud_Data$NumbAccountBin[Alliance_Fraud_Data$OPEN_ACCT_CT > 7 & Alliance_Fraud_Data$OPEN_ACCT_CT <= 10] <- "8 to 10"
Alliance_Fraud_Data$NumbAccountBin[Alliance_Fraud_Data$OPEN_ACCT_CT > 10] <- "Over 10"

Alliance_Fraud_Data$Foreign <- ifelse(Alliance_Fraud_Data$STATE_PRVNC_TXT %in% c("alabama","texas"),0,1)
