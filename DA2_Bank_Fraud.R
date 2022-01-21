#IN DEV PHASE

library(tidyverse)
library(caret)
Fraud_Data <- read.csv(file = 'New_Alliance_data_.csv', header = T)

library(skimr)
summaryStats <- skim(Fraud_Data)
summaryStats

#Create Month, Day, Year variables from transaction timestamp
library(lubridate)
date_format <- as.Date(Fraud_Data$TRAN_TS)

Fraud_Data <- mutate(Fraud_Data, ts_month = month(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data, ts_day = day(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data, ts_year = year(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')))

#Transaction amount greater than available cash? 
Fraud_Data$Can_Afford <- c()
Fraud_Data$Can_Afford <- ifelse(Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL >= Fraud_Data$TRAN_AMT,1,0)


#Group by Age
Fraud_Data$AgeBin <- c()
Fraud_Data$AgeBin[Fraud_Data$CUST_AGE <= 25] <- "25 and Younger"
Fraud_Data$AgeBin[Fraud_Data$CUST_AGE > 25 & Fraud_Data$CUST_AGE <= 49] <- "26 to 49"
Fraud_Data$AgeBin[Fraud_Data$CUST_AGE > 49 & Fraud_Data$CUST_AGE <= 70] <- "50 to 70"
Fraud_Data$AgeBin[Fraud_Data$CUST_AGE > 70] <- "Over 70"


#Does transaction take place for customer who owns no accounts? 
Fraud_Data$NonAccountTrans <- c()
Fraud_Data$NonAccountTrans <- ifelse(Fraud_Data$OPEN_ACCT_CT == 0, "Yes","No")



#Number of accounts in customer household. 
Fraud_Data$NumbAccountBin <- c()
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT == 0] <- "None"
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT > 0 & Fraud_Data$OPEN_ACCT_CT <= 3] <- "1 to 3"
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT > 3 & Fraud_Data$OPEN_ACCT_CT <= 7] <- "4 to 7"
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT > 7 & Fraud_Data$OPEN_ACCT_CT <= 10] <- "8 to 10"
Fraud_Data$NumbAccountBin[Fraud_Data$OPEN_ACCT_CT > 10] <- "Over 10"

#Build vector of US states to determine, is transaction in US or not.
Fraud_Data$Foreign <- ifelse(Fraud_Data$STATE_PRVNC_TXT %in% c("alabama","alaska", "arizon", "arkansas",
                                                               "california", "colorado", "connecticut",
                                                               "delaware", "florida", "georgia", "hawaii",
                                                               "idaho", "illinois", "indiana", "iowa", "kansas",
                                                               "kentucky", "louisiana", "maine", "maryland", "massachusetts",
                                                               "michigan", "minnesota", "mississippi", "missouri",
                                                               "montana", "nebraska", "nevada", "new hampshire", "new jersey",
                                                               "new mexico", "new york", "north carolina", "north dakota",
                                                               "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island",
                                                               "south carolina", "south dakota", "tennessee", "texas",
                                                               "utah", "vermont", "virginia", "washington", "west virginia",
                                                               "wisconsin", "wyoming"),0,1)
#Dummy Variables


#Convert FRAUD_NOFRAUD to a factor


#set.seed()
#Split data for partitioning and indexing 



#set. seed()
#Train model


#model
#plot(model)
#plot(varImp)


#plot tree with rpart.plot


#get predicted probabilities


#get auc
