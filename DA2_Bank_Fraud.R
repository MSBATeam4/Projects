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

Fraud_Data <- mutate(Fraud_Data, TS_Month = month(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data, TS_Day = day(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data, TS_Year = year(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')))

#Missing Value Handling
Fraud_Data$TRAN_ID <- ifelse(is.na(Fraud_Data$TRAN_ID), na.omit(Fraud_Data$TRAN_ID), Fraud_Data$TRAN_ID)
Fraud_Data$TRAN_AMT <- ifelse(is.na(Fraud_Data$TRAN_AMT), na.omit(Fraud_Data$TRAN_AMT), Fraud_Data$TRAN_AMT)
Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL <- ifelse(is.na(Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL), na.omit(Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL), Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL)
Fraud_Data$CUST_AGE <- ifelse(is.na(Fraud_Data$CUST_AGE), round(mean(Fraud_Data$CUST_AGE), 0), Fraud_Data$CUST_AGE)
Fraud_Data$OPEN_ACCT_CT <- ifelse(is.na(Fraud_Data$OPEN_ACCT_CT), na.omit(Fraud_Data$OPEN_ACCT_CT), Fraud_Data$OPEN_ACCT_CT)
Fraud_Data$WF_dvc_age <- ifelse(is.na(Fraud_Data$WF_dvc_age), round(mean(Fraud_Data$WF_dvc_age), 0), Fraud_Data$WF_dvc_age)
Fraud_Data$PWD_UPDT_TS <- ifelse(is.na(Fraud_Data$PWD_UPDT_TS), Fraud_Data$CUST_SINCE_DT, Fraud_Data$PWD_UPDT_TS)
Fraud_Data$CARR_NAME <- ifelse(is.na(Fraud_Data$CARR_NAME), "other/unlisted", Fraud_Data$CARR_NAME)
Fraud_Data$RGN_NAME <- ifelse(is.na(Fraud_Data$RGN_NAME), "unknown", Fraud_Data$RGN_NAME)
Fraud_Data$STATE_PRVNC_TXT <- ifelse(is.na(Fraud_Data$STATE_PRVNC_TXT), "unknown", Fraud_Data$STATE_PRVNC_TXT)
Fraud_Data$ALERT_TRGR_CD <- ifelse(is.na(Fraud_Data$ALERT_TRGR_CD), "NO_DEVICE_ON_RECORD", Fraud_Data$ALERT_TRGR_CD)
Fraud_Data$DVC_TYPE_TXT <- ifelse(is.na(Fraud_Data$DVC_TYPE_TXT), "NO_DEVICE_ON_RECORD", Fraud_Data$DVC_TYPE_TXT)
Fraud_Data$AUTHC_PRIM_TYPE_CD <- ifelse(is.na(Fraud_Data$AUTHC_PRIM_TYPE_CD), "NO_AUTH_REQUIRED", Fraud_Data$AUTHC_PRIM_TYPE_CD)
Fraud_Data$AUTHC_SCNDRY_STAT_TXT <- ifelse(is.na(Fraud_Data$AUTHC_SCNDRY_STAT_TXT), "AUTH_BYPASSED", Fraud_Data$AUTHC_SCNDRY_STAT_TXT)
Fraud_Data$CUST_ZIP <- ifelse(is.na(Fraud_Data$CUST_ZIP), na.omit(Fraud_Data$CUST_ZIP), Fraud_Data$CUST_ZIP)
Fraud_Data$CUST_STATE <- ifelse(is.na(Fraud_Data$CUST_STATE), na.omit(Fraud_Data$CUST_STATE), Fraud_Data$CUST_STATE)
Fraud_Data$PH_NUM_UPDT_TS <- ifelse(is.na(Fraud_Data$PH_NUM_UPDT_TS), Fraud_Data$CUST_SINCE_DT, Fraud_Data$PH_NUM_UPDT_TS)
Fraud_Data$CUST_SINCE_DT <- ifelse(is.na(Fraud_Data$CUST_SINCE_DT), na.omit(Fraud_Data$CUST_SINCE_DT), Fraud_Data$CUST_SINCE_DT)
Fraud_Data$TRAN_TS <- ifelse(is.na(Fraud_Data$TRAN_TS), na.omit(Fraud_Data$TRAN_TS), Fraud_Data$TRAN_TS)
Fraud_Data$TRAN_DT <- ifelse(is.na(Fraud_Data$TRAN_DT), na.omit(Fraud_Data$TRAN_DT), Fraud_Data$TRAN_DT)
Fraud_Data$ACTN_CD <- ifelse(is.na(Fraud_Data$ACTN_CD), "SCHPMT", Fraud_Data$ACTN_CD)
Fraud_Data$ACTN_INTNL_TXT <- ifelse(is.na(Fraud_Data$ACTN_INTNL_TXT), "P2P_COMMIT", Fraud_Data$ACTN_INTNL_TXT)
Fraud_Data$TRAN_TYPE_CD <- ifelse(is.na(Fraud_Data$TRAN_TYPE_CD), "P2P", Fraud_Data$TRAN_TYPE_CD)
Fraud_Data$ACTVY_DT <- ifelse(is.na(Fraud_Data$ACTVY_DT), na.omit(Fraud_Data$ACTVY_DT), Fraud_Data$ACTVY_DT)


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
