library(tidyverse)
library(caret)

Fraud_Data <- (read.csv(file = 'New_Alliance_data_.csv', header = T))

library(skimr)
summaryStats <- skim(Fraud_Data)
summaryStats

#Create Month, Day, Year variables from transaction timestamp
library(lubridate)
date_format <- as.Date(Fraud_Data$TRAN_TS)

Fraud_Data$PWD_UPDT_TS <- ifelse(is.na(Fraud_Data$PWD_UPDT_TS), Fraud_Data$CUST_SINCE_DT, Fraud_Data$PWD_UPDT_TS)
Fraud_Data$PH_NUM_UPDT_TS <- ifelse(is.na(Fraud_Data$PH_NUM_UPDT_TS), Fraud_Data$CUST_SINCE_DT, Fraud_Data$PH_NUM_UPDT_TS)
Fraud_Data$CUST_SINCE_DT <- ifelse(is.na(Fraud_Data$CUST_SINCE_DT), na.omit(Fraud_Data$CUST_SINCE_DT), Fraud_Data$CUST_SINCE_DT)
Fraud_Data$TRAN_TS <- ifelse(is.na(Fraud_Data$TRAN_TS), na.omit(Fraud_Data$TRAN_TS), Fraud_Data$TRAN_TS)
Fraud_Data$TRAN_DT <- ifelse(is.na(Fraud_Data$TRAN_DT), na.omit(Fraud_Data$TRAN_DT), Fraud_Data$TRAN_DT)
Fraud_Data$ACTVY_DT <- ifelse(is.na(Fraud_Data$ACTVY_DT), na.omit(Fraud_Data$ACTVY_DT), Fraud_Data$ACTVY_DT)

#Date and timestamp handling
Fraud_Data <- mutate(Fraud_Data, TS_Month = month(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data, TS_Day = day(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data, TS_Year = year(as.Date(Fraud_Data$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data, PWD_Month = month(as.Date(Fraud_Data$PWD_UPDT_TS,'%m/%d/%Y')),
                     Fraud_Data, PWD_Day = day(as.Date(Fraud_Data$PWD_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data, PWD_Year = year(as.Date(Fraud_Data$PWD_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data, PH_Month = month(as.Date(Fraud_Data$PH_NUM_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data, PH_Day = day(as.Date(Fraud_Data$PH_NUM_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data, PH_Year = year(as.Date(Fraud_Data$PH_NUM_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data, CUST_DT_Month = month(as.Date(Fraud_Data$CUST_SINCE_DT, '%m/%d/%Y')),
                     Fraud_Data, CUST_DT_Day = day(as.Date(Fraud_Data$CUST_SINCE_DT, '%m/%d/%Y')),
                     Fraud_Data, CUST_DT_Year = year(as.Date(Fraud_Data$CUST_SINCE_DT, '%m/%d/%Y')),
                     Fraud_Data, TR_DT_Month = month(as.Date(Fraud_Data$TRAN_DT, '%m/%d/%Y')),
                     Fraud_Data, TR_DT_Day = day(as.Date(Fraud_Data$TRAN_DT, '%m/%d/%Y')),
                     Fraud_Data, TR_DT_Year = year(as.Date(Fraud_Data$TRAN_DT, '%m/%d/%Y')),
                     Fraud_Data, ACTVY_Month = month(as.Date(Fraud_Data$ACTVY_DT, '%m/%d/%Y')),
                     Fraud_Data, ACTVY_Day = day(as.Date(Fraud_Data$ACTVY_DT, '%m/%d/%Y')),
                     Fraud_Data, ACTVY_Year = year(as.Date(Fraud_Data$ACTVY_DT, '%m/%d/%Y')))

Fraud_Data$PWD_UPDT_TS <- NULL
Fraud_Data$PH_NUM_UPDT_TS <- NULL
Fraud_Data$CUST_SINCE_DT <- NULL
Fraud_Data$TRAN_TS <- NULL
Fraud_Data$TRAN_DT <- NULL
Fraud_Data$ACTVY_DT <- NULL

#Missing Value Handling
Fraud_Data$TRAN_ID <- ifelse(is.na(Fraud_Data$TRAN_ID), na.omit(Fraud_Data$TRAN_ID), Fraud_Data$TRAN_ID)
Fraud_Data$TRAN_AMT <- ifelse(is.na(Fraud_Data$TRAN_AMT), na.omit(Fraud_Data$TRAN_AMT), Fraud_Data$TRAN_AMT)
Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL <- ifelse(is.na(Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL), na.omit(Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL), Fraud_Data$ACCT_PRE_TRAN_AVAIL_BAL)
Fraud_Data$CUST_AGE <- ifelse(is.na(Fraud_Data$CUST_AGE), round(mean(Fraud_Data$CUST_AGE), 0), Fraud_Data$CUST_AGE)
Fraud_Data$OPEN_ACCT_CT <- ifelse(is.na(Fraud_Data$OPEN_ACCT_CT), na.omit(Fraud_Data$OPEN_ACCT_CT), Fraud_Data$OPEN_ACCT_CT)
Fraud_Data$WF_dvc_age <- ifelse(is.na(Fraud_Data$WF_dvc_age), round(mean(Fraud_Data$WF_dvc_age), 0), Fraud_Data$WF_dvc_age)
Fraud_Data$CARR_NAME <- ifelse(is.na(Fraud_Data$CARR_NAME), "other/unlisted", Fraud_Data$CARR_NAME)
Fraud_Data$RGN_NAME <- ifelse(is.na(Fraud_Data$RGN_NAME), "unknown", Fraud_Data$RGN_NAME)
Fraud_Data$STATE_PRVNC_TXT <- ifelse(is.na(Fraud_Data$STATE_PRVNC_TXT), "unknown", Fraud_Data$STATE_PRVNC_TXT)
Fraud_Data$ALERT_TRGR_CD <- ifelse(is.na(Fraud_Data$ALERT_TRGR_CD), "NO_DEVICE_ON_RECORD", Fraud_Data$ALERT_TRGR_CD)
Fraud_Data$DVC_TYPE_TXT <- ifelse(is.na(Fraud_Data$DVC_TYPE_TXT), "NO_DEVICE_ON_RECORD", Fraud_Data$DVC_TYPE_TXT)
Fraud_Data$AUTHC_PRIM_TYPE_CD <- ifelse(is.na(Fraud_Data$AUTHC_PRIM_TYPE_CD), "NO_AUTH_REQUIRED", Fraud_Data$AUTHC_PRIM_TYPE_CD)
Fraud_Data$AUTHC_SCNDRY_STAT_TXT <- ifelse(is.na(Fraud_Data$AUTHC_SCNDRY_STAT_TXT), "AUTH_BYPASSED", Fraud_Data$AUTHC_SCNDRY_STAT_TXT)
Fraud_Data$CUST_ZIP <- ifelse(is.na(Fraud_Data$CUST_ZIP), na.omit(Fraud_Data$CUST_ZIP), Fraud_Data$CUST_ZIP)
Fraud_Data$CUST_STATE <- ifelse(is.na(Fraud_Data$CUST_STATE), na.omit(Fraud_Data$CUST_STATE), Fraud_Data$CUST_STATE)
Fraud_Data$ACTN_CD <- ifelse(is.na(Fraud_Data$ACTN_CD), "SCHPMT", Fraud_Data$ACTN_CD)
Fraud_Data$ACTN_INTNL_TXT <- ifelse(is.na(Fraud_Data$ACTN_INTNL_TXT), "P2P_COMMIT", Fraud_Data$ACTN_INTNL_TXT)
Fraud_Data$TRAN_TYPE_CD <- ifelse(is.na(Fraud_Data$TRAN_TYPE_CD), "P2P", Fraud_Data$TRAN_TYPE_CD)
Fraud_Data$TS_Month <- ifelse(is.na(Fraud_Data$TS_Month), na.omit(Fraud_Data$TS_Month), Fraud_Data$TS_Month)
Fraud_Data$TS_Day <- ifelse(is.na(Fraud_Data$TS_Day), na.omit(Fraud_Data$TS_Day), Fraud_Data$TS_Day)
Fraud_Data$TS_Year <- ifelse(is.na(Fraud_Data$TS_Year), na.omit(Fraud_Data$TS_Year), Fraud_Data$TS_Year)
Fraud_Data$PWD_Month <- ifelse(is.na(Fraud_Data$PWD_Month), na.omit(Fraud_Data$PWD_Month), Fraud_Data$PWD_Month)
Fraud_Data$PWD_Day <- ifelse(is.na(Fraud_Data$PWD_Day), na.omit(Fraud_Data$PWD_Day), Fraud_Data$PWD_Day)
Fraud_Data$PWD_Year <- ifelse(is.na(Fraud_Data$PWD_Year), na.omit(Fraud_Data$PWD_Year), Fraud_Data$PWD_Year)
Fraud_Data$PH_Month <- ifelse(is.na(Fraud_Data$PH_Month), na.omit(Fraud_Data$PH_Month), Fraud_Data$PH_Month)
Fraud_Data$PH_Day <- ifelse(is.na(Fraud_Data$PH_Day), na.omit(Fraud_Data$PH_Day), Fraud_Data$PH_Day)
Fraud_Data$PH_Year <- ifelse(is.na(Fraud_Data$PH_Year), na.omit(Fraud_Data$PH_Year), Fraud_Data$PH_Year)
Fraud_Data$CUST_DT_Month <- ifelse(is.na(Fraud_Data$CUST_DT_Month), na.omit(Fraud_Data$CUST_DT_Month), Fraud_Data$CUST_DT_Month)
Fraud_Data$CUST_DT_Day <- ifelse(is.na(Fraud_Data$CUST_DT_Day), na.omit(Fraud_Data$CUST_DT_Day), Fraud_Data$CUST_DT_Day)
Fraud_Data$CUST_DT_Year <- ifelse(is.na(Fraud_Data$CUST_DT_Year), na.omit(Fraud_Data$CUST_DT_Year), Fraud_Data$CUST_DT_Year)
Fraud_Data$CUST_DT_Month <- ifelse(is.na(Fraud_Data$CUST_DT_Month), na.omit(Fraud_Data$CUST_DT_Month), Fraud_Data$CUST_DT_Month)
Fraud_Data$CUST_DT_Day <- ifelse(is.na(Fraud_Data$CUST_DT_Day), na.omit(Fraud_Data$CUST_DT_Day), Fraud_Data$CUST_DT_Day)
Fraud_Data$CUST_DT_Year <- ifelse(is.na(Fraud_Data$CUST_DT_Year), na.omit(Fraud_Data$CUST_DT_Year), Fraud_Data$CUST_DT_Year)
Fraud_Data$TR_DT_Month <- ifelse(is.na(Fraud_Data$TR_DT_Month), na.omit(Fraud_Data$TR_DT_Month), Fraud_Data$TR_DT_Month)
Fraud_Data$TR_DT_Day <- ifelse(is.na(Fraud_Data$TR_DT_Day), na.omit(Fraud_Data$TR_DT_Day), Fraud_Data$TR_DT_Day)
Fraud_Data$TR_DT_Year <- ifelse(is.na(Fraud_Data$TR_DT_Year), na.omit(Fraud_Data$TR_DT_Year), Fraud_Data$TR_DT_Year)
Fraud_Data$ACTVY_Month <- ifelse(is.na(Fraud_Data$ACTVY_Month), na.omit(Fraud_Data$ACTVY_Month), Fraud_Data$ACTVY_Month)
Fraud_Data$ACTVY_Day <- ifelse(is.na(Fraud_Data$ACTVY_Day), na.omit(Fraud_Data$ACTVY_Day), Fraud_Data$ACTVY_Day)
Fraud_Data$ACTVY_Year <- ifelse(is.na(Fraud_Data$ACTVY_Year), na.omit(Fraud_Data$ACTVY_Year), Fraud_Data$ACTVY_Year)

#Carr name consolidation
Fraud_Data$CARR_NAME <- ifelse(Fraud_Data$CARR_NAME != 'other/unlisted', 'listed', 'other/unlisted')
#RGN Name cleaning
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "mid atlantic", "northeast", Fraud_Data$RGN_NAME)
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "pacific northwest", "northwest", Fraud_Data$RGN_NAME)
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "pacific", "west coast", Fraud_Data$RGN_NAME)
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "south central", "south central", Fraud_Data$RGN_NAME)
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "north west", "northwest", Fraud_Data$RGN_NAME)
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "south east", "southeast", Fraud_Data$RGN_NAME)
Fraud_Data$RGN_NAME <- ifelse(Fraud_Data$RGN_NAME == "great lakes", "great lakes",
                              ifelse(Fraud_Data$RGN_NAME == "southwest", "southwest",
                                     ifelse(Fraud_Data$RGN_NAME == "mountain", "mountain", 
                                            ifelse(Fraud_Data$RGN_NAME == "midwest", "midwest",
                                                   ifelse(Fraud_Data$RGN_NAME == "unknown", "unknown", "international")))))




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
#get predictors without response (and those predictors with only one value as well as date/timestamp values)
Fraud_Data_predictors <- select(Fraud_Data, -FRAUD_NONFRAUD, -ACTN_CD, -ACTN_INTNL_TXT, -TRAN_TYPE_CD)
#create dummy vars expect for the response
dummies_model <- dummyVars(~., data = Fraud_Data_predictors)

#provide only predictors that are now convert to dummy variables
Fraud_Data_predictors_dummy <- data.frame(predict(dummies_model, newdata = Fraud_Data))

#recombine predictors including dummy variables with response
Fraud_Data <- cbind(FRAUD_NONFRAUD = Fraud_Data$FRAUD_NONFRAUD, Fraud_Data_predictors_dummy)

#Convert FRAUD_NOFRAUD to a factor
Fraud_Data$FRAUD_NONFRAUD <- as.factor(Fraud_Data$FRAUD_NONFRAUD)
Fraud_Data$FRAUD_NONFRAUD <- fct_recode(Fraud_Data$FRAUD_NONFRAUD, NonFraud = 'Non-Fraud', Fraud = 'Fraud')

set.seed(99)
index <- createDataPartition(Fraud_Data$FRAUD_NONFRAUD,p = .8, list = FALSE)
Fraud_data_train <- Fraud_Data[index,]
Fraud_data_test <- Fraud_Data[-index,]

#XGBoost
library(xgboost)
set.seed(99)
Fraud_xgb_model <- train(FRAUD_NONFRAUD~.,
                         data = Fraud_data_train,
                         method = 'xgbTree',
                         trControl = trainControl(method = 'cv', number = 5,
                                                  #Estimate class probabilities
                                                  classProbs = TRUE,
                                                  #needed to get ROC
                                                  summaryFunction = twoClassSummary),
                         metric = 'ROC')
#Prunning to avoid overfitting



#establish Holdout data
Fraud_Data_Holdout <- (read.csv(file = 'New_Alliance_holdout_.csv', header = T))

library(skimr)
summaryStatsHoldout <- skim(Fraud_Data_Holdout)
summaryStatsHoldout

#Create Month, Day, Year variables from transaction timestamp
library(lubridate)

Fraud_Data_Holdout$PWD_UPDT_TS <- ifelse(is.na(Fraud_Data_Holdout$PWD_UPDT_TS), Fraud_Data_Holdout$CUST_SINCE_DT, Fraud_Data_Holdout$PWD_UPDT_TS)
Fraud_Data_Holdout$PH_NUM_UPDT_TS <- ifelse(is.na(Fraud_Data_Holdout$PH_NUM_UPDT_TS), Fraud_Data_Holdout$CUST_SINCE_DT, Fraud_Data_Holdout$PH_NUM_UPDT_TS)
Fraud_Data_Holdout$CUST_SINCE_DT <- ifelse(is.na(Fraud_Data_Holdout$CUST_SINCE_DT), na.omit(Fraud_Data_Holdout$CUST_SINCE_DT), Fraud_Data_Holdout$CUST_SINCE_DT)
Fraud_Data_Holdout$TRAN_TS <- ifelse(is.na(Fraud_Data_Holdout$TRAN_TS), na.omit(Fraud_Data_Holdout$TRAN_TS), Fraud_Data_Holdout$TRAN_TS)
Fraud_Data_Holdout$TRAN_DT <- ifelse(is.na(Fraud_Data_Holdout$TRAN_DT), na.omit(Fraud_Data_Holdout$TRAN_DT), Fraud_Data_Holdout$TRAN_DT)
Fraud_Data_Holdout$ACTVY_DT <- ifelse(is.na(Fraud_Data_Holdout$ACTVY_DT), na.omit(Fraud_Data_Holdout$ACTVY_DT), Fraud_Data_Holdout$ACTVY_DT)

#Date and timestamp handling
Fraud_Data_Holdout <- mutate(Fraud_Data_Holdout, TS_Month = month(as.Date(Fraud_Data_Holdout$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data_Holdout, TS_Day = day(as.Date(Fraud_Data_Holdout$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data_Holdout, TS_Year = year(as.Date(Fraud_Data_Holdout$TRAN_TS, '%m/%d/%Y')),
                     Fraud_Data_Holdout, PWD_Month = month(as.Date(Fraud_Data_Holdout$PWD_UPDT_TS,'%m/%d/%Y')),
                     Fraud_Data_Holdout, PWD_Day = day(as.Date(Fraud_Data_Holdout$PWD_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data_Holdout, PWD_Year = year(as.Date(Fraud_Data_Holdout$PWD_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data_Holdout, PH_Month = month(as.Date(Fraud_Data_Holdout$PH_NUM_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data_Holdout, PH_Day = day(as.Date(Fraud_Data_Holdout$PH_NUM_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data_Holdout, PH_Year = year(as.Date(Fraud_Data_Holdout$PH_NUM_UPDT_TS, '%m/%d/%Y')),
                     Fraud_Data_Holdout, CUST_DT_Month = month(as.Date(Fraud_Data_Holdout$CUST_SINCE_DT, '%m/%d/%Y')),
                     Fraud_Data_Holdout, CUST_DT_Day = day(as.Date(Fraud_Data_Holdout$CUST_SINCE_DT, '%m/%d/%Y')),
                     Fraud_Data_Holdout, CUST_DT_Year = year(as.Date(Fraud_Data_Holdout$CUST_SINCE_DT, '%m/%d/%Y')),
                     Fraud_Data_Holdout, TR_DT_Month = month(as.Date(Fraud_Data_Holdout$TRAN_DT, '%m/%d/%Y')),
                     Fraud_Data_Holdout, TR_DT_Day = day(as.Date(Fraud_Data_Holdout$TRAN_DT, '%m/%d/%Y')),
                     Fraud_Data_Holdout, TR_DT_Year = year(as.Date(Fraud_Data_Holdout$TRAN_DT, '%m/%d/%Y')),
                     Fraud_Data_Holdout, ACTVY_Month = month(as.Date(Fraud_Data_Holdout$ACTVY_DT, '%m/%d/%Y')),
                     Fraud_Data_Holdout, ACTVY_Day = day(as.Date(Fraud_Data_Holdout$ACTVY_DT, '%m/%d/%Y')),
                     Fraud_Data_Holdout, ACTVY_Year = year(as.Date(Fraud_Data_Holdout$ACTVY_DT, '%m/%d/%Y')))

Fraud_Data_Holdout$PWD_UPDT_TS <- NULL
Fraud_Data_Holdout$PH_NUM_UPDT_TS <- NULL
Fraud_Data_Holdout$CUST_SINCE_DT <- NULL
Fraud_Data_Holdout$TRAN_TS <- NULL
Fraud_Data_Holdout$TRAN_DT <- NULL
Fraud_Data_Holdout$ACTVY_DT <- NULL

#Missing Value Handling
Fraud_Data_Holdout$TRAN_ID <- ifelse(is.na(Fraud_Data_Holdout$TRAN_ID), na.omit(Fraud_Data_Holdout$TRAN_ID), Fraud_Data_Holdout$TRAN_ID)
Fraud_Data_Holdout$TRAN_AMT <- ifelse(is.na(Fraud_Data_Holdout$TRAN_AMT), na.omit(Fraud_Data_Holdout$TRAN_AMT), Fraud_Data_Holdout$TRAN_AMT)
Fraud_Data_Holdout$ACCT_PRE_TRAN_AVAIL_BAL <- ifelse(is.na(Fraud_Data_Holdout$ACCT_PRE_TRAN_AVAIL_BAL), na.omit(Fraud_Data_Holdout$ACCT_PRE_TRAN_AVAIL_BAL), Fraud_Data_Holdout$ACCT_PRE_TRAN_AVAIL_BAL)
Fraud_Data_Holdout$CUST_AGE <- ifelse(is.na(Fraud_Data_Holdout$CUST_AGE), round(mean(Fraud_Data_Holdout$CUST_AGE), 0), Fraud_Data_Holdout$CUST_AGE)
Fraud_Data_Holdout$OPEN_ACCT_CT <- ifelse(is.na(Fraud_Data_Holdout$OPEN_ACCT_CT), na.omit(Fraud_Data_Holdout$OPEN_ACCT_CT), Fraud_Data_Holdout$OPEN_ACCT_CT)
Fraud_Data_Holdout$WF_dvc_age <- ifelse(is.na(Fraud_Data_Holdout$WF_dvc_age), round(mean(Fraud_Data_Holdout$WF_dvc_age), 0), Fraud_Data_Holdout$WF_dvc_age)
Fraud_Data_Holdout$CARR_NAME <- ifelse(is.na(Fraud_Data_Holdout$CARR_NAME), "other/unlisted", Fraud_Data_Holdout$CARR_NAME)
Fraud_Data_Holdout$RGN_NAME <- ifelse(is.na(Fraud_Data_Holdout$RGN_NAME), "unknown", Fraud_Data_Holdout$RGN_NAME)
Fraud_Data_Holdout$STATE_PRVNC_TXT <- ifelse(is.na(Fraud_Data_Holdout$STATE_PRVNC_TXT), "unknown", Fraud_Data_Holdout$STATE_PRVNC_TXT)
Fraud_Data_Holdout$ALERT_TRGR_CD <- ifelse(is.na(Fraud_Data_Holdout$ALERT_TRGR_CD), "NO_DEVICE_ON_RECORD", Fraud_Data_Holdout$ALERT_TRGR_CD)
Fraud_Data_Holdout$DVC_TYPE_TXT <- ifelse(is.na(Fraud_Data_Holdout$DVC_TYPE_TXT), "NO_DEVICE_ON_RECORD", Fraud_Data_Holdout$DVC_TYPE_TXT)
Fraud_Data_Holdout$AUTHC_PRIM_TYPE_CD <- ifelse(is.na(Fraud_Data_Holdout$AUTHC_PRIM_TYPE_CD), "NO_AUTH_REQUIRED", Fraud_Data_Holdout$AUTHC_PRIM_TYPE_CD)
Fraud_Data_Holdout$AUTHC_SCNDRY_STAT_TXT <- ifelse(is.na(Fraud_Data_Holdout$AUTHC_SCNDRY_STAT_TXT), "AUTH_BYPASSED", Fraud_Data_Holdout$AUTHC_SCNDRY_STAT_TXT)
Fraud_Data_Holdout$CUST_ZIP <- ifelse(is.na(Fraud_Data_Holdout$CUST_ZIP), na.omit(Fraud_Data_Holdout$CUST_ZIP), Fraud_Data_Holdout$CUST_ZIP)
Fraud_Data_Holdout$CUST_STATE <- ifelse(is.na(Fraud_Data_Holdout$CUST_STATE), na.omit(Fraud_Data_Holdout$CUST_STATE), Fraud_Data_Holdout$CUST_STATE)
Fraud_Data_Holdout$ACTN_CD <- ifelse(is.na(Fraud_Data_Holdout$ACTN_CD), "SCHPMT", Fraud_Data_Holdout$ACTN_CD)
Fraud_Data_Holdout$ACTN_INTNL_TXT <- ifelse(is.na(Fraud_Data_Holdout$ACTN_INTNL_TXT), "P2P_COMMIT", Fraud_Data_Holdout$ACTN_INTNL_TXT)
Fraud_Data_Holdout$TRAN_TYPE_CD <- ifelse(is.na(Fraud_Data_Holdout$TRAN_TYPE_CD), "P2P", Fraud_Data_Holdout$TRAN_TYPE_CD)
Fraud_Data_Holdout$TS_Month <- ifelse(is.na(Fraud_Data_Holdout$TS_Month), na.omit(Fraud_Data_Holdout$TS_Month), Fraud_Data_Holdout$TS_Month)
Fraud_Data_Holdout$TS_Day <- ifelse(is.na(Fraud_Data_Holdout$TS_Day), na.omit(Fraud_Data_Holdout$TS_Day), Fraud_Data_Holdout$TS_Day)
Fraud_Data_Holdout$TS_Year <- ifelse(is.na(Fraud_Data_Holdout$TS_Year), na.omit(Fraud_Data_Holdout$TS_Year), Fraud_Data_Holdout$TS_Year)
Fraud_Data_Holdout$PWD_Month <- ifelse(is.na(Fraud_Data_Holdout$PWD_Month), na.omit(Fraud_Data_Holdout$PWD_Month), Fraud_Data_Holdout$PWD_Month)
Fraud_Data_Holdout$PWD_Day <- ifelse(is.na(Fraud_Data_Holdout$PWD_Day), na.omit(Fraud_Data_Holdout$PWD_Day), Fraud_Data_Holdout$PWD_Day)
Fraud_Data_Holdout$PWD_Year <- ifelse(is.na(Fraud_Data_Holdout$PWD_Year), na.omit(Fraud_Data_Holdout$PWD_Year), Fraud_Data_Holdout$PWD_Year)
Fraud_Data_Holdout$PH_Month <- ifelse(is.na(Fraud_Data_Holdout$PH_Month), na.omit(Fraud_Data_Holdout$PH_Month), Fraud_Data_Holdout$PH_Month)
Fraud_Data_Holdout$PH_Day <- ifelse(is.na(Fraud_Data_Holdout$PH_Day), na.omit(Fraud_Data_Holdout$PH_Day), Fraud_Data_Holdout$PH_Day)
Fraud_Data_Holdout$PH_Year <- ifelse(is.na(Fraud_Data_Holdout$PH_Year), na.omit(Fraud_Data_Holdout$PH_Year), Fraud_Data_Holdout$PH_Year)
Fraud_Data_Holdout$CUST_DT_Month <- ifelse(is.na(Fraud_Data_Holdout$CUST_DT_Month), na.omit(Fraud_Data_Holdout$CUST_DT_Month), Fraud_Data_Holdout$CUST_DT_Month)
Fraud_Data_Holdout$CUST_DT_Day <- ifelse(is.na(Fraud_Data_Holdout$CUST_DT_Day), na.omit(Fraud_Data_Holdout$CUST_DT_Day), Fraud_Data_Holdout$CUST_DT_Day)
Fraud_Data_Holdout$CUST_DT_Year <- ifelse(is.na(Fraud_Data_Holdout$CUST_DT_Year), na.omit(Fraud_Data_Holdout$CUST_DT_Year), Fraud_Data_Holdout$CUST_DT_Year)
Fraud_Data_Holdout$CUST_DT_Month <- ifelse(is.na(Fraud_Data_Holdout$CUST_DT_Month), na.omit(Fraud_Data_Holdout$CUST_DT_Month), Fraud_Data_Holdout$CUST_DT_Month)
Fraud_Data_Holdout$CUST_DT_Day <- ifelse(is.na(Fraud_Data_Holdout$CUST_DT_Day), na.omit(Fraud_Data_Holdout$CUST_DT_Day), Fraud_Data_Holdout$CUST_DT_Day)
Fraud_Data_Holdout$CUST_DT_Year <- ifelse(is.na(Fraud_Data_Holdout$CUST_DT_Year), na.omit(Fraud_Data_Holdout$CUST_DT_Year), Fraud_Data_Holdout$CUST_DT_Year)
Fraud_Data_Holdout$TR_DT_Month <- ifelse(is.na(Fraud_Data_Holdout$TR_DT_Month), na.omit(Fraud_Data_Holdout$TR_DT_Month), Fraud_Data_Holdout$TR_DT_Month)
Fraud_Data_Holdout$TR_DT_Day <- ifelse(is.na(Fraud_Data_Holdout$TR_DT_Day), na.omit(Fraud_Data_Holdout$TR_DT_Day), Fraud_Data_Holdout$TR_DT_Day)
Fraud_Data_Holdout$TR_DT_Year <- ifelse(is.na(Fraud_Data_Holdout$TR_DT_Year), na.omit(Fraud_Data_Holdout$TR_DT_Year), Fraud_Data_Holdout$TR_DT_Year)
Fraud_Data_Holdout$ACTVY_Month <- ifelse(is.na(Fraud_Data_Holdout$ACTVY_Month), na.omit(Fraud_Data_Holdout$ACTVY_Month), Fraud_Data_Holdout$ACTVY_Month)
Fraud_Data_Holdout$ACTVY_Day <- ifelse(is.na(Fraud_Data_Holdout$ACTVY_Day), na.omit(Fraud_Data_Holdout$ACTVY_Day), Fraud_Data_Holdout$ACTVY_Day)
Fraud_Data_Holdout$ACTVY_Year <- ifelse(is.na(Fraud_Data_Holdout$ACTVY_Year), na.omit(Fraud_Data_Holdout$ACTVY_Year), Fraud_Data_Holdout$ACTVY_Year)



#Does transaction take place for customer who owns no accounts? 
Fraud_Data_Holdout$NonAccountTrans <- c()
Fraud_Data_Holdout$NonAccountTrans <- ifelse(Fraud_Data_Holdout$OPEN_ACCT_CT == 0, "Yes","No")



#Number of accounts in customer household. 
Fraud_Data_Holdout$NumbAccountBin <- c()
Fraud_Data_Holdout$NumbAccountBin[Fraud_Data_Holdout$OPEN_ACCT_CT == 0] <- "None"
Fraud_Data_Holdout$NumbAccountBin[Fraud_Data_Holdout$OPEN_ACCT_CT > 0 & Fraud_Data_Holdout$OPEN_ACCT_CT <= 3] <- "1 to 3"
Fraud_Data_Holdout$NumbAccountBin[Fraud_Data_Holdout$OPEN_ACCT_CT > 3 & Fraud_Data_Holdout$OPEN_ACCT_CT <= 7] <- "4 to 7"
Fraud_Data_Holdout$NumbAccountBin[Fraud_Data_Holdout$OPEN_ACCT_CT > 7 & Fraud_Data_Holdout$OPEN_ACCT_CT <= 10] <- "8 to 10"
Fraud_Data_Holdout$NumbAccountBin[Fraud_Data_Holdout$OPEN_ACCT_CT > 10] <- "Over 10"

#Build vector of US states to determine, is transaction in US or not.
Fraud_Data_Holdout$Foreign <- ifelse(Fraud_Data_Holdout$STATE_PRVNC_TXT %in% c("alabama","alaska", "arizon", "arkansas",
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

#Carr name consolidation
Fraud_Data_Holdout$CARR_NAME <- ifelse(Fraud_Data_Holdout$CARR_NAME != 'other/unlisted', 'listed', 'other/unlisted')
#RGN Name cleaning
Fraud_Data_Holdout$RGN_NAME <- ifelse(Fraud_Data_Holdout$RGN_NAME == "mid atlantic", "northeast", Fraud_Data_Holdout$RGN_NAME)
Fraud_Data_Holdout$RGN_NAME <- ifelse(Fraud_Data_Holdout$RGN_NAME == "pacific northwest", "northwest", Fraud_Data_Holdout$RGN_NAME)
Fraud_Data_Holdout$RGN_NAME <- ifelse(Fraud_Data_Holdout$RGN_NAME == "pacific", "west coast", Fraud_Data_Holdout$RGN_NAME)
Fraud_Data_Holdout$RGN_NAME <- ifelse(Fraud_Data_Holdout$RGN_NAME == "south central", "south central", Fraud_Data_Holdout$RGN_NAME)
Fraud_Data_Holdout$RGN_NAME <- ifelse(Fraud_Data_Holdout$RGN_NAME == "north west", "northwest", Fraud_Data_Holdout$RGN_NAME)
Fraud_Data_Holdout$RGN_NAME <- ifelse(Fraud_Data_Holdout$RGN_NAME == "south east", "southeast", Fraud_Data_Holdout$RGN_NAME)
Fraud_Data_Holdout$RGN_NAME <- ifelse(Fraud_Data_Holdout$RGN_NAME == "great lakes", "great lakes",
                              ifelse(Fraud_Data_Holdout$RGN_NAME == "southwest", "southwest",
                                     ifelse(Fraud_Data_Holdout$RGN_NAME == "mountain", "mountain", 
                                            ifelse(Fraud_Data_Holdout$RGN_NAME == "midwest", "midwest",
                                                   ifelse(Fraud_Data_Holdout$RGN_NAME == "unknown", "unknown", "international")))))




#Transaction amount greater than available cash? 
Fraud_Data_Holdout$Can_Afford <- c()
Fraud_Data_Holdout$Can_Afford <- ifelse(Fraud_Data_Holdout$ACCT_PRE_TRAN_AVAIL_BAL >= Fraud_Data_Holdout$TRAN_AMT,1,0)


#Group by Age
Fraud_Data_Holdout$AgeBin <- c()
Fraud_Data_Holdout$AgeBin[Fraud_Data_Holdout$CUST_AGE <= 25] <- "25 and Younger"
Fraud_Data_Holdout$AgeBin[Fraud_Data_Holdout$CUST_AGE > 25 & Fraud_Data_Holdout$CUST_AGE <= 49] <- "26 to 49"
Fraud_Data_Holdout$AgeBin[Fraud_Data_Holdout$CUST_AGE > 49 & Fraud_Data_Holdout$CUST_AGE <= 70] <- "50 to 70"
Fraud_Data_Holdout$AgeBin[Fraud_Data_Holdout$CUST_AGE > 70] <- "Over 70"

Fraud_Data_Holdout <- data.frame(predict(dummies_model, newdata = Fraud_Data_Holdout))
Fraud_Data_Holdout$STATE_PRVNC_TXT.amman <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXT.amman <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTaguascalientes <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTaguascalientes <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTandhra.pradesh <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTandhra.pradesh <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTantioquia <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTantioquia <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTaragua <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTaragua <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTbaja.california <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTbaja.california <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTbolivar <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTbolivar <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTbritish.columbia <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTbritish.columbia <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTcarabobo <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTcarabobo <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTchihuahua <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTchihuahua <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTcolima <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTcolima <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTda.nang <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTda.nang <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTdelaware <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTdelaware <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTdelhi <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTdelhi <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTdublin <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTdublin <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTgauteng <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTgauteng <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTgreater.accra <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTgreater.accra <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTguanacaste <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTguanacaste <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTguerrero <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTguerrero <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTgujarat <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTgujarat <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXThamerkaz <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXThamerkaz <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXThessen <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXThessen <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXThong.kong <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXThong.kong <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTistanbul <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTistanbul <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTkinshasa <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTkinshasa <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTlara <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTlara <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTliverpool <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTliverpool <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTloire.atlantique <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTloire.atlantique <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTmadrid <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTmadrid <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTmaine <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTmaine <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTmazowieckie <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTmazowieckie <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTmichoacan.de.ocampo <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTmichoacan.de.ocampo <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTnairobi.area <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTnairobi.area <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTnew.south.wales <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTnew.south.wales <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTnoord.brabant <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTnoord.brabant <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTnordrhein.westfalen <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTnordrhein.westfalen <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTnueva.esparta <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTnueva.esparta <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTontario <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTontario <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTostergotlands.lan <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTostergotlands.lan <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTparis <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTparis <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTphnum.penh <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTphnum.penh <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTpichincha <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTpichincha <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTpunjab <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTpunjab <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTra.s.al.khaymah <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTra.s.al.khaymah <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTrhode.island <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTrhode.island <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTsaint.george.s <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTsaint.george.s <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTsan.salvador <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTsan.salvador <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTsao.paulo <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTsao.paulo <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTseoul.teukbyeolsi <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTseoul.teukbyeolsi <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTsinaloa <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTsinaloa <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTsouth.australia <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTsouth.australia <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTsouth.west <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTsouth.west <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTtaipei <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTtaipei <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTtamaulipas <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTtamaulipas <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTtelangana <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTtelangana <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTtokyo <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTtokyo <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTvictoria <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTvictoria <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTwest.bengal <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTwest.bengal <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTwest.virginia <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTwest.virginia <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTwestern.cape <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTwestern.cape <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTzuerich <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTzuerich <- 0
Fraud_Data_Holdout$STATE_PRVNC_TXTzulia <- c()
Fraud_Data_Holdout$STATE_PRVNC_TXTzulia <- 0
Fraud_Data_Holdout$CUST_STATEAR <- c()
Fraud_Data_Holdout$CUST_STATEAR <- 0
Fraud_Data_Holdout$CUST_STATEGU <- c()
Fraud_Data_Holdout$CUST_STATEGU <- 0
Fraud_Data_Holdout$CUST_STATEIN <- c()
Fraud_Data_Holdout$CUST_STATEIN <- 0
Fraud_Data_Holdout$CUST_STATEME <- c()
Fraud_Data_Holdout$CUST_STATEME <- 0
Fraud_Data_Holdout$CUST_STATEOK <- c()
Fraud_Data_Holdout$CUST_STATEOK <- 0
Fraud_Data_Holdout$CUST_STATEWV <- c()
Fraud_Data_Holdout$CUST_STATEWV <- 0


predprob_fraud_xgb_holdout <- predict(Fraud_xgb_model, Fraud_Data_Holdout, type = 'prob')
#use cbind() to bring together predprob[,2] with tran_id
cmb_NonFraud_Tran <- cbind(TRAN_ID = Fraud_Data_Holdout$TRAN_ID, Fraud_Prob = predprob_fraud_xgb_holdout$Fraud)
cmb_NonFraud_Tran
