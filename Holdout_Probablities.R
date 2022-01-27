
library(tidyverse)
library(caret)

Fraud_Data <- (read.csv(file = 'New_Alliance_holdout_.csv', header = T))

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

#Scatter plot for Transaction amt by month of the year, fraud or not
Fraud_Month <- ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = TS_Month, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Scatter plot for Transaction amt by customer age, fraud or not
Fraud_Age <- ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = CUST_AGE, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Transaction amount by age groupings, fraud or not
Fraud_Age_Groupings <- ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = AgeBin, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Scatter plot for Transaction amt by day of the month, fraud or not
Fraud_Day <- ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = TS_Day, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Scatter plot for Transaction amt by number of accts customer has at bank, fraud or not
Fraud_NumbAccts <- ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = NumbAccountBin, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Bar chart for transaction by location of transaction (US OR INTL), fraud or not
Fraud_Foreign <- ggplot(data = Fraud_Data) + 
  geom_bar(mapping = aes(x = Foreign, fill = FRAUD_NONFRAUD))

ggplot(data = Fraud_Data)+
  stat_count(mapping = aes(x = CARR_NAME, color = FRAUD_NONFRAUD))

Carrier_list_Unlist <- Fraud_Data %>% 
  group_by(CARR_NAME, FRAUD_NONFRAUD) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(FRAUD_NONFRAUD == "Fraud",
         count > 50)


#Dummy Variables
#get predictors without response (and those predictors with only one value as well as date/timestamp values)
Fraud_Data_predictors <- select(Fraud_Data, -FRAUD_NONFRAUD, -ACTN_CD, -ACTN_INTNL_TXT, -TRAN_TYPE_CD)
#create dummy vars expect for the response
dummies_model <- dummyVars(~., data = Fraud_Data_predictors)

#provide only predictors that are now convert to dummy variables
Fraud_Data_predictors_dummy <- data.frame(predict(dummies_model, newdata = Fraud_Data))

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

Fraud_xgb_model

plot(Fraud_xgb_model) #provides plot of parameter tuning via cross validation

#plot variable importance
plot(varImp(Fraud_xgb_model))

predprob_fraud_xgb <- predict(Fraud_xgb_model, Fraud_data_test, type = 'prob')
#use cbind() to bring together predprob[,2] with tran_id
cmb_NonFraud_Tran <- cbind(TRAN_ID = Fraud_data_test$TRAN_ID, NonFraud_Prob = predprob_fraud_xgb$NonFraud)
cmb_NonFraud_Tran
