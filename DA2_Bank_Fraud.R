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
Fraud_Data$TS_Month <- ifelse(is.na(Fraud_Data$TS_Month), na.omit(Fraud_Data$TS_Month), Fraud_Data$TS_Month)
Fraud_Data$TS_Day <- ifelse(is.na(Fraud_Data$TS_Day), na.omit(Fraud_Data$TS_Day), Fraud_Data$TS_Day)
Fraud_Data$TS_Year <- ifelse(is.na(Fraud_Data$TS_Year), na.omit(Fraud_Data$TS_Year), Fraud_Data$TS_Year)

#Consolidate Carriers to show as listed or unlisted.
Fraud_Data$CARR_NAME <- ifelse(Fraud_Data$CARR_NAME != 'other/unlisted', 'listed', 'other/unlisted')

#Clean and consolidate region names
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
ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = TS_Month, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Scatter plot for Transaction amt by customer age, fraud or not
ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = CUST_AGE, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Transaction amount by age groupings, fraud or not
ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = AgeBin, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Scatter plot for Transaction amt by day of the month, fraud or not
ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = TS_Day, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Scatter plot for Transaction amt by number of accts customer has at bank, fraud or not
ggplot(data = Fraud_Data) + 
  geom_point(mapping = aes(x = NumbAccountBin, y = TRAN_AMT, color = FRAUD_NONFRAUD))
#Bar chart for transaction by location of transaction (US OR INTL), fraud or not
ggplot(data = Fraud_Data) + 
  geom_bar(mapping = aes(x = Foreign, fill = FRAUD_NONFRAUD))

Carrier_FvNF <- ggplot(data = Fraud_Data)+
  stat_count(mapping = aes(x = CARR_NAME, color = FRAUD_NONFRAUD))

Carrier_fraud <- Fraud_Data %>% 
  group_by(CARR_NAME, FRAUD_NONFRAUD) %>%
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  filter(FRAUD_NONFRAUD == "Fraud",
         count > 50)
               

#Dummy Variables
#get predictors without response (and those predictors with only one value as well as date/timestamp values)
Fraud_Data_predictors <- select(Fraud_Data, -FRAUD_NONFRAUD, -ACTN_CD, -ACTN_INTNL_TXT, -TRAN_TYPE_CD, 
                                -PWD_UPDT_TS, -PH_NUM_UPDT_TS, -TRAN_TS, -TRAN_DT, -CUST_SINCE_DT, 
                                -ACTVY_DT)
#create dummy vars expect for the response
dummies_model <- dummyVars(~., data = Fraud_Data_predictors)

#provide only predictors that are now convert to dummy variables
Fraud_Data_predictors_dummy <- data.frame(predict(dummies_model, newdata = Fraud_Data))

#recombine predictors including dummy variables with response
Fraud_Data <- cbind(FRAUD_NONFRAUD = Fraud_Data$FRAUD_NONFRAUD, PWD_UPDT_TS = Fraud_Data$PWD_UPDT_TS, PH_NUM_UPDT_TS = Fraud_Data$PH_NUM_UPDT_TS,
                    TRAN_TS = Fraud_Data$TRAN_TS, TRAN_DT = Fraud_Data$TRAN_DT, CUST_SINCE_DT = Fraud_Data$CUST_SINCE_DT, 
                    ACTVY_DT = Fraud_Data$ACTVY_DT, Fraud_Data_predictors_dummy)

#Convert FRAUD_NOFRAUD to a factor
Fraud_Data$FRAUD_NONFRAUD <- as.factor(Fraud_Data$FRAUD_NONFRAUD)
Fraud_Data$FRAUD_NONFRAUD <- fct_recode(Fraud_Data$FRAUD_NONFRAUD, NonFraud = 'Non-Fraud', Fraud = 'Fraud')


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
