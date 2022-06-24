library(tidyverse)
library(skimr)
library(lubridate)

scipen = 9999

messages <- read_csv("Network_Messages_Capstone.csv")
#skim(messages)

#Convert relevant timestamps to datetime
messages$delivered_at <- ymd_hms(messages$delivered_at)
messages$notif_read_at <- ymd_hms(messages$notif_read_at)

#impute standard date in future for missing values of messages not read
messages$notif_read_at_na <- ifelse(is.na(messages$notif_read_at), as.POSIXct("2022-12-01"), messages$notif_read_at) 
messages$notif_delivered_at_na <- ifelse(is.na(messages$delivered_at), as.POSIXct("2022-12-01"), messages$delivered_at)   

#Create variable RRTM for timliness of a message being read
messages$RRTM <- format((messages$notif_read_at_na - messages$notif_delivered_at_na) / 60, scientific = FALSE)
messages$RRTM <- as.numeric(messages$RRTM)


#Variables needed for feature engineering
avg_char_count <- mean(messages$character_length)
avg_subject_length <- mean(messages$subject_length)
avg_RRTM <- mean(messages$RRTM)

#Date (Day and Month and Year)
messages$read_day <- day(messages$notif_read_at)
messages$read_day <- ifelse(is.na(messages$read_day), 1, messages$read_day)
messages$read_month <- month(messages$notif_read_at)
messages$read_month <- ifelse(is.na(messages$read_month), 12, messages$read_month)
messages$read_year <- year(messages$notif_read_at)
messages$read_year <- ifelse(is.na(messages$read_year), 2022, messages$read_year)
messages$delivered_day <- day(messages$delivered_at)
messages$delivered_day <- ifelse(is.na(messages$delivered_day), na.omit(messages$delivered_day), messages$delivered_day)
messages$delivered_month <- month(messages$delivered_at)
messages$delivered_month <- ifelse(is.na(messages$delivered_month), na.omit(messages$delivered_month), messages$delivered_month)
messages$delivered_year <- year(messages$delivered_at)
messages$delivered_year <- ifelse(is.na(messages$delivered_year), na.omit(messages$delivered_year), messages$delivered_year)


#Bins for length of time
messages$TimeBin <- c()
messages$TimeBin[messages$RRTM <= 5] <- "5 minutes or less"
messages$TimeBin[messages$RRTM >5 & messages$RRTM <=60] <- "5m to 1hr"
messages$TimeBin[messages$RRTM >60 & messages$RRTM <=480] <- "1 to 8hrs"
messages$TimeBin[messages$RRTM >480 & messages$RRTM <=1440] <- "8hrs to 1 day"
messages$TimeBin[messages$RRTM >1440 & messages$RRTM <=10080] <- "1 to 7 days"
messages$TimeBin[messages$RRTM >10080 & messages$RRTM <=43200] <- "Over 1 Week"
messages$TimeBin[messages$RRTM >43200 & messages$RRTM <=345600] <- "Over 4 Months"
messages$TimeBin[messages$RRTM >345600 & messages$RRTM <=500000] <- "Over 8 Months"
messages$TimeBin[messages$RRTM >500000] <- "Unread Msg"

#Remove Features no longer needed
messages$notif_read_at <- NULL
messages$notif_details_read_at <- NULL
messages$delivered_at <- NULL
messages$created_at <- NULL
messages$updated_at <- NULL
messages$notif_sent <- NULL
messages$conversation_id <- NULL
messages$message_id...8 <- NULL
messages$message_id...11 <- NULL
messages$mesg_created_at <- NULL
messages$mesg_updated_at <- NULL
messages$mesg_sender_id <- NULL
messages$content_type <- NULL
messages$use_case <- NULL
messages$notif_created_at <- NULL
messages$notif_updated_at <- NULL
messages$mesg_date <- NULL

#Time to read message greater than average
messages$Long_Read_Time <- ifelse(messages$RRTM > avg_RRTM, 1, 0)

#Long Read Time and attachment included
messages$Attachment_LRT <- ifelse((messages$RRTM > avg_RRTM) & messages$attachments == "TRUE", 1, 0)
messages$EmbAttachment_LRT <- ifelse((messages$RRTM > avg_RRTM) & messages$embed_attachment == "TRUE", 1, 0)
messages$Long_Subject_RT <- ifelse((messages$subject_length > avg_subject_length) & messages$RRTM > avg_RRTM, 1, 0)
messages$Large_Char_RT <- ifelse((messages$character_length > avg_char_count) & messages$RRTM > avg_RRTM, 1,0)

#Quick Read and any attachments
messages$PromptRead_AnyAttach <- ifelse((messages$TimeBin == "5 minutes or less") &
                                          messages$attachments == "TRUE" &
                                          messages$embed_attachment == "TRUE", 1,0)
#Quick read and long subject plus embedded attachment
messages$PromptRead_LS_EmbAttach <- ifelse((messages$TimeBin == "5 minutes or less") &
                                            messages$subject_length > avg_subject_length &
                                            messages$embed_attachment == "TRUE", 1,0)

#Quick read and long character count plus embedded attachment
messages$PromptRead_LC_EmbAttach <- ifelse((messages$TimeBin == "5 minutes or less") &
                                         messages$character_length > avg_char_count &
                                         messages$embed_attachment == "TRUE", 1, 0)

#skim(messages)






write.csv(messages, "messages_250K_with_DVs.csv")

ggplot(data = messages) + 
  geom_bar(mapping = aes(x = embed_attachment, fill = notif_read))

ggplot(data = messages) + 
  geom_bar(mapping = aes(x = attachments, fill = notif_read))

ggplot(data = messages) + 
  geom_bar(mapping = aes(x = PromptRead_LC_EmbAttach, fill = notif_read))

ggplot(data = messages) + 
  geom_bar(mapping = aes(x = TimeBin)) +
  coord_flip()

