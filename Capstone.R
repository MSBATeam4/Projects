library(tidyverse)
library(skimr)
library(lubridate)

scipen = 9999

messages <- read_csv("Network_Messages_Capstone.csv")
skim(messages)


messages$delivered_at <- ymd_hms(messages$delivered_at)
messages$notif_read_at <- ymd_hms(messages$notif_read_at)

messages$notif_read_at_na <- ifelse(is.na(messages$notif_read_at), as.POSIXct("2022-12-01"), messages$notif_read_at) 
messages$notif_delivered_at_na <- ifelse(is.na(messages$delivered_at), as.POSIXct("2022-12-01"), messages$delivered_at)   


messages$RRTM <- format((messages$notif_read_at_na - messages$notif_delivered_at_na) / 60, scientific = FALSE)
messages$RRTM <- as.numeric(messages$RRTM)

skim(messages)

#write.csv(messages, "messages_250K_with_DVs.csv")

ggplot(data = messages) + 
  geom_bar(mapping = aes(x = embed_attachment, fill = notif_read))

ggplot(data = messages) + 
  geom_bar(mapping = aes(x = attachments, fill = notif_read))

ggplot(data = messages) + 
  geom_bar(mapping = aes(x = content_type, fill = notif_read))

