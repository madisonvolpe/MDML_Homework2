library(data.table)
library(lubridate)
library(dplyr)
trump <- as.data.frame(fread("trump_data.tsv", header = FALSE))
names(trump) <- c("source", "time_posted", "text")

#Clean time 
trump$time_posted <- gsub("[^-0-9//:]", " ", trump$time_posted)
trump$time_posted <-trimws(trump$time_posted)
trump$time_posted <- ymd_hms(trump$time_posted)

#Feature 1: Hour of Day 
trump$hour <- hour(trump$time_posted)

#Feature 2: Start with Quotation Mark 
trump$quote <- ifelse(grepl('^"',trump$text), 1, 0) #if it begins with a quotation mark 1, if not then 0

#Filtering out non Trump or non staff language 
trump_filter <- trump%>%
  filter(quote==0)

#Feature 3: Contains an Https(proxy for link)
trump_filter$link <- ifelse(grepl("https",trump_filter$text), 1,0)

#Feature 4: Presence of a hastag 
trump_filter$hashtag <- ifelse(grepl("#", trump_filter$text),1,0)

#Feature 5: 


#Join trump to trump_filter
X <- left_join(trump,trump_filter)

#relevel NAs as 0 
