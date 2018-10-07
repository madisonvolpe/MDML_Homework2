#Loading necessary packages 
library(data.table)
library(lubridate)
library(dplyr)
library(tidytext) #may have to download this if you do not have it 
library(stringr)

#Read in Data 
trump <- as.data.frame(fread("trump_data.tsv", header = FALSE))
names(trump) <- c("source", "time_posted", "text")

#Clean time 
trump$time_posted <- gsub("[^-0-9//:]", " ", trump$time_posted)
trump$time_posted <-trimws(trump$time_posted)
trump$time_posted <- ymd_hms(trump$time_posted)

#Feature 1: Hour of Day 
trump$hour <- hour(trump$time_posted)

#Important Note - if 1- means has feature, if 0 - means does not have feature! 

#Feature 2: Start with Quotation Mark 
trump$quote <- ifelse(grepl('^"',trump$text), 1, 0) #if it begins with a quotation mark 1, if not then 0

#Filtering out non Trump or non staff language 
trump_filter <- trump%>%
  filter(quote==0)

#Feature 3: Contains a Link/ Picture 
trump_filter$link <- ifelse(grepl("t.co",trump_filter$text), 1,0) #in the article he uses t.co 

#Feature 4: Presence of a hastag 
trump_filter$hashtag <- ifelse(grepl("#", trump_filter$text),1,0)

#Feature 5: Pesence of an exclamation point at the end of the tweet 
trump_filter$ExclamationEnd <- ifelse(grepl("!$", trump_filter$text),1,0)

#Feature 6: Does this tweet contain words that are associated with negative emotions? 

  #step one: We will tokenize each tweet - meaning that each word in a tweet will become its own row
  #within this step we will take out links 
  #also this code takes out stop_words - According to Stiles and Robinson in their book: Text
  #Mining with R - stop_words are  stop words are words that are not useful for an analysis,
  #typically extremely common words such as “the”, “of”, “to”, and so forth in English. 

  reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))" #this reg ex. is taken from David Robinson's article
  
  tweet_words <- trump_filter %>%
    mutate(text = str_replace_all(text,  "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = reg) %>%
    filter(!word %in% stop_words$word,
           str_detect(word, "[a-z]")) %>%
    select(source, time_posted, word) #this code was adapted from David Robinson's article, as well 
  
  #Now we have all words - excluding stop words from each tweet, time_posted will serve as an ID 
  
  



#Join trump to trump_filter
X <- left_join(trump,trump_filter)

#relevel NAs as 0 


