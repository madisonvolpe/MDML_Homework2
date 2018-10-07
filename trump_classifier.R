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

##### Part 1: Cleaning/ Features #######

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

#Feature 6,7,8,9: Does the tweet contain negative emotion? Does the tweet contain positive emotion?
#Number of positive words in the tweet? Number of negative emotions in the tweet? 

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
  
  #step two: sentiment analysis- we will use the NRC Word Emotion Association Lexicon to see,
  #which individual word in each tweet is as an emotion. The NRC is available in the tidy text package
  #filter on 10 sentiments: positive, negative, anger, anticipation, disgust, fear, joy, sadness, suprise
  # and trust. 
  
    nrc <- sentiments %>% #load in nrc words and sentiments 
      filter(lexicon == "nrc") %>%
      dplyr::select(word, sentiment) %>%
      filter(sentiment %in% c("trust", "positive", "joy", "fear", "negative", "sadness", "anger", "disgust", "anticipation", 
                              "surprise"))

  #step three: join nrc and tweet_words 
    
    Sentiments <- inner_join(tweet_words, nrc, by = "word") 
    
  # 
    positive_sent <- c("trust", "positive", "joy", "anticipation", "surprise")
    negative_sent <- c("fear", "negative", "sadness", "anger", "disgust")
    
    Sentiments$Positive <- ifelse(is.element(Sentiments$sentiment, positive_sent), 1, 0)
    Sentiments$Negative <- ifelse(is.element(Sentiments$sentiment, negative_sent), 1, 0)

  #IDs with positive emotions 
    PositiveIDs <- unique(Sentiments[Sentiments$Positive==1,2])
  #IDs with negative emotions 
    NegativeIDs <- unique(Sentiments[Sentiments$Negative==1,2])

  #Assigning Positive and Negative Emotions per Tweet 
    trump_filter$Positive <- ifelse(is.element(trump_filter$time_posted, PositiveIDs),1, 0)
    trump_filter$Negative <- ifelse(is.element(trump_filter$time_posted, NegativeIDs),1, 0)
  
  #Counts 
    SentimentsCount <- Sentiments %>%
    group_by(source, time_posted) %>%
    summarise(NumberPositiveEmotions = sum(Positive), NumberNegativeEmotions = sum(Negative))
    
  #Assigning Counts of Positive and Negative Emotions per Tweet 
    trump_filter <- full_join(trump_filter, SentimentsCount)
    
  #For counts, if 0 - then no emotion associated so we can make NAs 0 
    trump_filter<- trump_filter %>%
      mutate(NumberPositiveEmotions = ifelse(is.na(NumberPositiveEmotions),0, NumberPositiveEmotions),
             NumberNegativeEmotions = ifelse(is.na(NumberNegativeEmotions),0, NumberNegativeEmotions))
    
    trump_filter %>%
      group_by(source)%>%
      summarise(PositiveEmotions = sum(NumberPositiveEmotions), NegativeEmotions = sum(NumberNegativeEmotions), 
                PercentNeg = NegativeEmotions/sum(NegativeEmotions+PositiveEmotions),
                PercentPos = PositiveEmotions/sum(PositiveEmotions+NegativeEmotions))

#Final dataset 
    
    #note - that trump_filter only examined tweets written by trump and his staff
    #we will code NAs as 0 - because they do not pertain to staff or trump - so
    #they do not have any of the features 

    trump <- left_join(trump,trump_filter)

    #relevel NAs as 0 
    
    trump <- data.frame(apply(trump, 2, function(x){ ifelse(is.na(x), 0, x)}))
    
    #check 
    
    sum(is.na(trump))
    


