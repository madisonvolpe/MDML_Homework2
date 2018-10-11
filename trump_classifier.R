#Ekanta Nagi, William Spagnola, Madison Volpe 

#Loading necessary packages 

library(data.table)
library(lubridate)
library(dplyr)
library(tidytext) #may have to download this if you do not have it 
library(stringr)
library(naivebayes)
library(ggplot2)
library(readr)

#Read in Data 
trump <- as.data.frame(fread("trump_data.tsv", header = FALSE))
names(trump) <- c("source", "time_posted", "text")

#Clean time 
trump$time_posted <- gsub("[^-0-9//:]", " ", trump$time_posted)
trump$time_posted <-trimws(trump$time_posted)
trump$time_posted <- ymd_hms(trump$time_posted)

##### Part 1: Cleaning/ Features #####

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

  #Feature 5: Presence of an exclamation point at the end of the tweet 
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

    
#Feature 10: Presence of capitalized words (Check if 3 consecutive letters are capitalized)
    ##Omit if tweet contains link (because many links conta)
    ## Should be at least 4 to omit News organizations such as ABC, WSJ, etc. 
    trump_filter$capitalized <- ifelse(grepl("[A-Z]{4,}", trump_filter$text)== T &
                                         grepl("t.co",trump_filter$text) ==F, 1, 0)
     #Note sure how to exclude links with 4 consecutive letters 
    trump_filter[(grepl("[A-Z]{4,}", trump_filter$text)== T &
    grepl("t.co",trump_filter$text) ==F),] %>% select(source, text) %>% group_by(source) %>%
      summarize(n = n())
    
#Final dataset 
    
    #note - that trump_filter only examined tweets written by trump and his staff
    #we will code NAs as 0 - because they do not pertain to staff or trump - so
    #they do not have any of the features 

    trump <- left_join(trump,trump_filter)

    #relevel NAs as 0 
    
    #trump <- as.data.frame(apply(trump, 2, function(x){ ifelse(is.na(x)==T, 0, x)}))
    
    trump <-trump %>% mutate_all(function(x){ ifelse(is.na(x)==T, 0, x)})
    

    #check 
    sum(is.na(trump))

#clean work space#
    
    rm(nrc,Sentiments, SentimentsCount, tweet_words)
    
    

##### Part 2: Dividing Data into Training and Test Set ##### 
    
    set.seed(1234)
    
    # rename the label, make it a factor, and shuffle the data
    trump <- trump %>% mutate(label = as.factor(source)) %>% select(-source) %>% slice(sample(1:n())) #shuffle data
    
    #training dataset 
    split_size = .8*nrow(trump) #splits size  - we want it to be 80%
    train <- trump %>% slice(1:split_size) #first half 
    train_labels <- train$label
    train <- train %>% select(-label)#drops labels 
    
    #test dataset
    test <- trump %>% slice(split_size+1:n()) #splits size - we want it to be 20% 
    test_labels <- test$label
    test <- test %>% select(-label)

#### Part 3: Naive Bayes ####
#Function to Print Confusion Matrix and Calculate Accuracy 
    
check_accuracy <- function(predictions, labels, phase){
  confusion_matrix <- table(predictions, labels)
  print(confusion_matrix)
  accuracy <-  sum(diag(confusion_matrix )) / sum(confusion_matrix)
  precision <-  confusion_matrix[2,2] / sum(confusion_matrix[2 , ] )
  recall <-  confusion_matrix[2,2] / sum(confusion_matrix[ , 2] )
                                                                                      
  cat("Accuracy on", phase ,"Data: ", accuracy)
  cat("\n Precision on", phase ,"Data: ", precision)
  cat("\n Recall on", phase ,"Data: ", recall)
  
}
    
FinalModel <- naive_bayes(x=select(train,link,hashtag,hour,Positive,Negative), y =train_labels)

pred_train_2 <- predict(FinalModel, train)
check_accuracy(pred_train_2, train_labels, "Training")
#Test
pred_test_2 <- predict(FinalModel, test)
check_accuracy(pred_test_2, test_labels, "Test")
    

####Part4: Final Model Check #####

Test <- as.data.frame(fread("trump_hidden_test_set.tsv", header = FALSE))
names(Test) <- c("source", "time_posted", "text") 


######CLEAN TEST DATA WITH FEATURES WE WANT#####


#Clean time 
Test$time_posted <- gsub("[^-0-9//:]", " ", Test$time_posted)
Test$time_posted <-trimws(Test$time_posted)
Test$time_posted <- ymd_hms(Test$time_posted)

##### Part 1: Cleaning/ Features #####

#Feature 1: Hour of Day 
Test$hour <- hour(Test$time_posted)

#Important Note - if 1- means has feature, if 0 - means does not have feature! 

#Feature 2: Start with Quotation Mark 
Test$quote <- ifelse(grepl('^"',Test$text), 1, 0) #if it begins with a quotation mark 1, if not then 0

#Filtering out non Trump or non staff language 
Test_filter <- Test%>%
  filter(quote==0)

#Feature 3: Contains a Link/ Picture 
Test_filter$link <- ifelse(grepl("t.co",Test_filter$text), 1,0) #in the article he uses t.co 

#Feature 4: Presence of a hastag 
Test_filter$hashtag <- ifelse(grepl("#", Test_filter$text),1,0)


#Feature 6,7,8,9: Does the tweet contain negative emotion? Does the tweet contain positive emotion?
#Number of positive words in the tweet? Number of negative emotions in the tweet? 

#step one: We will tokenize each tweet - meaning that each word in a tweet will become its own row
#within this step we will take out links 
#also this code takes out stop_words - According to Stiles and Robinson in their book: Text
#Mining with R - stop_words are  stop words are words that are not useful for an analysis,
#typically extremely common words such as “the”, “of”, “to”, and so forth in English. 

reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))" #this reg ex. is taken from David Robinson's article

tweet_words <- Test_filter %>%
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
Test_filter$Positive <- ifelse(is.element(Test_filter$time_posted, PositiveIDs),1, 0)
Test_filter$Negative <- ifelse(is.element(Test_filter$time_posted, NegativeIDs),1, 0)


#Final dataset 

#note - that trump_filter only examined tweets written by trump and his staff
#we will code NAs as 0 - because they do not pertain to staff or trump - so
#they do not have any of the features 

Test <- left_join(Test,Test_filter)

####FIT MODEL ON TRAINING DATA######

FinalFitModel<- naive_bayes(x=select(trump,link,hashtag, hour, Positive, Negative), y =trump$label)

#### apply fitted model to generate predictions####

FinalPredictions <- predict(FinalFitModel, Test)
table(FinalPredictions, Test$source)

#### if else ####
FinalPredictionsLast <- ifelse(FinalPredictions == "Trump", 1, 0)
    
#### write the csv ####

write.csv(FinalPredictionsLast, "predictions.csv", row.names = FALSE)


