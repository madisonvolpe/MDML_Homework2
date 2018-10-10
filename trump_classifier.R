#Loading necessary packages 

library(data.table)
library(lubridate)
library(dplyr)
library(tidytext) #may have to download this if you do not have it 
library(stringr)
library(naivebayes)
library(ggplot2)

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

    
##### Part 4: Explore Data
    
 #Compare Number of Tweets by Hour
trump %>%
      group_by(hour, label) %>%
      summarize(number_of_tweets = n()) %>%
      ggplot() +
      geom_line(aes(x=hour, y=number_of_tweets, group = label, color = label))

#Compare Quotation Marks
(quote_summary <- trump %>%
      mutate(quote = ifelse(quote==1, "quote", "no quote")) %>%
      count(label, quote))
ggplot(quote_summary, aes(x=label, y = n, fill = quote)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(x = "", y = "Number of tweets", fill = "")

#Compare Capitalization
(capital_summary <- trump %>%
        mutate(all_caps = ifelse(capitalized==1, "All Caps", "No All Caps")) %>%
        count(label, all_caps))
ggplot(capital_summary, aes(x=label, y=n, fill = all_caps))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "")

#Compare Links
(links_summary <- trump %>%
    mutate(link = ifelse(link==1, "Links", "No Link")) %>%
    count(label, link))
ggplot(links_summary, aes(x=label, y=n, fill = link))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "")

#Compare Hashtag
(hashtag_summary <- trump %>%
    mutate(hashtag = ifelse(hashtag==1, "Hashtag", "No Hashtag")) %>%
      count(label, hashtag) )
ggplot(hashtag_summary, aes(x=label, y=n, fill = hashtag))+
         geom_bar(stat = "identity", position = "dodge") +
         labs(x = "", y = "Number of tweets", fill = "")


#Compare Exclamation Points 
(exclaim_summary <- trump %>%
    mutate(exclamation_point = ifelse(ExclamationEnd==1, "Exclamation Point", "No Exclamation Point")) %>%
    count(label, exclamation_point) )
ggplot(exclaim_summary, aes(x=label, y=n, fill = exclamation_point))+
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "")




#### Part 5: Naive Bayes ####
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

#Full Model
#Create model and make predictions on training data
nb_mod <- naive_bayes(x=select(train, -text, -time_posted ), y=train_labels)
#Training Data
pred_train_1 <- predict(nb_mod, train)
check_accuracy(pred_train_1, train_labels, "Training")
#Test
pred_test_1 <- predict(nb_mod, test)
check_accuracy(pred_test_1, test_labels, "Test")
#Accuracy on Test Data:  0.8629032


#Model 2: Drop All Caps Field 
no_caps <- select(train, -text, -time_posted, -capitalized )
nb_mod2 <- naive_bayes(x=no_caps, y=train_labels)
#Training Data
pred_train_2<- predict(nb_mod2, train)
check_accuracy(pred_train_2, train_labels, "Training")
#Test
pred_test_2 <- predict(nb_mod2, test)
check_accuracy(pred_test_2, test_labels, "Test")
#Accuracy on Test Data:  0.8629032


#Model 3: Drop Time 
no_time <- select(train, -text, -time_posted, -capitalized, -hour)
nb_mod3 <- naive_bayes(x=no_time, y=train_labels)
nb_mod3$tables
#Training Data
pred_train_3 <- predict(nb_mod3, train)
check_accuracy(pred_train_3, train_labels, "Training")
#Test
pred_test_3 <- predict(nb_mod3, test)
check_accuracy(pred_test_3, test_labels, "Test")
#Accuracy on Test Data:  0.8629032s

#Model 4: Drop  Links 
no_links <- select(train, -text, -time_posted, -capitalized, -hour, -link)
nb_mod4 <- naive_bayes(x=no_links, y=train_labels)
#Training Data
pred_train_4 <- predict(nb_mod4, train)
check_accuracy(pred_train_4, train_labels, "Training")
#Test
pred_test_4 <- predict(nb_mod4, test)
check_accuracy(pred_test_4, test_labels, "Test")
#Accuracy on Test Data:  0.8145161

#looks like we should definitely keep links
#I agree-Madison

#Model 5: Drop  Positive Emotions
no_positive <- select(train, -text, -time_posted, -capitalized, -hour,
                      -NumberPositiveEmotions, -Positive)
nb_mod5 <- naive_bayes(x=no_positive, y=train_labels )
#Training Data
pred_train_5 <- predict(nb_mod5, train)
check_accuracy(pred_train_5, train_labels, "Training")
#Test Data
pred_test_5 <- predict(nb_mod5, test)
check_accuracy(pred_test_5, test_labels, "Training")
#Accuracy on Training Data:  0.8629032

#Model 6: Drop  Negative Emotions
no_negative <- select(train, -text, -time_posted, -capitalized, -hour,
                      -NumberPositiveEmotions, -Positive, -Negative, - NumberNegativeEmotions )
nb_mod6 <- naive_bayes(x=no_negative, y=train_labels )
#Training Data
pred_train_6 <- predict(nb_mod6, train)
check_accuracy(pred_train_6, train_labels, "Training")
#Test Data
pred_test_6 <- predict(nb_mod6, test)
check_accuracy(pred_test_6, test_labels, "Test")
#Accuracy on Test Data:  0.8629032

#Model 7: Drop Exclamation Point
no_exclamation <- select(train, link, hashtag, quote)
nb_mod7 <- naive_bayes(x=no_exclamation, y=train_labels )
#Training Data
pred_train_7 <- predict(nb_mod7, train)
check_accuracy(pred_train_7, train_labels, "Training")
#Test Data
pred_test_7 <- predict(nb_mod7, test)
check_accuracy(pred_test_7, test_labels, "Test")
#Accuracy on Test Data:  0.8629032

#Model 8: Drop Hashtag
drop_hash <- select(train, quote,  link)
nb_mod8 <- naive_bayes(x=drop_hash, y=train_labels )
pred_train_8 <- predict(nb_mod8, train)
check_accuracy(pred_train_8, train_labels, "Training")
#Test Data
pred_test_8 <- predict(nb_mod8, test)
check_accuracy(pred_test_8, test_labels, "Test")
#Accuracy on Test Data:  0.8427419

#NOTE: It looks like we should keep hashtag


#Model 9: Drop Quote
drop_quote <- select(train, quote, hashtag,  link)
nb_mod9 <- naive_bayes(x=drop_quote, y=train_labels )
pred_train_9 <- predict(nb_mod9, train)
check_accuracy(pred_train_9, train_labels, "Training")
#Test Data
pred_test_9 <- predict(nb_mod9, test)
check_accuracy(pred_test_9, test_labels, "Test")
#Accuracy on Test Data:  0.8629032


#Model  With Link and Quote
nb_mod13 <- naive_bayes(x=select(train, link, quote), y=train_labels)
pred_train_13 <- predict(nb_mod13, train)
check_accuracy(pred_train_13, train_labels, "Training")
#Test Data
pred_test_13 <- predict(nb_mod13, test)
check_accuracy(pred_test_13, test_labels, "Test")
#Accuracy on Test Data:  0.8427419

#Model with Hashtag and Quote
nb_mod11 <- naive_bayes(x=select(train, hashtag,  quote), y=train_labels)
pred_train_11 <- predict(nb_mod11, train)
check_accuracy(pred_train_11, train_labels, "Training")
#Test Data
pred_test_11 <- predict(nb_mod11, test)
check_accuracy(pred_test_11, test_labels, "Test")
#Accuracy on Test Data:  0.7822581

#Best Models


#Best Models: 10 & 12

#Model 10: Link and Hashtag
nb_mod10 <- naive_bayes(x=select(train, link, hashtag), y=train_labels)
pred_train_10 <- predict(nb_mod10, train)
check_accuracy(pred_train_10, train_labels, "Training")
#Test Data
pred_test_10 <- predict(nb_mod10, test)
check_accuracy(pred_test_10, test_labels, "Test")
#Accuracy on Test Data:  0.8629032


#Model 12: Link, Quote, and Hashtag 
nb_mod12 <- naive_bayes(x=select(train, link, quote, hashtag), y=train_labels)
pred_train_12 <- predict(nb_mod12, train)
check_accuracy(pred_train_12, train_labels, "Training")
#Test Data
pred_test_12 <- predict(nb_mod12, test)
check_accuracy(pred_test_12, test_labels, "Test")
#Accuracy on Test Data:  0.8629032


