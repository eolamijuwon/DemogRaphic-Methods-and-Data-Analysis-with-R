---
Title: "Introduction to Computational and Digital Demography"

Date: "14 February 2020"
---


```{r}

#install.packages("rtweet")

library (rtweet)

library (tidyverse)

library (mosaic)

library (tidytext)

```

```{r}

consumer_key <- "xxxconsumerkeyxxx"
consumer_secret <- "xxxconsumerSecretxxxx"
app_name <- "ComputationalDemOe"
access_token <- "xxxxaccess-tokenxxxx"
access_secret <- "xxxxxaccess-secretxxxxx"

create_token(app=app_name, consumer_key=consumer_key,
             consumer_secret=consumer_secret,
             access_token = access_token, access_secret = access_secret)

```

```{r}

keywords <- c("\"visa ban\"", "ban",
              "\"from the US\"",
              "\"in the US\"",
              "\"immigrant visa\"",
              "\"visa restriction\"")

NvisaBan_NG <- Map("search_tweets", n=10000, keywords,
              geocode = "9.06048,7.48324,400mi",
              include_rts = FALSE, retryonratelimit = TRUE)

NDF_visaBan_NG <-  do_call_rbind(NvisaBan_NG)

save_as_csv(DF_visaBan_NG, "./2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/visaBan_NG.csv",
            prepend_ids = TRUE, na = "NA")

```


```{r}

DF_visaBan_NG <- read.csv("./2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/visaBan_NG.csv")

ImmigrantBan_NG <- DF_visaBan_NG %>% 
                   filter (is.na(urls_url) == TRUE) %>% 
                   select(user_id, account_created_at, 
                          followers_count, friends_count,
                          verified, location, status_id,
                          text, created_at, reply_to_user_id, 
                          retweet_count, favorite_count, source) %>% 
                   mutate(account_created_at = as.Date(account_created_at, format = "%Y-%m-%d %H:%M:%S"),
                          created_at = as.Date(created_at, format = "%Y-%m-%d %H:%M:%S")) %>% 
                   mutate(deviceType = derivedFactor("Android" = (source == "Plume for Android" |
                                                           source == "Twidere for Android (Deprecated)" | 
                                                           source == "Twitter for Android" | 
                                                           source == "UberSocial for Android"),
                                                    "iOS" = (source == "Tweetbot for iΟS" |
                                                             source == "Twitter for iPad" |
                                                             source == "Twitter for iPhone" |
                                                             source == "Tweetbot for iΟS"),
                                                    "PC/Mac" = (source == "Twitter for Mac" |
                                                                source == "Twitter Web App" |
                                                                source == "Twitter Web Client"),
                                                    "Unknown" = (source == "Buffer" | source == "Cheap Bots, Done Quick!" |
                                                                source == "Crowdfire App" | source == "dlvr.it" |
                                                                source == "Echobox Social" | source == "Hootsuite Inc." | 
                                                                source == "IFTTT" | source == "Mobile Web (M2)" |
                                                                source == "Sprout Social" | source == "Storm_It" |
                                                                source == "TrafficChiefNG" | source == "TweetDeck"),
                                                    .default = NA)) %>% 
                   mutate(verified = derivedFactor("Verified" = verified ==(TRUE),
                                                   "Not Verified" = verified ==(FALSE),
                                                    .default = NA))

```

#### Exercise 2A.1

Using the information available in the `ImmigrantBan_NG` dataset. Find the:

- Percentage distribution of devices from which tweets were sent.

- Average number of Retweets/Favourites 

- What percentage of the tweets were posted by verified users

*Hint:*  Can the exercise be addressed using any existing package? `stargazer`, `kableExtra`, `compareGroups`



```{r}

tidy_ImmigrantBan_NG <- ImmigrantBan_NG %>%
                        select(created_at, text, 
                              favorite_count, 
                              retweet_count) %>%
                        mutate (text = as.character(text)) %>% 
                        mutate(text = gsub(text, pattern = "@\\w+", replacement = "")) %>% 
                        
                        # removes tweets with the word "okada" OR "keke" since it's not related to the immigrant visa ban
                        filter (!str_detect(text, pattern = "okada"),
                                !str_detect(text, pattern = "Okada"),
                                !str_detect(text, pattern = "OKADA"),
                                !str_detect(text, pattern = "keke"),
                                !str_detect(text, pattern = "Keke"),
                                !str_detect(text, pattern = "KEKE"))
                                
                                
word_ImmigrantBan_NG <- tidy_ImmigrantBan_NG %>% 
                        unnest_tokens(bigram, text, token = "ngrams", n = 3) %>% 
                        separate(bigram, c("word1", "word2", "word3"), sep = " ") %>%  
                        
                        filter(!word1 %in% stop_words$word,       # remove stopwords from both words in bi-gram
                               !word2 %in% stop_words$word,       # remove stopwords from both words in bi-gram
                               !word3 %in% stop_words$word) %>% 
                               
                        filter(!str_detect(word1, pattern = "[[:digit:]]"), # removes any words with numeric digits
                               !str_detect(word2, pattern = "[[:digit:]]"),
                               !str_detect(word3, pattern = "[[:digit:]]"),
                               
                               !str_detect(word1, pattern = "[[:punct:]]"), # removes any remaining punctuations
                               !str_detect(word2, pattern = "[[:punct:]]"),
                               !str_detect(word3, pattern = "[[:punct:]]"),
                               
                               !str_detect(word1, pattern = "(.)\\1{2,}"),  # removes any words with 3 or more repeated letters
                               !str_detect(word2, pattern = "(.)\\1{2,}"),
                               !str_detect(word3, pattern = "(.)\\1{2,}"),
                               
                               !str_detect(word1, pattern = "\\b(.)\\b")) %>%  # removes any remaining single letter words
                               # !str_detect(word2, pattern = "\\b(.)\\b")
                               # !str_detect(word3, pattern = "\\b(.)\\b")) 
                               
                       unite("bigram", c(word1, word2, word3), sep = " ") %>%
                       count(bigram, sort = TRUE)

```





```{r}

text_immigration <-  tidy_ImmigrantBan_NG %>%
                     select(created_at, text, 
                            favorite_count, 
                            retweet_count) %>%
                     unnest_tokens("word", text) %>%
                     anti_join(stop_words, by = "word") %>%
                     inner_join(y = sentiments, by = "word") %>% 
                     count(created_at, sentiment) %>% 
                     group_by(created_at) %>% 
                     mutate (perc_nega = (n/sum(n))*100) %>% 
                     mutate (perc_nega = round (perc_nega, digits = 2),
                             sentiment = as.factor (sentiment))

```

```{r}

library(ggthemes)

    text_immigration %>% 
    ggplot (aes(x = created_at, y = perc_nega,
                fill = factor(sentiment, 
                        labels = c("Negative",
                                    "Positive")))) +
    geom_bar (stat = "identity") + 
    theme_fivethirtyeight () +
    labs (title = "Sentiment of Tweets Related to US Immigration Ban") +
    guides(fill=guide_legend(title="Sentiment of Tweets")) +
    scale_fill_manual(values = c("#ffb612", "#000000"))


```