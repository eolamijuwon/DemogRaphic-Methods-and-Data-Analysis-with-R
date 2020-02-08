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

keywords <- c("\"visa ban\"", "\"ban\"",
              "\"from the US\"",
              "\"in the US\"",
              "\"immigrant visa\"",
              "\"visa restriction\"")

visaBan_NG <- Map("search_tweets", n=10000, keywords,
              geocode = "9.06048,7.48324,400mi",
              include_rts = FALSE, retryonratelimit = TRUE)

DF_visaBan_NG <-  do_call_rbind(visaBan_NG)

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

word_ImmigrantBan_NG <- ImmigrantBan_NG %>%
                        select(text, created_at, user_id, 
                               favorite_count, retweet_count) %>% 
                        mutate (text = as.character(text)) %>% 
                        unnest_tokens("word", text) %>% 
                        removeNumbers(word_ImmigrantBan_NG$word)
                        
word <- removeNumbers(word)


words_politicalZA$word <- 
    removePunctuation(words_politicalZA$word)


head(word_ImmigrantBan_NG, n=15)



```