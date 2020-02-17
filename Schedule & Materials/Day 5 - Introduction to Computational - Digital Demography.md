---
Title: "Introduction to Computational and Digital Demography"

Date: "14 February 2020"
---




## Introduction to Computational and Digital Demography


**Computational:** methods or approaches
**Digital:** alternative sources of data ranging from social media data to web search logs
**Demography:** the scientific study of human populations and, at its most basic, focuses on the processes of (i) fertility, (ii) mortality and (iii) mobility

We could define computational and digital demography as the application of mathematical/statistical methods or approaches to alternative sources of data (such as social media and others) in the study of human populations.

Below are some impressive examples of studies in the field of computational and digital demography.

### Fertility

- Francesco Billari, Osea Giuntella, and Luca Stella. (2019) Does broadband Internet affect fertility?, Population Studies, 73:3, 297-316, DOI: 10.1080/00324728.2019.1584327

- Rampazzo Francesco, Emilio Zagheni, Ingmar Weber, Maria Rita Testa, and Francesco Billari. (2018). "Mater certa est, pater numquam: What can Facebook Advertising Data Tell Us about Male Fertility Rates?." arXiv preprint arXiv:1804.04632


### Migration

- Zagheni, E., Weber, I. and Gummadi, K. (2017), Leveraging Facebook's Advertising Platform to Monitor Stocks of Migrants. Population and Development Review, 43: 721-734. doi:10.1111/padr.12102

- Quantifying international human mobility patterns using Facebook Network data Spyratos S, Vespe M, Natale F, Weber I, Zagheni E, et al. (2019) Quantifying international human mobility patterns using Facebook Network data. PLOS ONE 14(10): e0224134. https://doi.org/10.1371/journal.pone.0224134

- Alexander, M., Polimis, K. and Zagheni, E. (2019), The Impact of Hurricane Maria on Out‐migration from Puerto Rico: Evidence from Facebook Data. Population and Development Review, 45:617-630. doi:10.1111/padr.12289



### Other Population Processes

- Fatehkia, Masoomali, Ridhi Kashyap, and Ingmar Weber. (2018), "Using Facebook ad data to track the global digital gender gap." World Development 107: 189-209. doi: https://doi.org/10.1016/j.worlddev.2018.03.007

- Khare Sangita, S. Kavyashree, Deepa Gupta, and Amalendu Jyotishi. (2017) "Investigation of nutritional status of children based on machine learning techniques using Indian demographic and health survey data." Procedia computer science 115: 338-349.


From the above examples, you can observe that there has been a wide range of opportunities to answer new and exciting demographic research questions using data from digital platforms. In todays session, we use the `rtweet` package to stream online conversations about the *US immigration visa ban*. We will explore the sentiments of the tweets as well as the top 10 trigrams. If you are more keen about using quantitative data from the Facebook advertising platform, Sofia Gill has an impressive and easy to follow tutorial that can be accessed [here](https://github.com/SofiaG1l/Using_Facebook_API)


## Twitter Data Retreival

As with other data analysis, remeber to install the `rtweet` package and all the relevant libraries. In addition to some of the libraries that we have used in the previous sessions, we will also use some functions from the `tidytext` package


```{r}

#install.packages("rtweet")

library (rtweet)

library (tidyverse)

library (mosaic)

library (tidytext)

```

In order to be able to stream tweets directly from Twitter, you need to have a Twitter account created. Please note `rtweet` should be used strictly in accordance with Twitter’s [developer terms](https://developer.twitter.com/en/developer-terms/more-on-restricted-use-cases.

In the next few lines of code, we will create an object `keywords` with all possible keywords that could capture all possible tweets related to the immigration ban. The \" and \" have been used together with the keyword to ensure that tweets contain exact matches of the keywords.


```{r}

keywords <- c("\"visa ban\"", "ban",
              "\"from the US\"",
              "\"in the US\"",
              "\"immigrant visa\"",
              "\"visa restriction\"")
            
            ## The geocode argument is specified to retain only tweets from Nigeria
            ## The retryonratelimit argument instructs R to retry the downloading of tweets once the rate limit has been reached
visaBan_NG <- Map("search_tweets", n=10000, keywords,
              geocode = "9.06048,7.48324,400mi",
              include_rts = FALSE, retryonratelimit = TRUE)

                    ## Convert your downloaded tweets to data.frame()
DF_visaBan_NG <-  do_call_rbind(visaBan_NG)


## Save the downloaded tweets as .csv on your local computer using the `save.csv` function

save_as_csv(DF_visaBan_NG, "visaBan_NG.csv",
            prepend_ids = TRUE, na = "NA")

```

Remember that on Day 1, we covered how to import and export data into R.
In the following sections, we will import the dataset from the local computer and do some data cleaning/manipulation. This will include, filtering observations `filter`, selecting relevant variables `select`, checking variables structure, to be sure they are consistent and as desired and recreating new variables `mutate`.


```{r}

DF_visaBan_NG <- read.csv("visaBan_NG.csv")

ImmigrantBan_NG <- DF_visaBan_NG %>% 
                   ## Keep only Tweets without links
                   ## We are doing this to eliminate tweets that we assume might be from New agencies/bloggers
                   ## Perhaps we are interested in Nigerian's response to the ban
                   filter (is.na(urls_url) == TRUE) %>% 
                   
                   ## Keep only the relevant variables
                   select(user_id, account_created_at, 
                          followers_count, friends_count,
                          verified, location, status_id,
                          text, created_at, reply_to_user_id, 
                          retweet_count, favorite_count, source) %>% 
                   ## Check the structure of "account_created_at" and "created_at"
                   ## You will observe that the two variables are characters.
                   ## We will format and convert them to dates using the as.Date function
                   mutate(account_created_at = as.Date(account_created_at, format = "%Y-%m-%d %H:%M:%S"),
                          created_at = as.Date(created_at, format = "%Y-%m-%d %H:%M:%S")) %>% 
                          
                   ## Perhaps we are also interested in the different types of devices that Twitter users use
                   ## This could perhaps give us some background information about
                   ## "socio-economic status" of those who are tweeting
                   
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
                                                    
                   ## Finally, create a variable to identify Tweets from "Celebrities" or verified users
                   mutate(verified = derivedFactor("Verified" = verified ==(TRUE),
                                                   "Not Verified" = verified ==(FALSE),
                                                    .default = NA))

```

#### Exercise 5A.1

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

## What Next

**The Summer Institute for Computational Social Science**

The Summer Institute for Computational Social Science [SICSS] is currently accepting applications (until March 20) for this years summer school. The institute brings together graduate students, postdoctoral researchers, and junior faculty for 2 weeks of intensive study and interdisciplinary research. This year there are more than 20 partner locations. Feel free to review the application requirements and apply to attend the location nearest to you. **[Apply Now](https://compsocialscience.github.io/summer-institute/2020/stellenbosch/)**


**The Barcelona Summer School of Demography**

The Barcelona Summer School of Demography [BSSD] is also accepting applications (until March 31) for participation at this years summer school. The BSSD is based at the Centre for Demographic Studies (CED), Universitat Autònoma de Barcelona, and offers a four-week intensive course in R covering three major strengths of R: statistical and demographic analysis, data visualization, and spatial analysis. Participants are free to apply for the entire course or any of the individual modules. **[Apply Now](https://ced.uab.cat/en/courses/barcelona-summer-school-of-demography/)**


## Supplemental Readings

- Alburez-Gutierrez, D., Aref, S., Gil-Clavel, B. S., Grow, A., Negraia, D. V., Zagheni, E. In: Arbia, G., Peluso, S., Pini, A., Rivellini, G. (Eds.): Smart statistics for smart applications : book of short papers SIS2019, 23–30 Pearson (2019) https://osf.io/preprints/socarxiv/24jp7/

- Cesare, N., Lee, H., McCormick, T., Spiro, E., & Zagheni, E. (2018). Promises and pitfalls of using digital traces for demographic research.Demography,55(5), 1979-1999.

- Pham, Katherine Hoffmann, Francesco Rampazzo, and Leah R. Rosenzweig. "Online Surveys and Digital Demography in the Developing World: Facebook Users in Kenya." arXiv preprint arXiv:1910.03448 (2019).

- Salganik, Matthew J. (2017). Bit by Bit: Social Research in the Digital Age. Princeton, NJ: Princeton University Press. Open review edition. https://www.bitbybitbook.com/en/1st-ed/preface/


