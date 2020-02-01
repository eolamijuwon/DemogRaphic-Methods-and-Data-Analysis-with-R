
## Solutions

#### Exercise 1A.1


## Find the difference between 125 and its 3/5. 

    diff_quiz <- function (num = 125, x1 = 3/5){
      
      prod = x1 * num
      diff = num - prod
      
      print(paste0("The difference between ",
                   num, " and it's ",
                   x1, " is = ", diff)) 
    
    }

    diff_quiz(num = 125, x1 = 3/5)
    
## Using the same function, find the difference between 50 and it's 1/5.
    
    diff_quiz(num = 50, x1 = 1/5)



## Find a number which is greater than 17 as much as it is less than 31.
    
    quiz <- function (x1 = 17, x2 = 31){
      
      num <- (x1 + x2)/2
      print (
        paste0("A number that is greater than ", 
               x1, " as much as is greater than ",
               x2, " is: ", num))
      
    }
    
    quiz(x1 = 17, x2 = 31)

##  Using the same function, find the number 
##  which is greater than 16 as much as it is less than 50.
    
    quiz(16, 50)

    
    
    
## Solution 1A.2

## Install the `tidyverse` package
    install.packages("tidyverse")

## Load the `tidyverse` package
    library(tidyverse)
    
## Solution 1A.3
    
##  Using your background knowledge of how functions work, use the 
##  `read.dta13()` function from `readstata13` package to load the 
##  AGYW dataset (downloaded on your computer) and assign it to "agyw.dataset"
    install.packages("readstata13")
    library(readstata13)
    
    ## You have to change the directory to the file directory on your PC.
    agyw.dataset <- read.dta13("./2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/Nigeria [DHS].dta")
    
    ## Alternatively, you could import directly from GitHub
    agyw.dataset <- read.csv(text=RCurl::getURL("https://raw.githubusercontent.com/eolamijuwon/Workshops_Seminars/master/master/2020/Workshops/DemogRaphic%20Research%20and%20Data%20Analysis/Data%20-%20Misc/Nigeria%20%5BDHS%5D.dta"), header=T)
    

#### Exercise 1D.1

## Create a vector *region* with information on the number and percentage of AGYW in each region.
region_freq <- data.frame(table(agyw.clean$region)) %>% 
      mutate (perc = Freq/sum(Freq)) %>% 
      mutate (perc = perc * 100) %>% 
      mutate (perc = round(perc, digits = 2))
    
    
## Create a vector *residence* with information on the number and percentage of AGYW in each residence.
residence_freq <- data.frame(table(agyw.clean$residence)) %>% 
      mutate (perc = Freq/sum(Freq)) %>% 
      mutate (perc = perc * 100) %>% 
      mutate (perc = round(perc, digits = 2))

## Create a vector *religion* with information on the number and percentage of AGYW in the different religious denomination.
religion_freq <- data.frame(table(agyw.clean$religion)) %>% 
      mutate (perc = Freq/sum(Freq)) %>% 
      mutate (perc = perc * 100) %>% 
      mutate (perc = round(perc, digits = 2))

    
    