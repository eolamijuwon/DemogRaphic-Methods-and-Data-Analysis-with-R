
## Solutions

#### Exercise 2A.1
##  Estimate the prevalence of modern contraceptive use by:
        ##  Region of Residence, 
        
        chi2q_region <- svychisq(~mCuse + region, dhs_design)$p.value
        chi2q_region <- format(if_else(  (chi2q_region<0.0001), 
                                            0.0001, 
                                         chi2q_region, 
                                            missing = 0), 
                                  scientific = FALSE)
        
        rowBiv_region <- svyby( ~ mCuse, ~ region,  dhs_design, svymean) %>% 
          dplyr::rename( "perc_Using" = "mCuseUsing Modern Contraceptives",
                         "perc_notUsing" = "mCuseNot Using Modern Contraceptives") %>% 
          mutate (freq = (agyw_clean %>% count(region))$n) %>% 
          dplyr::select("region", "freq", "perc_Using") %>% 
          mutate (perc_Using = round((perc_Using * 100), digits = 2)) %>% 
          mutate (perc_Using = paste0(perc_Using, "%")) %>% 
          rename (characteristics = region)
        
        rownames(rowBiv_region) <- NULL
        
        
        ##  place of residence, and 
        
        chi2q_residence <- svychisq(~mCuse + residence, dhs_design)$p.value
        chi2q_residence <- format(if_else(  (chi2q_residence<0.0001), 
                                         0.0001, 
                                         chi2q_residence, 
                                         missing = 0), 
                               scientific = FALSE)
        
        rowBiv_residence <- svyby( ~ mCuse, ~ residence,  dhs_design, svymean) %>% 
          dplyr::rename( "perc_Using" = "mCuseUsing Modern Contraceptives",
                         "perc_notUsing" = "mCuseNot Using Modern Contraceptives") %>% 
          mutate (freq = (agyw_clean %>% count(residence))$n) %>% 
          dplyr::select("residence", "freq", "perc_Using") %>% 
          mutate (perc_Using = round((perc_Using * 100), digits = 2)) %>% 
          mutate (perc_Using = paste0(perc_Using, "%"))  %>% 
          rename (characteristics = residence)
        
        rownames(rowBiv_residence) <- NULL
        
        
        ##  Religious affiliation
        
        chi2q_religion <- svychisq(~mCuse + religion, dhs_design)$p.value
        chi2q_religion <- format(if_else(  (chi2q_religion<0.0001), 
                                            0.0001, 
                                            chi2q_religion, 
                                            missing = 0), 
                                  scientific = FALSE)
        
        rowBiv_religion <- svyby( ~ mCuse, ~ religion,  dhs_design, svymean) %>% 
          dplyr::rename( "perc_Using" = "mCuseUsing Modern Contraceptives",
                         "perc_notUsing" = "mCuseNot Using Modern Contraceptives") %>% 
          mutate (freq = (agyw_clean %>% count(religion))$n) %>% 
          dplyr::select("religion", "freq", "perc_Using") %>% 
          mutate (perc_Using = round((perc_Using * 100), digits = 2)) %>% 
          mutate (perc_Using = paste0(perc_Using, "%"))  %>% 
          rename (characteristics = religion)
        
        rownames(rowBiv_religion) <- NULL



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
    agyw_dataset <- read.dta13("./2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/Nigeria [DHS].dta")
    
    ## Alternatively, you could import directly from GitHub
    agyw_dataset <- read.csv(text=RCurl::getURL("https://raw.githubusercontent.com/eolamijuwon/Workshops_Seminars/master/master/2020/Workshops/DemogRaphic%20Research%20and%20Data%20Analysis/Data%20-%20Misc/Nigeria%20%5BDHS%5D.dta"), header=T)
    

#### Exercise 1D.1

## Create a vector *region* with information on the number and percentage of AGYW in each region.
region_freq <- data.frame(table(agyw_clean$region)) %>% 
      mutate (perc = Freq/sum(Freq)) %>% 
      mutate (perc = perc * 100) %>% 
      mutate (perc = round(perc, digits = 2))
    
    
## Create a vector *residence* with information on the number and percentage of AGYW in each residence.
residence_freq <- data.frame(table(agyw_clean$residence)) %>% 
      mutate (perc = Freq/sum(Freq)) %>% 
      mutate (perc = perc * 100) %>% 
      mutate (perc = round(perc, digits = 2))

## Create a vector *religion* with information on the number and percentage of AGYW in the different religious denomination.
religion_freq <- data.frame(table(agyw_clean$religion)) %>% 
      mutate (perc = Freq/sum(Freq)) %>% 
      mutate (perc = perc * 100) %>% 
      mutate (perc = round(perc, digits = 2))

    
    