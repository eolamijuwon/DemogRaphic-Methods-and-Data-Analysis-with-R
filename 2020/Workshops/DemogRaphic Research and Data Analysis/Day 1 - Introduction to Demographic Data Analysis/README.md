---
Title: "DemogRaphic Research and Data Analysis"

Author: "Emmanuel Olamijuwon"

Host Institution: "Federal University, Oye Ekiti, Ekiti State, Nigeria"

Date: "10-14 February 2020"
---


# Introduction



For the session, you will be required to have R and Rstudio installed on your computer. If you are still to download/install R [[click here](https://cloud.r-project.org/)] or [[here](https://rstudio.com/products/rstudio/download/)] to download RStudio


## The R syntax

The R synthax would usually comprise of:
  - a function
  - '<-' the assignment operator
  - = for arguments
  - '# for comments # and how they are used to document function and its content
  - $ the operator

You can get an output from R simply by typing in math in the console.

```{r}

## How far are we from vision 2022 (in years)

2022 - 2019

3 + 5

## Quiz - 
 
# Pick a number

```


## Creating Objects & Function

Instead of just typing, we could assign the value[answer] to an *object/variable*

### Practice Exercise I

```{r}

##  Add 3 and 5. Assign the value to a


##  Subtract 2019 from 2022. Assign the value to b


##  Find the square root of 49. Assign the value to c
##  Note:: The R function to find square root is `sqrt`


##  Find the value of (a+c)/b. Assign the value to d


##  print the value of d

print(d)

##  or

d


```

Note that we used a function *`sqrt`* to find the square root of 49 in the practice exercise. Functions are built in capabilities of R (base R) or could be gotten from libraries (see ##packages) or you could write yours. Exectuting a function is reffered to as *calling* the function. Most functions can take several arguments (details can be found on the package website).


## Packages

A package is a collection of R functions, complied code and sample data. Examples of R packages can be found on the RStudio [[website](https://rstudio.com/products/rpackages/)].

Some common R packages are
  
  -  *ggplot2*
  -  *dplyr*
  -  *tidyr*
  -  *readr*
  
  These packages are also embedded in the Tidyverse package and a detailed description of can be found on the package  [[website](https://www.tidyverse.org/packages/)].

  
To install/use a package

  - Step 1: Install the package install.packages(`package.name`)
  
      > or devtools::install_github("repository/package.name")
  
  -  Step 2: Load the package    library(`package.name`)
  
  Note, that we are calling two functions above *install.packages* and *library*
  
```{r}

  install.packages("dplyr")

  install.packages("readr")
  
  ##  or install.packages("readr", "dplyr")
  
  library(dplyr)
  
  library(readr)

```


#  Load Data

We can load data into R from various data format including ".csv", ".txt", ".dta", ".sav", ".rjson" among others.

For this activity, we will use the [[teenageData](https://github.com/eolamijuwon/EswatiniUser/blob/master/Intro%20to%20R/teenageData.csv/)] which is a subset data extracted from the 2014 Multiple Indicator Cluster Survey of Eswatini. The sample data contains basic demographic features of teenagers [15-19 years] who were in Eswatini at the time of data collection. The MICS full dataset can be downloaded on the [[UNICE website](https://mics.unicef.org/surveys/)].

- Activity: Import the teenageData into RStudio and assign it to ::TeenData::

```{r}

# Please change all directory names accordingly


TeenData <- read.csv(".\\teenageData.csv")

#   Note that <- is an assignment function (just as = in STATA)

#   You could also read the data into RStudio using the read_csv function in readr package

TeenDatR <- read_csv ("C:\\Users\\eOlamijuwon\\OneDrive\\Research\\Computational Social Science\\Eswatini UseR\\Intro to R\\teenageData.csv")


# since the data is also available online, we could also read it directly from github

```


# Data Exploration

```{r}

  # You can browse the dataset with "View"
  View(TeenData)
  
  # You can check the structure of your dataset
  str(TeenData)
  
  # Alternatively you can use the "glimpse" function from the dplyr package to view the data structure
  
  glimpse(TeenData)
  
  # You can examine the first few observations in the dataset (could be 5/10/more) using the `head` function
  
  head(TeenData, n=30) 
  
  # You can check how many rows(number of observations) are in your dataset using the `nrow` function 
  
  nrow(TeenData)
  
  # You can check how many rows(number of variables) are in your dataset using the `nrow` function 
  ncol(TeenData)

```


# Activity 1

Clean Data - We want study teenage pregnancy in Eswatini. That is, our sample should be teenagers

From the old `TeenData` data, create a new dataset ::TeenPreg:: focusing on:

  - Teenagers that have never given birth (CM1)
  
  - Create a var `ever_had_sex` from -Age at first sex (SB1)
  
    Note: Variable names cannot have spaces
    
  - Create a var `education` [<Sec/Sec+] from Highest educational attain (welevel).
  
  - Create a var `violence_atti [No support/Support Violence]` from DV1A-DV1I
  
  - Drop all other variables and keep [ever_had_sex, education, violence_atti]
  
  We will use the dplyr package [filter, select, mutate]


## Solution 1

```{r}

  #     - Clean Data - We want study teenage pregnancy in Eswatini
  #       That is, our sample should be teenagers
  #       From the old `TeenData` data, create a new dataset ::TeenPreg:: focusing on:
                TeenPreg <- TeenData %>% 
  
  #       - Teenagers that have never given birth (CM1)
                      filter(CM1 == "No") %>% 
  
  #       - Create a var `ever_had_sex` from -Age at first sex (SB1)
                      mutate(ever_had_sex = as.numeric(SB1),
                             ever_had_sex = ifelse((ever_had_sex < 11), 
                                                   "Had Sex", "Never Had Sex")) %>% 
  #         Note: Variable names cannot have spaces
  
  #       - Create a var `education` [<Sec/Sec+] from Highest educational attain (welevel).
                      mutate(education = ifelse((welevel == "None" | welevel == "Primary"),
                                                "< Secondary", "Secondary+")) %>% 
  
  #       - Create a var `violence_atti [No support/Support Violence]` from DV1A-DV1I
    
                      mutate(violence_atti = ifelse((DV1A =="Yes" |
                                                      DV1B =="Yes" | DV1C =="Yes" | 
                                                      DV1D =="Yes" | DV1E =="Yes" | 
                                                      DV1F =="Yes" | DV1G =="Yes" |
                                                      DV1H =="Yes" | DV1I =="Yes"),
                                                    "No support", "Support Violence")) %>% 
                      
  #       - Drop all other variables and keep [ever_had_sex, education, violence_atti]
                      select("ever_had_sex", "education", "violence_atti")

```

Note that in the above example we used the `filter`, `mutate`, and `select` functions from the `dplyr` package.


You could also take the analysis further by presenting the descriptive statistics for all elements in the data using the `summaryTools` package

```{r echo=TRUE}
install.packages("compareGroups")
library(compareGroups)
descrTable(TeenPreg)
```


## Practice Exercise II
  
      - How many teenagers are in the `TeenPreg` dataset
      
      - How many teenagers in the `TeenPreg` dataset have had sex and support IPV
      
      - How many teenagers in the `TeenPreg` dataset support IPV and have Sec+ education
      
        NOTE: You can use the `table` function to tabulate
  
  
# Class Activity

  Using the TeenData, find:
  
      - The number of teenagers who have a comprehensive knowledge of HIV
      
      - The number of teenagers who have a positive attitude to the PLHIH
      
      - Among those who have had sex, what is the mean years since last sex (Age - Age1sex).
  
  
  
NOTE:: Solutions to Practice exercise:

  - [[Practice Exercise I](https://github.com/eolamijuwon/EswatiniUser/blob/master/Intro%20to%20R/Solutions/Practice%20Exercise%20I.R/)]
  
  - [[Practice Exercise II](https://github.com/eolamijuwon/EswatiniUser/blob/master/Intro%20to%20R/Solutions/Practice%20Exercise%20II.R)]
  
  - [[Class Activity](https://github.com/eolamijuwon/EswatiniUser/blob/master/Intro%20to%20R/Solutions/Practice%20Exercise%20II.R)]
  


