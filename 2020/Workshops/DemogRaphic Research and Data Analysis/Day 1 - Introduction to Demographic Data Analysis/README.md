---
Title: "Introduction to Data Analysis with R"

Date: "10 February 2020"
---



## Introduction to R and RStudio


### R syntax and Basic operations: assigning

The R synthax would usually comprise of:

  - a function
  
  - '<-' the assignment operator
  
  - = for arguments
  
  - '# for comments/description/documentation
  
  - $ the operator

You can get an output from R simply by typing in math in the console.

```{r}

## How old are you (in years)

YoB <- 1960

present_year <- 2020

age <- present_year - YoB

print (age)

```


Alternatively,

```{r}

## How old are you (in years)

age <- 2020 - 1960

print (age)

## OR

2020 - 1960

```

Note that we used a function *`print`* in the above examples. Functions are built in capabilities of R (base R) or could be gotten from libraries (see ##packages) or you could write one. Executing a function is reffered to as *calling* the function. Most functions can take several arguments (details can be found on the package website).

### R Functions


#### Exercise 1A.1

Write a function to:

  - Find the difference between 125 and its 3/5. Using the same function, find the difference between 50 and it's 1/5.
  
  - Find a number which is greater than 17 as much as it is less than 31. Using the same function, find the number which is greater than 16 as much as it is less than 50.


### R packages

R packages are a collection of R *functions*, *complied code* and *sample data*. They are stored under a directory called "library" in the R environment. By default, R installs a set of packages during installation. You can view already installed packages using the `installed.packages()` function. More packages can be installed/added after using `install.packages("package.name")` or `devtools::install_github("repository/package.name")`. 


Some very useful R packages in applied demography includes [`DemoDecomp`](https://github.com/timriffe/DemoDecomp/), [`demography`](https://github.com/robjhyndman/demography), [`demogR`](https://www.jstatsoft.org/article/view/v022i10), among many others. Examples of R packages can be found on the RStudio [[website](https://rstudio.com/products/rpackages/)].


At every start of the R studio, only the default packages will be loaded by default. Other packages which are already installed have to be loaded explicitly using the `library(package.name)` function. *Think about digging a well. You only have to dig once `installed.packages()` and fetch water everytime you need one `library(package.name)`*


Some common R packages are
  -  *ggplot2*
  -  *dplyr*
  -  *tidyr*
  -  *readr*
  
These packages are also embedded in the Tidyverse package and a detailed description of can be found on the package  [[website](https://www.tidyverse.org/packages/)].


#### Exercise 1A.2
  
- Install the `tidyverse` package

- Load the `tidyverse` package



##  Data handling: import/export data to/from R

You can load/export data into R from various data format including ".csv", ".txt", ".dta", ".sav", ".rjson" among others.

For this activity, we will use the Adolescent Girls and Young Women (AGYW) [[Dataset](https://github.com/eolamijuwon/Workshops_Seminars/blob/master/2020/Workshops/DemogRaphic%20Research%20and%20Data%20Analysis/Data%20-%20Misc/Nigeria%20%5BDHS%5D.dta)]. The dataset is a subset of the Women's recode Nigeria Demographic and Health Survey published in 2018. The dataset was obtained with permission from the [[DHS Website](https://dhsprogram.com/)].

The subset data contains basic demographic and health information of adolescent girls and young women aged 15 -24 years who were usual residents or visitors at an household selected for interview. The full NDHS dataset can be downloaded on the [[DHS Website](https://dhsprogram.com/)].

Since the AGYW dataset is stata formatted, a function that can open the dataset is required.
We could get one from the `readstata13` package. 

*Remember that the package needs to be installed and loaded*

```{r}
install.packages("readstata13")
library(readstata13)
```

#### Exercise 1A.3

Using your background knowledge of how functions work, use the `read.dta13()` function from `readstata13` package to load the AGYW dataset (downloaded on your computer) and assign it to *agyw.dataset*

*Example:* agmy.data <- `"C:/Users/Nigeria/Ekiti State/Federal University Oye/Demo-SocStat/Nigeria [DHS].dta"`


You can browse the dataset with "View"

`View(agyw.dataset)`
  
You can check the structure of your dataset

`str(agyw.dataset)`
  

Alternatively you can use the "glimpse" function from the dplyr package to view the data structure

`glimpse(agyw.dataset)`
  

You can examine the first few observations in the dataset (could be 5/10/more) using the `head` function

`head(agyw.dataset, n=30)`
  
  
You can check how many rows(number of observations) are in your dataset using the `nrow` function 
  
`nrow(agyw.dataset)`
  
You can check how many rows(number of variables) are in your dataset using the `ncol` function 

`ncol(agyw.dataset)`




##  Data Wrangling/Management

Data wrangling refers to the process of cleaning, restructuring and enriching the raw data available into a more usable format. This could include, creating new information from raw data, dropping values or organizing data.


### Activity - Patterns of Contraceptive Use Among AGYW

Clean the *agyw.dataset* and assign it to *agyw.clean*

- Keep only AGYW who are not "at risk of pregnancy" `filter()`. That is, not currently pregnant, sexually active (in the last one month), and not currently amenorrheic. See [Measuring contraceptive prevalence among women who are at risk of pregnancy](https://doi.org/10.1016/j.contraception.2017.06.007) for details.

- Create a new vector/variabel `mutate()`
  
  - *mCuse* with categories Using or not using modern contraception
  
  - *teen_educ* with categories "< Secondary" and "Secondary +"
  
  - *religion* with categories "Catholic", "Other Christian", "Muslim", and "Others"

- Rename vectors/variables `rename()`:

  - *v024* = "religion"
  
  - *v025* = "residence"
  
- Keep only vectors/variables of interest `select()`


```{r}

agyw.clean <- agyw.dataset %>% 

              ## Keep only AGYW "at risk"" of pregnancy
              filter (v213 == "no or unsure") %>% ## AGYW who are not currently pregnant
              filter (v529 == 0) %>%              ## Sexually active AGYW (< 1 month) %>% 
              filter (v405 == "no") %>%
              
              mutate (mCuse = ifelse((v364 == "using modern method"),
                                      "Using Modern Contraceptives",
                                      "Not Using Modern Contraceptives")) %>% 
                                      
              ## Complete the line of code below
              #mutate (teen_educ = ifelse()) %>% 
              
              ##
              ## To reclassify vectors to more than two categories,
              ## we you use derivedFactor() function from the *mosaic* package
              mutate (religion = derivedFactor("Catholic" = (v130 == "catholic"),
                                                "Other Christian" = (v130 == "other christian"),
                                                "Muslim" = (v130 == "islam"),
                                                "Others" = (v130 == "traditionalist" |
                                                          v130 == "other"),
                                                .default = NA)) %>% 
              rename (region = v024,
                      residence = v025) %>% 
                                                
              select (c("mCuse", "religion", "region",
                        "residence"))
```



-  Descriptive statistics in R

-  Contingency tables (cross-tabulations)

-  Analysis of Complex Surveys with [`survey`](http://asdfree.com/demographic-and-health-surveys-dhs.html)



TeenData <- read.csv(".\\teenageData.csv")

#   Note that <- is an assignment function (just as = in STATA)

#   You could also read the data into RStudio using the read_csv function in readr package

TeenDatR <- read_csv ("C:\\Users\\eOlamijuwon\\OneDrive\\Research\\Computational Social Science\\Eswatini UseR\\Intro to R\\teenageData.csv")


# since the data is also available online, we could also read it directly from github

```




## Descriptive statistics in R

## Contingency tables (cross-tabulations)

## Analysis of Complex Surveys with [`survey`](http://asdfree.com/demographic-and-health-surveys-dhs.html)






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


  
  
NOTE:: Solutions to Practice exercise:

  - [[Practice Exercise I](https://github.com/eolamijuwon/EswatiniUser/blob/master/Intro%20to%20R/Solutions/Practice%20Exercise%20I.R/)]
  
  - [[Practice Exercise II](https://github.com/eolamijuwon/EswatiniUser/blob/master/Intro%20to%20R/Solutions/Practice%20Exercise%20II.R)]
  
  - [[Class Activity](https://github.com/eolamijuwon/EswatiniUser/blob/master/Intro%20to%20R/Solutions/Practice%20Exercise%20II.R)]
  

# Acknowledgements

Some of the materials used in this session were adapted from:

- https://www.tutorialspoint.com/r/r_packages.htm
