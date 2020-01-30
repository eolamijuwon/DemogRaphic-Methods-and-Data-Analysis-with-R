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

- Write a function to:

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

##  Data Wrangling/Management


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
