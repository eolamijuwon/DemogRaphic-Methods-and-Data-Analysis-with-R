---
Title: "Introduction to Data Analysis with R"

Date: "10 February 2020"
---


#

# Introduction to R and RStudio


## R syntax and Basic operations: assigning

The R synthax would usually comprise of:

  - variables and functions
  
  - `<-` the assignment operator
  
  - = for arguments
  
  - `#` for comments/description/documentation
  
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

## R Functions


#### Exercise 1A.1

Write a function to:

  - Find the difference between 125 and its 3/5. Using the same function, find the difference between 50 and it's 1/5.
  
  - Find a number which is greater than 17 as much as it is less than 31. Using the same function, find the number which is greater than 16 as much as it is less than 50.


## R packages

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
  
- Install the `tidyverse` and `mosaic` package

- Load the `tidyverse` and `mosaic` package


#
#  Data handling: import/export data to/from R

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

#### Exercise 1B.1

- Using your background knowledge of how functions work, use the `read.dta13()` function from `readstata13` package to load the AGYW dataset (downloaded on your computer) and assign it to *agyw.dataset*

- *Example:* agmy.data <- `"C:/Users/Nigeria/Ekiti State/Federal University Oye/Demo-SocStat/Nigeria [DHS].dta"`


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



#
#  Data Wrangling/Management

Data wrangling refers to the process of cleaning, restructuring and enriching the raw data available into a more usable format. This could include, creating new information from raw data, dropping values or organizing data.


## Activity - Patterns of Contraceptive Use Among AGYW

Clean the *agyw.dataset* and assign it to *agyw.clean*

- Keep only AGYW who are not "at risk of pregnancy" `filter()`. That is, not currently pregnant, sexually active (in the last one month), and not currently amenorrheic. See [Measuring contraceptive prevalence among women who are at risk of pregnancy](https://doi.org/10.1016/j.contraception.2017.06.007) for details.

- Create a new vector/variabel `mutate()`
  
  - *mCuse* with categories Using or not using modern contraception
  
  - *teen_educ* with categories "< Secondary" and "Secondary +"
  
  - *religion* with categories "Catholic", "Other Christian", "Muslim", and "Others"
  
  - *weight* to adjust for complex design

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
              mutate (mCuse = as.factor(mCuse)) %>% 
                                      
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
                                                
              ##
              ##
              ##
              
              rename (region = v024,
                      residence = v025) %>% 
                                                
              select (c("mCuse", "religion", "region",
                        "residence", "v005", "v021"))
                        
```


#
# Descriptive statistics in R

Descriptive statistics are used to describe the basic features of the data in a study. It involves describing, showing or summarizing the data in a meaningful way. They provide simple summaries about the sample and the measures.

Descriptive statistics therefore enables us to present the data in a more meaningful way, which allows simpler interpretation of the data. Descriptive statistics are very important it would be hard to visualize what a data is showing by simply presenting raw data, especially if there was a lot say 3,500 students.


## Univariate Analysis

Univariate analysis involves describing the distribution of a single variable, including its central tendency (including the mean, median, and mode) and dispersion (including the range and quartiles of the data-set), measures of spread (such as the variance and standard deviation). Distributions may also be displayed using percentages.  For example, you could use percentages to describe the:

- percentage of AGYW in different categories of modern contraceptive use
- percentage of AGYW in different regions
- percentage of AGYW in different levels of education

```{r}

cont_use <- data.frame(table(agyw.clean$mCuse)) %>% 
            mutate(perc = round((Freq/sum(Freq))*100, digits = 2))
```

OR

```{r}

cont_use <- data.frame(table(agyw.clean$mCuse)) %>% 
            mutate (perc = Freq/sum(Freq)) %>% 
            mutate (perc = perc * 100) %>% 
            mutate (perc = round(perc, digits = 2))
            
```

*Think about `%>%` function as "then". E.g. Tabulate modern contraceptive use, then calculate the percentage distribution and assign it to `perc`, then multiply by 100, then round to 2 d.p.


#### Exercise 1D.1
  
- Create a dataframe **region_freq** with information on the number and percentage of AGYW in the different region.

- Create a dataframe **residence_freq** with information on the number and percentage of AGYW in each residence.

- Create a dataframe **religion_freq** with information on the number and percentage of AGYW in the different religious denomination.


#
## Bivariate Analysis (Cross-Tabulations)

Descriptive statistics may also be used to describe the relationship between pairs of variables. 

This could include:
- Cross-tabulations and contingency tables (a pair of categorical variables)
- Graphical representation via scatterplots (a pair of continuous variables)

For example, we could be interested in the level of modern contraceptive use among AGYW by:
- educational attainment
- place of residence
- region of residence


#
# Analysis of Complex Surveys with [`survey`](http://asdfree.com/demographic-and-health-surveys-dhs.html)

A sample survey obtains data from a subset of a population, in order to estimate population attributes. A complex (multistage) sample survey on the other hand refers to a survey that involves complex sampling designs. That is, the selection of final units of observation is accomplished through a series of stages, for example stratification and multistage sampling. See [Introduction to the design and analysis of complex survey data](http://eprints.lse.ac.uk/76991/1/Skinner_Introduction%20to%20the%20design.pdf) for a detailed explanation of complex survey design and analysis.

The DHS datasets used in this workshop were collected using a sample designs that involves two-stage probability samples drawn from an existing sample frame, generally the most recent census frame. A probability sample is defined as one in which the units are selected randomly with known and nonzero probabilities. Typically, DHS samples are stratified by geographic region and by urban/rural areas within each region. Detailed information on analyzing DHS data is available [online](https://dhsprogram.com/Data/Guide-to-DHS-Statistics/Analyzing_DHS_Data.htm).


## Univariate Analysis

```{r}
install.packages("survey")

library(survey)

agyw.clean$strata <- do.call( paste , agyw.clean[ , c( 'region' , 'residence' ) ] )

agyw.clean$weight <- agyw.clean$v005/(10^6)


dhs_design <- svydesign( 
  ~ v021, strata = ~strata, 
        data = agyw.clean, weights = ~weight)

table_mCuse <- agyw.clean %>% 
  count(mCuse) %>% 
  mutate(perc = round((n/sum(n))*100, digits = 2)) %>% 
  mutate("weighted_perc" = round((svytable ( ~ mCuse, dhs_design, Ntotal=TRUE)) *100, digits = 2)) %>% 
  rename("Characteristic" = mCuse)
  
table_mCuse

```

## Bivariate Analysis

```{r}

### Calculate Row Percentages
rowBiv_education <- svyby( ~ mCuse, ~ region,  dhs_design, svymean) %>% 
    rename( "perc_Using" = "mCuseUsing Modern Contraceptives",
            "perc_notUsing" = "mCuseNot Using Modern Contraceptives") %>% 
    dplyr::select("region", "perc_Using", "perc_notUsing") %>% 
    mutate (perc_Using = round((perc_Using * 100), digits = 2)) %>% 
    mutate (perc_notUsing = round((perc_notUsing * 100), digits = 2)) %>% 
    mutate (perc_Using = paste0(perc_Using, "%")) %>% 
    mutate (perc_notUsing = paste0(perc_notUsing, "%"))

View(rowBiv_education)   

### Calculate Column Percentages
colBiv_education <- svyby( ~ mCuse, ~ region,  dhs_design, svytotal) %>% 
    rename (`Not Using mContr` =  `mCuseNot Using Modern Contraceptives`,
            `Using mContr` = `mCuseUsing Modern Contraceptives`) %>% 
    
    mutate (wPerc_notUsing = round(((`Not Using mContr`/sum(`Not Using mContr`))*100), 2),
            wPerc_Using = round(((`Using mContr`/sum(`Using mContr`))*100), 2)) %>% 
          
    mutate (wPerc_notUsing = paste0(wPerc_notUsing, "%"),
            wPerc_Using = paste0(wPerc_Using, "%")) %>% 
    select (c("region", "wPerc_Using", "wPerc_notUsing"))
View(colBiv_education)   

```

#### Exercise 1F.1
  
- Create a dataframe `table_region` with information on:

  - The number (freq) of AGYW in the different region.
  
  - Unweighted percentage distribution of AGYW in the regions.
  
  - Weighted percentage distribution of AGYW in the regions.

- Repeat the above for `table_residence`, and `table_religion`


#### Exercise 1F.2
  
- Create a dataframe `rowBiv_region` with information on:

  - **Unweighted** number (freq) of AGYW in the different region who are using modern contraceptives.
  
  - **Unweighted** number (freq) of AGYW in the different region who are not using modern contraceptives.
  
    - *Hint: You may need to install a new package*
  
  - **Weighted** *(row)* percentage distribution of AGYW in the regions who are using modern contraceptives.

  - **Weighted** *(row)* percentage distribution of AGYW in the regions who are not using modern contraceptives.

- Repeat the above for `rowBiv_residence`, and `rowBiv_religion`
