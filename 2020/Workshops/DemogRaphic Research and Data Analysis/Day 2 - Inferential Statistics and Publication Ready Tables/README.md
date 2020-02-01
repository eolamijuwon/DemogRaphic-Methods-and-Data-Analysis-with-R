---
Title: "Inferential Statistics and Publication Ready Tables in R"

Date: "11 February 2020"
---


#

# Recap

- Install packages **(when required)**

- Load libraries at every fresh start

- Import dataset(s)

- View data structure

- Clean/manage dataset

- Descriptive statistics (comples surveys)


```{r}
library (tidyverse)
library (mosaic)
library (readstata13)
library (survey)
library (ecodist)


agyw_dataset <- read.dta13("./2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/Nigeria [DHS].dta")

str (agyw_dataset)

agyw_clean <- agyw_dataset %>% 

              ## Keep only AGYW "at risk"" of pregnancy
              filter (v213 == "no or unsure") %>% ## AGYW who are not currently pregnant
              filter (v529 == 0) %>%              ## Sexually active AGYW (< 1 month) %>% 
              filter (v405 == "no") %>%
              
              mutate (mCuse = ifelse((v364 == "using modern method"),
                                      "Using Modern Contraceptives",
                                      "Not Using Modern Contraceptives")) %>% 
              mutate (mCuse = as.factor(mCuse)) %>% 
                                      
              ## Complete the line of code below
              mutate (teen_educ = ifelse((v106 == "no education" |v106 == "primary"),
                                            "< Secondary", "Secondary +")) %>% 
              mutate (teen_educ = as.factor(teen_educ)) %>% 
              
              ##
              ## To reclassify vectors to more than two categories,
              ## we you use derivedFactor() function from the *mosaic* package
              mutate (religion = derivedFactor("Catholic" = (v130 == "catholic"),
                                                "Other Christian" = (v130 == "other christian"),
                                                "Muslim" = (v130 == "islam"),
                                                "Others" = (v130 == "traditionalist" |
                                                          v130 == "other"),
                                                .default = NA)) %>% 
                                                
              mutate (weight = v005/10^6) %>% 
              ##
              ##
              dplyr::rename (region = v024,
                              residence = v025) %>% 
                                                
              select (c("mCuse", "religion", 
                        "region", "teen_educ",
                        "residence", "weight",
                        "v021"))

```

#

# Bivariate Associations

In the introduction session, we examined the prevalence of current modern contraceptive use across sociodemographic characteristics including education, place of residence, region of residence and religious affiliation. In this module, we will examine bivariate relationships using *chisquare* since the variables are categorical. Since we are still working with the DHS, we will also take into consideration, the complex design of the survey.

```{r}

agyw_clean$strata <- do.call( paste , agyw_clean[ , c( 'region' , 'residence' ) ] )


dhs_design <- svydesign(~ v021, 
                        strata = ~strata, 
                        data = agyw_clean,
                        weights = ~weight,
                        nest = TRUE)

## Compute the prevalence of Modern Contraceptive Use
## Total Frequency, row % of those using mCuse, chisq

chi2q_education <- svychisq(~mCuse + teen_educ, dhs_design)$p.value
chi2q_education <- format(if_else(  (chi2q_education<0.0001), 
                                    0.0001, 
                                    chi2q_education, 
                                    missing = 0), 
                          scientific = FALSE)

rowBiv_education <- svyby( ~ mCuse, ~ teen_educ,  dhs_design, svymean) %>% 
    dplyr::rename( "perc_Using" = "mCuseUsing Modern Contraceptives",
            "perc_notUsing" = "mCuseNot Using Modern Contraceptives") %>% 
    mutate (freq = count(agyw_clean$teen_educ)$freq) %>% 
    dplyr::select("teen_educ", "freq", "perc_Using") %>% 
    mutate (perc_Using = round((perc_Using * 100), digits = 2)) %>% 
    mutate (perc_Using = paste0(perc_Using, "%")) %>% 
    mutate (chi_sq = chi2q_education)
rownames(rowBiv_education) <- NULL

##        
        
```

#### Exercise 2A.1

Estimate the prevalence of modern contraceptive use by region, place of residence, and religious affiliation

    
    
# Regression Models

Regression analysis is a statistical technique for estimating the association between two or more quantitative variables. With regression analysis, we are interested in the type as well as the degree of association between (simple regression) or among (multiple regression) variables. For example, one might be interested in understanding the underlying effect of an (independent) variable on another (dependent variable) for example, the effect of women’s education/empowerment on the adoption of modern contraceptive use. At the same time, one might also want to assess the “statistical significance” of the estimated relationships, that is, the degree of confidence that the true relationship is close to the estimated relationship. Although the independent variables may explain the underlying variations in the dependent variable, it does not automatically imply causation.

Some common regression models in social sciences are:

- Linear regression model

- Logistic regression model

  - Ordinary/binary logistic model `binomial(link = "logit")`
  
  - Complementary logistic model `binomial(link = "cloglog")`
  
  - Ordinal logistic regression model `polr`
  
  - Multinomial logistic regression model

- Poisson regression model `poisson(link = "log")`

  - Quasipoisson model `quasipoisson(link = "log")`
  
  - Negative binomial model `glm.nb` from `MASS` package
  
- Survival models

  - Cox-proportional hazard models
  
  - Competing risks models


    
    
    
# Publication Ready Tables
    
    
    
    
    
    
    

#### Exercise 1B.1

- Using your background knowledge of how functions work, use the `read.dta13()` function from `readstata13` package to load the AGYW dataset (downloaded on your computer) and assign it to *agyw.dataset*

- *Example:* agmy_data <- `"C:/Users/Nigeria/Ekiti State/Federal University Oye/Demo-SocStat/Nigeria [DHS].dta"`

You can browse the dataset with "View"

`View(agyw.dataset)`


## Data Structures

The basic data structures in R programming includes vectors, matrices, data frames and lists. It refers to the way in which multiple values are stored. A vector is the most common and basic data structure in R, and is pretty much the workhorse of R. It’s basically a collection of values, mainly 

- `numeric`: 1, 3, 5.2, 7

- `integer`: 2L, 5L

- `character`: "Matthew is a student", "Rebecca is a doctor", "John is a professor"

- `logical`: TRUE, FALSE

Note that all values in a vector must be of the same data type. If you try to create a vector with more than a single data type, R will try to coerce it into a single data type.

You can check the structure of your dataset

`str(agyw_dataset)`
  

Alternatively you can use the "glimpse" function from the dplyr package to view the data structure

`glimpse(agyw_dataset)`
  

You can examine the first few observations in the dataset (could be 5/10/more) using the `head` function

`head(agyw_dataset, n=30)`
  
  
You can check how many rows(number of observations) are in your dataset using the `nrow` function 

`nrow(agyw_dataset)`
  
You can check how many rows(number of variables) are in your dataset using the `ncol` function

`ncol(agyw_dataset)`



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
agyw_clean <- agyw_dataset %>% 

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

cont_use <- data.frame(table(agyw_clean$mCuse)) %>% 
            mutate(perc = round((Freq/sum(Freq))*100, digits = 2))
```

OR

```{r}

cont_use <- data.frame(table(agyw_clean$mCuse)) %>% 
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


The analysis factor [website](https://www.theanalysisfactor.com/complex-sample-what-and-why/) also provides some very useful tips about complex sample surveys and some incredible good reasons to use of complex samples:

- Complex samples can be incredibly cost effective, while improving the precision of sample estimates.

- Complex samples allow access to difficult-to-access sampling frames.

- Complex samples can ensure sufficient representation of small sub-population groups in the final sample.

## Univariate Analysis

```{r}
install.packages("survey")

library(survey)

## Remember that the DHS samples are stratified by geographic region 
## and by urban/rural areas within each region.

agyw_clean$strata <- do.call( paste , agyw_clean[ , c( 'region' , 'residence' ) ] )


## Sample weights are calculated to six decimals but are 
## presented in the standard recode files without the decimal point.
## They need to be divided by 1,000,000 before use to approximate the number of cases.

agyw_clean$weight <- agyw_clean$v005/(10^6)

```

Sampling weights can be applied in two main ways:

- A simple application of weights when all that is needed are indicator estimates.

- As part of complex sample parameters when standard errors, confidence intervals or significance testing is required for the indicator.

```{r}

dhs_design <- svydesign( 
  ~ v021, strata = ~strata, 
        data = agyw_clean, weights = ~weight)

table_mCuse <- agyw_clean %>% 
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

  - The **unweighted** number (freq) of AGYW in the different region.
  
  - **Unweighted** percentage distribution of AGYW in the regions.
  
  - **Weighted** percentage distribution of AGYW in the regions.

- Repeat the above for `table_residence`, and `table_religion`


#### Exercise 1F.2
  
- Create a dataframe `rowBiv_region` with information on:

  - **Unweighted** number (freq) of AGYW in the different region who are using modern contraceptives.
  
  - **Unweighted** number (freq) of AGYW in the different region who are not using modern contraceptives.
  
    - *Hint: You may need to install a new package*
  
  - **Weighted** *(row)* percentage distribution of AGYW in the regions who are using modern contraceptives.

  - **Weighted** *(row)* percentage distribution of AGYW in the regions who are not using modern contraceptives.

- Repeat the above for `rowBiv_residence`, and `rowBiv_religion`
