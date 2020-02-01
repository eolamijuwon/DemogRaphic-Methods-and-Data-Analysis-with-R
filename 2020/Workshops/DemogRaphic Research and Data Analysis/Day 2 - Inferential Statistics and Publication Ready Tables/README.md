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



## Logistic Regression Model - adjusting for complex survey design

mCuse_logReg_I <- svyglm (mCuse ~ relevel(teen_educ, ref = "Secondary +"),
                    dhs_design, family = binomial(link="logit"))

mCuse_logReg_II <- svyglm (mCuse ~ relevel(teen_educ, ref = "Secondary +") + 
                    relevel(region, ref = "south west") +
                    factor(residence) + factor(religion),
                    dhs_design, family = binomial(link="logit"))

summary(mCuse_logReg_II)    
    





# Publication Ready Tables


A list of supported styles in `stargazer` is available [online](https://rdrr.io/cran/stargazer/man/stargazer_style_list.html)

```{r}    
    
install.packages("stargazer")
library(stargazer)

stargazer(mCuse_logReg_I, mCuse_logReg_II, 
          type = "text", digits = 2, 
          single.row = FALSE, ci = TRUE, 
          ci.level = 0.95, 
          style = "demography",
          ci.separator = "; ",
          covariate.labels=c("< Secondary",
                             "North Central",
                             "North East",
                             "North West",
                             "South East",
                             "South South",
                             "Rural", 
                             "Other Christian",
                             "Muslim", "Others"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "./2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/TeenPreg_logitModel.html")

```    
  
!![demographics map]("https://github.com/eolamijuwon/Workshops_Seminars/blob/master/2020/Workshops/DemogRaphic%20Research%20and%20Data%20Analysis/Data%20-%20Misc/TeenPreg_logitModel.html")
  

