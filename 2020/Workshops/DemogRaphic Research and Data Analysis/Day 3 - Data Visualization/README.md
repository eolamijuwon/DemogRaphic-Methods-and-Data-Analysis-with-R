---
Title: "Inferential Statistics and Publication Ready Tables in R"

Date: "11 February 2020"
---


#

![Image]("./MigrationFlows.PNG")

![Image]("./_ImmigrationFlows_ZA.PNG")

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
    mutate (freq = (agyw_clean %>% count(teen_educ))$n) %>% 
    dplyr::select("teen_educ", "freq", "perc_Using") %>% 
    mutate (perc_Using = round((perc_Using * 100), digits = 2)) %>% 
    mutate (perc_Using = paste0(perc_Using, "%")) %>% 
    rename (characteristics = teen_educ)
    
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

WmCuse_logReg_I <- svyglm (mCuse ~ relevel(teen_educ, ref = "Secondary +"),
                    dhs_design, family = binomial(link="logit"))

WmCuse_logReg_II <- svyglm (mCuse ~ relevel(teen_educ, ref = "Secondary +") + 
                    relevel(region, ref = "south west") +
                    factor(residence) + factor(religion),
                    dhs_design, family = binomial(link="logit"))


mCuse_logReg_II <- glm (mCuse ~ relevel(teen_educ, ref = "Secondary +") + 
                    relevel(region, ref = "south west") +
                    factor(residence) + factor(religion),
                    data = agyw_clean, family = binomial(link="logit"))

summary(mCuse_logReg_II)    
    





# Publication Ready Tables

## Univariate - Frequency Distributions



```{r}

install.packages("compareGroups")

library (compareGroups)

descriptive <- descrTable((agyw_clean[, 1:5]))

export2word(descriptive, file = "./2020/Workshops/DemogRaphic Research and Data Analysis/Day 2 - Inferential Statistics and Publication Ready Tables/descriptiveTable.docx")

export2html(descriptive, file = "./2020/Workshops/DemogRaphic Research and Data Analysis/Day 2 - Inferential Statistics and Publication Ready Tables/descriptiveTable.html")

```

## Univariate (Complex Surveys) - Frequency Distributions


```{r}

tab <- descrTable(mCuse ~ . , (agyw_clean[, 1:5]))


```


## Bivariate/CrossTabulations

```{r}

bivariate <- descrTable(mCuse ~ . , (agyw_clean[, 1:5]), byrow = TRUE)

export2html(bivariate, file = "./2020/Workshops/DemogRaphic Research and Data Analysis/Day 2 - Inferential Statistics and Publication Ready Tables/bivariateTable.html")

```

## Bivariate/CrossTabulations (Complex Survey)

You will need to have completed exercise 2A.1 to proceed

```{r}

install.packages("kableExtra")
library (kableExtra)

wBivariate <- rbind ( rowBiv_religion,
                      rowBiv_region,
                      rowBiv_education,
                      rowBiv_residence) %>% 
              rename (`sample (n)` = freq)


wBivariate  %>%
  kable(caption = "Prevalence of Modern Contraceptive Use",
        align = "l", booktabs = T, position = "centre") %>%
  kable_styling("striped", full_width = F, row_label_position="l") %>%
  group_rows(paste0("Religion (p < ", chi2q_religion, ")"), 1, 4) %>% 
  group_rows(paste0("Region (p < ", chi2q_region, ")"), 5, 10) %>% 
  group_rows(paste0("Education (p < ", chi2q_education, ")"), 11, 12) %>% 
  group_rows(paste0("Place of Residence (p < ", chi2q_residence, ")"), 13, 14) %>% 
  save_kable(file = "./2020/Workshops/DemogRaphic Research and Data Analysis/Day 2 - Inferential Statistics and Publication Ready Tables/wBivariate.html", self_contained = T, bs_theme = "simplex")

```


## Regression Tables

A list of supported styles in `stargazer` is available [online](https://rdrr.io/cran/stargazer/man/stargazer_style_list.html)

```{r}    
    
install.packages("stargazer")
library(stargazer)

stargazer(WmCuse_logReg_I, WmCuse_logReg_II, mCuse_logReg_II, 
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
          dep.var.labels   = "Modern Contraceptive Use",
          out = "./2020/Workshops/DemogRaphic Research and Data Analysis/Day 2 - Inferential Statistics and Publication Ready Tables/TeenPreg_logitModel.html")

```    

Alternatively, we could use the KableExtra package to style our own regression table.


```{r}

signif.num <- function(x) {
    symnum(x, corr = FALSE, na = FALSE, legend = FALSE,
           cutpoints = c(0, 0.001, 0.01, 0.05, 1), 
           symbols = c("***", "**", "*", " "))
}

#####################
## Model I - Weighted

pval <- (summary(WmCuse_logReg_I)$coefficients[,4])
        logReg_I_pv <- as.data.frame(signif.num(pval))
        rownames (logReg_I_pv) <- NULL

tab_logReg_I <- cbind(OR = coef(WmCuse_logReg_I), confint(WmCuse_logReg_I)) %>% 
                    exp() %>% 
                    data.frame() %>% 
                    add_rownames("Characteristics") %>%
                    mutate ("OR" = round(OR, digits = 2)) %>% 
                    mutate ("LCI" = round(`X2.5..`, digits = 2)) %>% 
                    mutate ("UCI" = round(`X97.5..`, digits = 2)) %>% 
                    mutate ("Characteristics" = c("Intercept", "< Secondary")) %>% 
                    mutate (`95% CI` = paste0(LCI, "; ", UCI)) %>% 
                    select(c("Characteristics", "OR", "95% CI"))

tab_logReg_I <- cbind(tab_logReg_I, logReg_I_pv) %>% 
                mutate (OR = paste0(OR, " ", `signif.num(pval)`)) %>% 
                select(-c("signif.num(pval)"))
                
                
######################
## Model II - Weighted

pval <- (summary(WmCuse_logReg_II)$coefficients[,4])
        logReg_II_wpv <- as.data.frame(signif.num(pval))
        rownames (logReg_II_wpv) <- NULL

tab_wlogReg_II <- cbind(OR = coef(WmCuse_logReg_II), confint(WmCuse_logReg_II)) %>% 
                    exp() %>% 
                    data.frame() %>% 
                    add_rownames("Characteristics") %>%
                    mutate ("OR" = round(OR, digits = 2)) %>% 
                    mutate ("LCI" = round(`X2.5..`, digits = 2)) %>% 
                    mutate ("UCI" = round(`X97.5..`, digits = 2)) %>% 
                    mutate ("Characteristics" = c("Intercept",
                                                  "< Secondary",
                                                  "North Central",
                                                  "North East",
                                                  "North West",
                                                  "South East",
                                                  "South South",
                                                  "Rural",
                                                  "Other Christian",
                                                  "Muslim",
                                                  "Others")) %>% 
                    mutate (`95% CI` = paste0(LCI, "; ", UCI)) %>% 
                    select(c("Characteristics", "OR", "95% CI"))

tab_wlogReg_II <- cbind(tab_wlogReg_II, logReg_II_wpv) %>% 
                  mutate (OR = paste0(OR, " ", `signif.num(pval)`)) %>% 
                  select(-c("signif.num(pval)"))


########################
## Model II - Unweighted

pval <- (summary(mCuse_logReg_II)$coefficients[,4])
        logReg_II_pv <- as.data.frame(signif.num(pval))
        rownames (logReg_II_pv) <- NULL

tab_logReg_II <- cbind(OR = coef(mCuse_logReg_II), confint(mCuse_logReg_II)) %>% 
                    exp() %>% 
                    data.frame() %>% 
                    add_rownames("Characteristics") %>%
                    mutate ("OR" = round(OR, digits = 2)) %>% 
                    mutate ("LCI" = round(`X2.5..`, digits = 2)) %>% 
                    mutate ("UCI" = round(`X97.5..`, digits = 2)) %>% 
                    mutate ("Characteristics" = c("Intercept",
                                                  "< Secondary",
                                                  "North Central",
                                                  "North East",
                                                  "North West",
                                                  "South East",
                                                  "South South",
                                                  "Rural",
                                                  "Other Christian",
                                                  "Muslim",
                                                  "Others")) %>% 
                    mutate (`95% CI` = paste0(LCI, "; ", UCI)) %>% 
                    select(c("Characteristics", "OR", "95% CI"))

tab_logReg_II <- cbind(tab_logReg_II, logReg_II_pv) %>% 
                  mutate (OR = paste0(OR, " ", `signif.num(pval)`)) %>% 
                  select(-c("signif.num(pval)"))

############

tab_logReg_I %>% 
        right_join(tab_wlogReg_II, by = "Characteristics") %>% 
        right_join(tab_logReg_II, by = "Characteristics") %>%
        filter (Characteristics != "Intercept") %>% 
        rename (OR = "OR.x", OR = "OR.y",
                `95% CI` = `95% CI.x`,
                `95% CI` = `95% CI.y`) %>% 
        kable(caption = "Determinants of Modern Contraceptive Use Among AGYW",
              align = "l", booktabs = T, position = "centre") %>%
        kable_styling("striped", full_width = F, row_label_position="l") %>%
        group_rows("Education (Ref = Sec +)", 1, 1) %>% 
        group_rows("Region (Ref = south west)", 2, 6) %>% 
        group_rows("Residence (Ref = Urban)", 7, 7) %>% 
        group_rows("Religious Affiliation (Ref = Catholic)", 8, 10) %>% 
        add_header_above(c(" ",
                          "Weighted Simple Model" = 2,
                          "Weighted Multiple Model" = 2,
                          "Unweighted Multiple Model" = 2)) %>%
save_kable(file = "./2020/Workshops/DemogRaphic Research and Data Analysis/Day 2 - Inferential Statistics and Publication Ready Tables/regressKable.html",
          self_contained = T, bs_theme = "simplex")

```
