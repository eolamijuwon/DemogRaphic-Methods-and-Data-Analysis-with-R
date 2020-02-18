---
Date: 13 February 2020
Title: Basic Demographic Methods
output: pdf_document
---


#




# Basic Demographic Measures

In this session, we will cover some basic concepts and techniques of demographic analysis using R. Participants will become familiar with some basic measures of fertility, mortality (including standardization) and population dynamics (dependency ratios).

In this session, we will combine data from multiple sources for our analysis. For the first part, import the population counts data obtained from the WorldBank DataBank [website](http://databank.worldbank.org/data/home.aspx) and assign it to `WB_Popdata`. The spreadsheet contains the estimated population counts of Nigeria since 2006.


```{r}
library (tidyverse)
library (readxl)
library (mosaic)

WB_PopData <- read_xlsx("./Data - Misc/NGA - Pop Distribution.xlsx")

WB_data_edited <- WB_PopData %>% 
                  ## Do not select Country name, Series Code, and country code.
                  select(-c("Country Name",
                            "Series Code",
                            "Country Code")) %>% 
                  ## Seperate the Series Name column into Age and Sex
                  ## The two new columns should be seperated based on the position of ","
                  separate(`Series Name`, 
                           c("Age", "Sex"),
                           fill = "right", sep = ",",) %>% 
                  ## Filter for rows with counts of male or female population not percentages
                  filter (Sex != " female (% of female population)" &
                          Sex != " male (% of male population)" &
                          Sex != " total") %>% 
                  ## Filter for rows with counts of male or female population for population for 5year age groups
                  ## Note that you could also retain the age groups and drop others for the dependency ratio estimations.
                  filter (Age != "Population ages 65 and above" &
                          Age != "Population ages 0-14" &
                          Age != "Population ages 15-64") %>% 
                  ## Extract age groups from the string of text in the Age column
                  mutate (Age = str_sub(Age, -5, -1)) %>% 
                  mutate (Age = replace(Age, which(Age == "above"), "80+")) %>% 
                  
                  ## Reshape data from wide to long format using `gather` or `pivot_longer`
                  pivot_longer( cols = `2018 [YR2018]`:`2006 [YR2006]`,
                                names_to = "year", 
                                values_to = "population",
                                values_drop_na = TRUE) %>% 
                  mutate (year = str_sub(year, 1, 4))
```



### Age Dependency Ratios

The age dependency ratio expresses the relationship between three age groups comprising of 0-15, 16-64 and 65-plus within a population. It is a measure of the number of dependents ages 0-14 and 65+ compared with the total population aged 15 to 64 for the given year. A high dependency ratio indicate a greater level of burden (in supporting the aging and youth population) on those of working age, and the overall economy, face a greater burden. There are three types of age dependency ratio. The *youth dependency ratio* measure the relationship between the population ages 0-15 and the population ages 16-64. The *old-age dependency ratio* is the population ages 65-plus divided by the population ages 16-64. The *total age dependency ratio* is the sum of the youth and old-age ratios.



```{r}

WB_dependencyRatio <- WB_data_edited %>% 
                      ## Extract the older age in all interval
                      mutate (age_single = str_sub(Age, -2, -1)) %>% 
                      mutate (age_single = replace(age_single, which(age_single == "0+"), 80)) %>% 
                      mutate (age_single = as.numeric(age_single)) %>% 
                      ## Reclassify ages into Young, working age or old
                      mutate (age_classification = derivedFactor("Young" = age_single < 15,
                                                                 "Working" = (age_single >= 15 & age_single <= 64),
                                                                 "Old" = (age_single > 64))) %>% 
                      group_by (year, age_classification) %>% 
                      summarise (population = sum(population)) %>% 
                      ungroup () %>% 
                      pivot_wider(names_from = age_classification, values_from = population) %>% 
                      mutate (youngDR = ((Young/Working)*100),
                              oldAgeDR = ((Old/Working)*100),
                              AgeDR = ((Young + Old)/Working)*100) %>% 
                      mutate (youngDR = round(youngDR, digits = 2),
                              oldAgeDR = round(oldAgeDR, digits = 2),
                              AgeDR = round(AgeDR, digits = 2)) %>% 
                      select (c("year", "youngDR",
                                "oldAgeDR", "AgeDR"))

```

### Basic Measures of Fertility

In the following sections, we will attempt to estimate the measures of fertility including the *Crude birth rate*, *Total fertility rate*, *General fertility rate*, and the *Gross reproduction rate*.

- **The crude birth rate (CBR):** measures the number of live births occurring among the population of a given geographical area during a given year, per 1,000 mid-year total population of the given geographical area during the same year. It could be summarised mathematically as the sum of all live births (in a year) divided by the total population (in the same year). Unlike other measures of fertility, the rate is crude because the total population is represented in the denominator. This measure of fertility ignores all differences in composition of the population such as age and sex.

- **The total fertility rate (TFR):** measures the number of children a woman or (group of women) would have if the population’s age-specific fertility rates remained constant.

- **The general fertility rate (GFR):** measures the total number of live births in a given geographical area during a given year/period per 1000 mid year population of women in child bearing ages 15-49 in the same year.

- **The age specific fertility rate (ASFR):** measures the number of live births occurring to women in each age group in a given geographical area during a given year per 1000 women in the each of the age groups. It could be summarised mathematically as the number of all live births to all women in a specific age group divided by the total number of women in that age group.


- **The gross reproduction rate (GRR):** measures the average number of daughters that would replace their mothers, assuming that the age and sex specific fertility rate for the current period remained constant. It is described as the number of baby girls that would be born to 1000 women passing through their child bearing years, if the age specific birth rates of a given year remained constant and if no women entering the child bearing period died before reaching menopause.

- **The net reproduction rate (NRR):** measures the average number of daughters that would be born to a female (or a group of females) if
she passed through her lifetime conforming to the age-specific fertility and mortality rates of a given year.


In the following session, we will combine the births data from the demographic and health survey with the population estimates obtained from the World Bank. Since the demographic and health survey is a national survey that monitors the demographic and health changes in a population, we will obtain the counts of total births in the popuilation for the year 2018. We apply the DHS sampling weights while obtaining the counts of number of births for women in each age group. We will also assume that the DHS children sample is 1/1000th of the total number of children in the country.

```{r}

library (readstata13)
library (survey)
library(tibble)


DHS_children <- read.dta13("./Data - Misc/[Sample] Nigeria  - children.dta")

DHS_children <- DHS_children %>% 
                ## FIlter for children born only in 2018
                filter (b2 == 2018) %>% 
                ## Resample the DHS sample weights.
                mutate (weight = v005/10^3)
                
DHS_children$strata <- do.call( paste , DHS_children[ , c( 'v024' , 'v025' ) ] )

dhs_cDesign <- svydesign(~ v021, strata = ~strata, 
                        data = DHS_children, weights = ~weight)
                        
table_births <- svytotal( ~ v013 , dhs_cDesign) %>% 
                data.frame () %>% 
                rownames_to_column("age_group") %>%
                #add_rownames("age_group")
                mutate (age_group = str_sub(age_group, -5, -1),
                        births = round (total)) %>%
                select ("age_group", "births")

```


```{r}

WB_fertility     <- WB_data_edited %>% 
                  filter (year == 2018) %>% 
                  pivot_wider(names_from = Sex, 
                              values_from = population) %>% 
                  left_join (table_births,
                            by = c("Age" = "age_group")) %>% 
                  rename (female = ` female`,
                          male = ` male`) %>% 
                  mutate (asfr = births/female,
                          sexRatio = male/female,
                          female_births = round(births/(1+sexRatio)),
                          fasfr = female_births/female)
                              
rates <- WB_fertility %>% 
          summarise(total_births = sum(births, na.rm = TRUE),
                    total_male = sum(male, na.rm = TRUE),
                    total_female = sum(female, na.rm = TRUE),
                    total_fbirths = sum(female_births, na.rm = TRUE),
                    CBR = total_births/(total_female+total_male),
                    GFR = total_births/sum(WB_fertility[4:10, 3]),
                    GRR = total_fbirths/sum(WB_fertility[4:10, 3]),
                    TFR = 5 * sum(asfr, na.rm = TRUE)) %>% 
          select("CBR", "GFR", "GRR", "TFR")

  print(paste0("There are about: ", round(rates$CBR*1000), " live births per 1000 persons in this sample"))

  print(paste0("There are about: ", round(rates$GRR*1000), " female births per 1000 women of reproductive ages in this sample"))
  
  print(paste0("A total of ", round(rates$TFR), " children that would be born to each woman if she were to live to the end of her child-bearing years and give birth to children in alignment with the prevailing age-specific fertility rates."))

```

## Standardization
