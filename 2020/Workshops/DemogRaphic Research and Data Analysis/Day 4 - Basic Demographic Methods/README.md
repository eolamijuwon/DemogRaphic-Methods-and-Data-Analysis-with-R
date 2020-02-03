---
Title: "Inferential Statistics and Publication Ready Tables in R"

Date: "11 February 2020"
---


#




# Basic Demographic Measures

```{r}

library (tidyverse)

library (readxl)
library (mosaic)

WB_PopData <- read_xlsx("./2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/NGA - Pop Distribution.xlsx")

WB_data_edited <- WB_PopData %>% 
                  select(-c("Country Name",
                            "Series Code",
                            "Country Code")) %>% 
                  separate(`Series Name`, 
                           c("Age", "Sex"),
                           fill = "right", sep = ",",) %>% 
                  filter (Sex != " female (% of female population)" &
                          Sex != " male (% of male population)" &
                          Sex != " total") %>% 
                  filter (Age != "Population ages 65 and above" &
                          Age != "Population ages 0-14" &
                          Age != "Population ages 15-64") %>% 
                  mutate (Age = str_sub(Age, -5, -1)) %>% 
                  mutate (Age = replace(Age, which(Age == "above"), "80+"))
                  
                  
WB_data_edited <- WB_data_edited %>% 
                  pivot_longer( cols = `2018 [YR2018]`:`2006 [YR2006]`,
                                names_to = "year", 
                                values_to = "population",
                                values_drop_na = TRUE) %>% 
                  mutate (year = str_sub(year, 1, 4))
                  
```


### Age Dependency Ratios

```{r}

WB_dependencyRatio <- WB_data_edited %>% 
                      mutate (age_single = str_sub(Age, -2, -1)) %>% 
                      mutate (age_single = replace(age_single, which(age_single == "0+"), 80)) %>% 
                      mutate (age_single = as.numeric(age_single)) %>% 
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

### Fertility Rates





```{r}

library (readstata13)
library (survey)
library(tibble)


DHS_children <- read.dta13("./2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/[Sample] Nigeria  - children.dta")

DHS_children <- DHS_children %>% 
                filter (b2 == 2018) %>% 
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

WB_PoPdata     <- WB_PopData %>% 
                  select(-c("Country Name",
                            "Series Code",
                            "Country Code")) %>% 
                  separate(`Series Name`, 
                           c("Age", "Sex"),
                           fill = "right", sep = ",",) %>% 
                  filter (Sex != " female (% of female population)" &
                          Sex != " male (% of male population)" &
                          Sex != " total") %>% 
                  filter (Age != "Population ages 65 and above" &
                          Age != "Population ages 0-14" &
                          Age != "Population ages 15-64") %>% 
                  mutate (Age = str_sub(Age, -5, -1)) %>% 
                  mutate (Age = replace(Age, which(Age == "above"), "80+"))
                  
                  
WB_PoPdata    <-  WB_PoPdata %>% 
                  select (c("Age", "Sex", "2018 [YR2018]")) %>% 
                  pivot_wider(names_from = Sex, 
                              values_from = `2018 [YR2018]`) %>% 
                  left_join (table_births,
                            by = c("Age" = "age_group")) %>% 
                  rename (female = ` female`,
                          male = ` male`) %>% 
                  mutate (asfr = births/female,
                          sexRatio = male/female,
                          female_births = round(births/(1+sexRatio)),
                          fasfr = female_births/female)
                              
rates <- WB_PoPdata %>% 
          summarise(total_births = sum(births, na.rm = TRUE),
                    total_male = sum(male, na.rm = TRUE),
                    total_female = sum(female, na.rm = TRUE),
                    total_fbirths = sum(female_births, na.rm = TRUE),
                    CBR = total_births/(total_female+total_male),
                    GFR = total_fbirths/sum(WB_PoPdata[4:10, 2]),
                    TFR = 5 *sum (asfr, na.rm = TRUE)) %>% 
          select("CBR", "GFR", "TFR")

  print(paste0("There are about: ", round(rates$CBR*1000), " live births per 1000 persons in this sample"))

  print(paste0("There are about: ", round(rates$GFR*1000), " female births per 1000 women of reproductive ages in this sample"))
  
  print(paste0("A total of ", round(rates$TFR), " children that would be born to each woman if she were to live to the end of her child-bearing years and give birth to children in alignment with the prevailing age-specific fertility rates."))

```

## Life Expectancy

## Migration Expectancy

## Standardization of Demographic Measures

-  Lab Exercises




