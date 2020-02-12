---
Title: "Basic Demographic Methods"

Date: "13 February 2020"

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

# Standardization
Standardization has been a commonly used procedure when comparing rates or probabilities for groups with differences in composition. This procedure is thus used to avoid the confounding effect of the population structure.


```{r}

pop_distr <- WB_data_edited %>% 
              filter (year == 2014) %>% 
              group_by (Age) %>% 
              summarize(population = sum(population)) %>% 
              ungroup()
              
```

In the absence of data on death rates, 

```{r}

library(demography)

Username <- "emmanuel@olamijuwon.com"
Password <- "1563262569"
dta.canada <- hmd.mx("CAN", Username, Password)
dta.taiwan <- hmd.mx("TWN", Username, Password)


# Setup year and age
year <- "2014"
age <- dta.canada$age


canada_pop<-dta.canada$pop$total[,year]
canada_rate<-dta.canada$rate$total[,year]

taiwan_pop<-dta.taiwan$pop$total[,year]
taiwan_rate<-dta.taiwan$rate$total[,year]

canada_death <- canada_pop * canada_rate
taiwan_death <- taiwan_pop * taiwan_rate

```

#### Exercise
Find the crude death rates for Canada and Taiwan in 2014
*Hint:* CDR = $$\frac{Number of deaths in year (x)}{Total population in year (x)}$$



## Standardization of Demographic Measures

Standardization has been a commonly used procedure when comparing rates or probabilities for
groups with differences in composition. This procedure is thus used to avoid the confounding
effect of the population structure. Note that in the previous example, Canada has a higher crude death rate compared to Taiwan.

```{r}

canada_distr <- cbind(age, canada_pop, canada_death) %>% 
                data.frame() %>% 
                mutate (age_group = derivedFactor("00-04" = (age >= 0 & age < 5),
                                              "05-09" = (age >= 5 & age < 10),
                                              "10-14" = (age >= 10 & age < 15),
                                              "15-19" = (age >= 15 & age < 20),
                                              "20-24" = (age >= 20 & age < 25),
                                              "25-29" = (age >= 25 & age < 30),
                                              "30-34" = (age >= 30 & age < 35),
                                              "35-39" = (age >= 35 & age < 40),
                                              "40-44" = (age >= 40 & age < 45),
                                              "45-49" = (age >= 45 & age < 50),
                                              "50-54" = (age >= 50 & age < 55),
                                              "55-59" = (age >= 55 & age < 60),
                                              "60-64" = (age >= 60 & age < 65),
                                              "65-69" = (age >= 65 & age < 70),
                                              "70-74" = (age >= 70 & age < 75),
                                              "75-79" = (age >= 75 & age < 80),
                                              "80+" = (age >= 80))) %>% 
                group_by(age_group) %>% 
                summarise (canada_pop = sum(canada_pop),
                           canada_death = sum(canada_death)) %>% 
                ungroup () %>% 
                mutate (asdr = (canada_death/canada_pop) * 1000)
                
                
taiwan_distr <- cbind(age, taiwan_pop, taiwan_death) %>% 
                data.frame() %>% 
                mutate (age_group = derivedFactor("00-04" = (age >= 0 & age < 5),
                                              "05-09" = (age >= 5 & age < 10),
                                              "10-14" = (age >= 10 & age < 15),
                                              "15-19" = (age >= 15 & age < 20),
                                              "20-24" = (age >= 20 & age < 25),
                                              "25-29" = (age >= 25 & age < 30),
                                              "30-34" = (age >= 30 & age < 35),
                                              "35-39" = (age >= 35 & age < 40),
                                              "40-44" = (age >= 40 & age < 45),
                                              "45-49" = (age >= 45 & age < 50),
                                              "50-54" = (age >= 50 & age < 55),
                                              "55-59" = (age >= 55 & age < 60),
                                              "60-64" = (age >= 60 & age < 65),
                                              "65-69" = (age >= 65 & age < 70),
                                              "70-74" = (age >= 70 & age < 75),
                                              "75-79" = (age >= 75 & age < 80),
                                              "80+" = (age >= 80))) %>% 
                group_by(age_group) %>% 
                summarise (taiwan_pop = sum(taiwan_pop),
                           taiwan_death = sum(taiwan_death)) %>% 
                ungroup () %>% 
                mutate (asdr = (taiwan_death/taiwan_pop) * 1000)

```


### Direct standardization

The purpose of standardization is to facilitate comparison of rates over time (or across groups) by removing the effect of composition. The direct standardized rate for a given population combines the population’s group-specific rates 𝑚𝑚𝑖𝑖 with the composition of a standard population 𝑐𝑐𝑖𝑖
𝑆𝑆:


We interpret this rate as a counterfactual: the rate that would be observed if the population had the
standard composition but its own group-specific rates. Using a uniform distribution as the standard
composition yields the average rate. (A closely related measure is the total fertility rate, which has a
synthetic cohort interpretation.) A good choice of standard to compare two populations is the average
composition.

The direct standardization method consists in:
- Finding a standard structure (sA c ), e.g. an average structure between the population compared or the structure of one of these populations.

```{r}

CA_ageD <- canada_distr$canada_pop/sum(canada_distr$canada_pop)
TA_ageD <- taiwan_distr$taiwan_pop/sum(taiwan_distr$taiwan_pop)

standard_pop <- (CA_ageD + TA_ageD)/2


```

- Multiplying the component-specific rates (rc) of the studied population by the standard structure.

- The standardized crude rates are found by summing sA c rc


```{r}

sum(standard_pop * canada_distr$asdr)
sum(standard_pop * taiwan_distr$asdr)

```

#### Exercise
Compare the crude death obtained for Nigeria with the crude rate obtained for Canada. What is your conclusion?



Direct and indirect standardisation usually give similar results in practice. In general, direct standardisation is preferred to the indirect method. This is because, in direct standardisation, the age-specific rates of the study populations are applied to just one standard population i.e. the weights applied to the age-specific rates are the same. In indirect standardisation, the weights applied to the standard age-specific rates depend on the age structure of the study populations. If there are large differences in age structure between the study populations the SMRs calculated would be based on different weightings and not comparable.

A further advantage of direct standardisation is that, because the denominator used is the same, comparisons across multiple sets of data are transitive. Thus, if direct standardisation is applied to three sets of data (C, D and E), if the CMF between C and D is 2, and that between D and E is 0.5, this would imply that the CMF between C and E is 2/0.5 =4.

The choice of method may be affected by several other considerations, including:

Direct standardisation requires that we know the age-specific rates of mortality (or morbidity) in all the populations under study. Indirect standardisation only requires that we know the total number of deaths (or cases) and the age structure of the study population, and thus indirect standardisation may be the only feasible method if age-specific rates are not available.
Indirect standardisation is preferable when there are small numbers in particular age groups.If we undertook direct standardisation under these circumstances, the estimated rates would be subject to substantial sampling variation.

With indirect adjustments we can choose rates from a large population as standard, thus minimising the effects of sampling error.


### Inirect standardization

The indirect standardization is used to estimate what would be the crude rates if both populations
had the same component-specific rates. This method allows quantifying the effect of population
structure on mortality.
The method consists in:
• Finding a standard component-specific rates (rcA).

```{r}

# Step 1: Find the standard age-specific rates
# We here use the average

Std.ageR <- (canada_distr$asdr + taiwan_distr$asdr)/2

```
• Multiplying the population structures (sc) of the studied population by the standard
component-specific rates.

```{r}
# Step 2: Find the standardized CDR
T.CDR.Istd <- sum(CA_ageD * Std.ageR)
T.CDR.Istd

T.CDR.Istd <- sum(TA_ageD * Std.ageR)

```

• The standardized crude rates are found by summing scrcA