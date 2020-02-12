


library (dplyr)
library (readstata13)
library (mosaic)
library (kableExtra)
library(compareGroups)
library(survey)
                          ## Please edit the directory to reflect where the dataset is saved on your PC.
agyw_dataset <- read.dta13("/2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/Nigeria [DHS].dta")
View(agyw_dataset)

agyw_clean <- agyw_dataset %>% 
              ## Filter for those who are not pregnant
              filter (v213 == "no or unsure") %>%
              ## SHIFT + CTRL + M for pipes
              ## Filter for those who are sexually active
              filter (v529 == 0) %>% 
              filter (v405 == "no") %>%
              ## rename region and residece
              rename(region = v024) %>% 
              rename (residence = v025) %>% 
              rename (psu = v001) %>% 
  
              mutate (weight = v005/10^6) %>% 
  
              mutate(mCuse = if_else((v364 != "using modern method"),
                                     "Not using Modern",
                                     "Using Modern"))  %>% 

              mutate (teen_educa = if_else((v106 == "no education" |
                                             v106 == "primary"),
                                          "< Secondary",
                                          "Secondary +")) %>% 
              mutate(religion = if_else((v130 == "catholic" | 
                                           v130 == "other christian"),
                                        "christians",
                                        if_else((v130 == "islam"), 
                                                "muslims", "others"))) %>% 
          select(region, mCuse, 
                 religion, teen_educa,
                 residence, weight, psu)

agyw_clean$strata <- do.call(paste, agyw_clean[, c("region", "residence")])

## Declare dataset in survey mode
dhs_design <- 
    svydesign( 
        ~ psu , 
        strata = ~strata , 
        data = agyw_clean , 
        weights = ~weight
    )

## Create a new object (data frame) with information on
## on weighted frequency and percentage distributions
wregion <- svytotal( ~ region , dhs_design) %>% 
            data.frame() %>% 
            mutate (perc = total/sum(total)) %>% 
            mutate (perc = perc * 100) %>% 
            mutate (perc = round(perc, digits = 2))

agyw_clean$mCuse <- as.factor(agyw_clean$mCuse)       

cuse_regress <- glm(mCuse ~ factor(region) + factor(residence),
                    data = agyw_clean, family = binomial(link = "cloglog"))

summary(cuse_regress)


wregion2 <- svytable ( ~ region, dhs_design, Ntotal=TRUE)



table_mCuse <- data.frame(table(agyw_clean$mCuse)) %>% 
            mutate (perc = Freq/sum(Freq)) %>% 
            mutate (perc = perc * 100) %>% 
            mutate (perc = round(perc, digits = 2))

table_religion <- data.frame(table(agyw_clean$religion)) %>% 
            mutate (perc = Freq/sum(Freq)) %>% 
            mutate (perc = perc * 100) %>% 
            mutate (perc = round(perc, digits = 2))

table_region <- data.frame(table(agyw_clean$region)) %>% 
            mutate (perc = Freq/sum(Freq)) %>% 
            mutate (perc = perc * 100) %>% 
            mutate (perc = round(perc, digits = 2))

table_residence <- data.frame(table(agyw_clean$residence)) %>% 
            mutate (perc = Freq/sum(Freq)) %>% 
            mutate (perc = perc * 100) %>% 
            mutate (perc = round(perc, digits = 2))

table_teen_educa <- data.frame(table(agyw_clean$teen_educa)) %>% 
            mutate (perc = Freq/sum(Freq)) %>% 
            mutate (perc = perc * 100) %>% 
            mutate (perc = round(perc, digits = 2))


univar_table <- rbind(table_mCuse, table_teen_educa,
                      table_region, table_religion,
                      table_residence) %>% 
                rename("Characteristics" = "Var1",
                       "Frequency" = "Freq",
                       "Percentages (%)" = "perc")

univar_table %>% 
  kable() %>% 
  kable_styling("striped", full_width = F) %>% 
  pack_rows("Modern Contraceptive Use", 1, 2) %>% 
  pack_rows("Education", 3, 4) %>% 
  pack_rows("Region", 5, 10) %>% 
  pack_rows("Religious Affiliation", 11, 13) %>% 
  pack_rows("Residence", 14, 15) %>% 
  footnote(general = "Here is a general comments of the table. ",
           symbol = c("< Secondary: Refers to AGYW with less than secondary education ",
                      "Secondary +: Refers to AGYW with Secondary or more education ")) %>% 
  save_kable(file = "C:/Users/eOlamijuwon/Desktop/descriptiveTable.html", self_contained = T)



table <- descrTable(agyw_clean)

## Export unweighted frequency table to html or docx
export2html(table, file = "descriptTable.html")
export2word(table, file = "descriptTable.docx")

## Create an unweighted crossTable
cross_tab <- descrTable(mCuse ~ teen_educa + region, agyw_clean)
cross_tab


export2word(cross_tab, "cross_tab.docx")
