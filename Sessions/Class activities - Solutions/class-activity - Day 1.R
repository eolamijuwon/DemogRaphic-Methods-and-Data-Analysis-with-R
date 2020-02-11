install


library (dplyr)
library (readstata13)
library (mosaic)

agyw_dataset <- read.dta13("C:/Users/eOlamijuwon/OneDrive/Workshops_Seminars/2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/Nigeria [DHS].dta")
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
  
              mutate(mCuse = if_else((v364 != "using modern method"),
                                     "Not using Modern",
                                     "Using Modern"))  %>% 

              mutate (teen_educa = if_else((v106 == "no education" |
                                             v106 == "primary"),
                                          "< Secondary",
                                          "Secondary +")) %>% 
              mutate(religion = derivedFactor("Christian" = (v130 == "catholic" | v130 == "other christian"),
                                              "Muslim" = (v130 == "islam"),
                                              "Others" = (v130 == "traditionalist" | v130 == "other"),
                                              .default = NA)) %>% 
          select(region, mCuse, 
                 religion, teen_educa,
                 residence)



