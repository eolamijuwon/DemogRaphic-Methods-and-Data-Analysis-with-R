---
Title: "Inferential Statistics and Publication Ready Tables in R"

Date: "11 February 2020"
---


#



<img align="right" src="wildlifeStrikes.png">


Visualizing top internation migration flows to South Africa using data from the 2016 community survey. Map and chart can be reproduced using the R code available [here](https://github.com/eolamijuwon/TidyTuesday/blob/master/migration_flows_ZA/Migration_flows_ZA.R)

<img align="center" src="_ImmigrationFlows_ZA.PNG">


<img align="center" src="MigrationFlows.PNG"> 

Visualizing internal migration flows in South Africa using data from the 2016 community survey and `chorddiag` package with code adapted from [Data-to-Viz](https://www.data-to-viz.com/graph/chord.html). A reproducible code for the chart is available [here](https://github.com/eolamijuwon/TidyTuesday/blob/master/migration_flows_ZA/Migration_flows_ZA.R)







# Recap

- Install packages **(when required)**

- Load libraries at every fresh start

- Import dataset(s)

- View data structure

- Clean/manage dataset

- Descriptive statistics (comples surveys)


# Introduction to Data visualization in R

This chapter will teach you how to visualize your data using ggplot2. R has several systems for making graphs, but ggplot2 is one of the most elegant and most versatile. ggplot2 implements the grammar of graphics, a coherent system for describing and building graphs. With ggplot2, you can do more faster by learning one system and applying it in many places.

If you’d like to learn more about the theoretical underpinnings of ggplot2 before you start, I’d recommend reading “A Layered Grammar of Graphics”.

Prerequisites
This chapter focuses on ggplot2, one of the core members of the tidyverse. To access the datasets, help pages, and functions that we will use in this chapter, load the tidyverse by running this code:





# Impressive data visualization showcases [{tidytuesday}](https://nsgrantham.shinyapps.io/tidytuesdayrocks/), [{R-Graph Gallery}](https://www.r-graph-gallery.com/index.html)

<img align="center" src="tidytuesday.gif"> 


# `ggplot2` Basics



There are three main plotting systems in R, the base plotting system `plot`, the `lattice` package, and the `ggplot2` package.

Here we will introduce the ggplot2 package, which has recently soared in popularity. ggplot allows you to create graphs for univariate and multivariate numerical and categorical data in a straightforward manner. It also allows for easy grouping and conditioning. It can generate complex plots create high quality graphics for publication.



A Graphing Template
Let’s turn this code into a reusable template for making graphs with ggplot2. To make a graph, replace the bracketed sections in the following code with a dataset, a geom function, or a collection of mappings:





```{r}

library(<tidyverse>ggplot2)

ggplot(data = <DATA>) +
  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>)) +
    <THEME_FUNCTION>() 

```
The rest of this chapter will show you how to complete and extend this template to make different types of graphs. We will begin with the <MAPPINGS> component.


### <DATA>


```{}

library (readxl)

library (tidyverse)

library (stringr)


## DATA I - Population Distribution Data from the Worldbank Data Bank
##          https://databank.worldbank.org/

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



WB_data_edited <- WB_data_edited %>%
                  pivot_wider(names_from = Sex, values_from = population) %>% 
                  mutate    (year = as.numeric(year)) %>% 
                  rename    (male = " male",
                            female = " female") %>% 
                  group_by  (year) %>% 
                  mutate    (perc_male = (male/(sum(male) + sum(female)))*100) %>% 
                  mutate    (perc_female = (female/(sum(male) + sum(female)))*100) %>% 
                  ungroup   ()
                  



## DATA II   -   Fertility and Life Expectancy Data from the Worldbank Data Bank

Region_filter <- read_xls("./2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/TFR_Africa.xls",
                          sheet = "Metadata - Countries") %>% 
                 filter(Region == "Sub-Saharan Africa")

TFR_data      <- read_xls("./2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/TFR_Africa.xls",
                          sheet = "Data") %>% 
                 filter (`Country Code` %in% Region_filter$`Country Code`) %>% 
                 select(-c("Indicator Name",	"Indicator Code")) %>% 
                 gather(key = "Year", value = "TFR",
                         `1960`:`2018`) %>% 
                 filter(is.na(TFR) == FALSE)

LE_data       <- read_xls("./2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/Life Expectancy_Africa.xls",
                          sheet = "Data") %>% 
                 filter(`Country Code` %in% Region_filter$`Country Code`) %>% 
                 select(-c("Indicator Name",	"Indicator Code")) %>% 
                 gather(key = "Year", value = "LE",
                        `1960`:`2018`) %>% 
                 filter(is.na(LE) == FALSE)


region_codes  <- read.csv("./2020/Workshops/DemogRaphic Research and Data Analysis/Data - Misc/region_codes.csv") %>% 
                 filter(sub.region == "Sub-Saharan Africa") %>% 
                 mutate(`Country Code` = alpha.3) %>% 
                 select(`Country Code`, intermediate.region)
  



merged_data <- inner_join(y = LE_data,
                          x = TFR_data,
                          by = c("Country Name", 
                                 "Country Code",
                                 "Year")) %>% 
               left_join(y = region_codes,
                         by = "Country Code") %>% 
               mutate(Year = as.numeric(Year)) %>% 
               filter(is.na(intermediate.region) == FALSE)
                          
```

### `geom_ ()`

Please see the `ggplot2` reference [website](https://ggplot2.tidyverse.org/reference/) for a full list of geoms_ and functions

-   `geom_bar`, `geom_col` for bar charts

-   `geom_boxplot()` for plotting a box and whiskers plot

-   `geom_dotplot()` for making dot plots

-   `geom_freqpoly()`, `geom_histogram()` for plotting histograms and frequency polygons

-   `geom_line()`, `geom_path()`, `geom_step()` for connecting observations

-   `geom_point()` for plotting points


### Mappings `aes ()`


```{r}	

WB_data_edited %>% filter (year == 2006) %>% 
                   ggplot (aes(x = Age, y = female)) + 
                   geom_col () +
                   coord_flip()


WB_data_edited %>% filter (year == 2006) %>% 
                   ggplot (aes(x = Age)) + 
                   geom_col (aes(y = -male)) +
                   coord_flip() 

                   
                   
WB_data_edited %>% filter (year == 2006) %>% 
                   ggplot () + 
                   geom_col (aes(x = Age, y = -perc_male), fill = "blue") +
                   geom_col (aes(x = Age, y = perc_female), fill = "orange") +
                   coord_flip() +
                   labs (x = "Age Groups (in years)",
                         y = "Percentage Distribution (%)") +
                   scale_y_continuous(breaks = seq(-10, 10, 2),
                                      labels = seq(-10, 10, 2) %>% abs %>% paste0 ("%"))

```


### `theme()`


```{r}

gg_plot <-  WB_data_edited %>% filter (year == 2006) %>% 
            ggplot () + 
            geom_col (aes(x = Age, y = -perc_male), fill = "#2863a6") +
            geom_col (aes(x = Age, y = perc_female), fill = "orange") +
            coord_flip() +
            labs (x = "Age Groups (in years)",
                         y = "Percentage Distribution",
                         title = "NG Population Distribution, 2006") +
            scale_y_continuous(breaks = seq(-10, 10, 2),
                               labels = seq(-10, 10, 2) %>% 
                               abs %>% paste0 ("%"))
                               


gg_plot + theme_minimal(base_family =  "mono")

gg_plot + theme_bw()

```                         



```{r}

## Pop Distribution, 2006

popDistr_2006 <-   WB_data_edited %>% filter (year == 2006) %>% 
                   ggplot () + 
                   geom_col (aes(x = Age, y = -perc_male), fill = "#2863a6") +
                   geom_col (aes(x = Age, y = perc_female), fill = "orange") +
                   coord_flip() +
                   labs (x = "Age Groups (in years)",
                         y = "Percentage Distribution",
                         title = "NG Population Distribution, 2006 - theme_gray") +
                   theme_gray (base_family =  "mono") +
                   theme (axis.title.x = element_text(size = 9.5, 
                                                      margin = unit(c(0.25, 0, 0, 0), "cm")),
                          axis.title.y = element_text(size = 9.5, 
                                                      margin = unit(c(0, 0.25, 0, 0), "cm")),
                          plot.title = element_text(size = 11.5, face = "bold"),
                          axis.text.x = element_text(size = 9),
                          axis.text.y = element_text(size = 9)) +
                    scale_y_continuous(breaks = seq(-10, 10, 2),
                               labels = seq(-10, 10, 2) %>% 
                               abs %>% paste0 ("%"))

## Pop Distribution, 2010

popDistr_2010 <-   WB_data_edited %>% filter (year == 2010) %>% 
                   ggplot () + 
                   geom_col (aes(x = Age, y = -perc_male), fill = "#2863a6") +
                   geom_col (aes(x = Age, y = perc_female), fill = "orange") +
                   coord_flip() +
                   labs (x = "Age Groups (in years)",
                         y = "Percentage Distribution",
                         title = "NG Population Distribution, 2010 - theme_classic") +
                   theme_classic () +
                   theme (axis.title.x = element_text(size = 9.5, 
                                                      margin = unit(c(0.25, 0, 0, 0), "cm")),
                          axis.title.y = element_text(size = 9.5, 
                                                      margin = unit(c(0, 0.25, 0, 0), "cm")),
                          plot.title = element_text(size = 11.5, face = "bold"),
                          axis.text.x = element_text(size = 9),
                          axis.text.y = element_text(size = 9)) +
                    scale_y_continuous(breaks = seq(-10, 10, 2),
                               labels = seq(-10, 10, 2) %>% 
                               abs %>% paste0 ("%"))

## Pop Distribution, 2014

popDistr_2014 <-   WB_data_edited %>% filter (year == 2014) %>% 
                   ggplot () + 
                   geom_col (aes(x = Age, y = -perc_male), fill = "#2863a6") +
                   geom_col (aes(x = Age, y = perc_female), fill = "orange") +
                   coord_flip() +
                   labs (x = "Age Groups (in years)",
                         y = "Percentage Distribution",
                         title = "NG Population Distribution, 2014 - theme_minimal") +
                   theme_minimal (base_family =  "mono") +
                   theme (axis.title.x = element_text(size = 9.5, 
                                                      margin = unit(c(0.25, 0, 0, 0), "cm")),
                          axis.title.y = element_text(size = 9.5, 
                                                      margin = unit(c(0, 0.25, 0, 0), "cm")),
                          plot.title = element_text(size = 11.5, face = "bold"),
                          axis.text.x = element_text(size = 9),
                          axis.text.y = element_text(size = 9)) +
                    scale_y_continuous(breaks = seq(-10, 10, 2),
                               labels = seq(-10, 10, 2) %>% 
                               abs %>% paste0 ("%"))

## Pop Distribution, 2018

popDistr_2018 <-   WB_data_edited %>% filter (year == 2018) %>% 
                   ggplot (aes(x = Age)) + 
                   geom_col (aes(y = -perc_male), fill = "#2863a6") +
                   geom_col (aes(y = perc_female), fill = "orange") +
                   coord_flip() +
                   labs (x = "Age Groups (in years)",
                         y = "Percentage Distribution",
                         title = "NG Population Distribution, 2018 - theme_bw") +
                   theme_bw () +
                   theme (axis.title.x = element_text(size = 9.5, 
                                                      margin = unit(c(0.25, 0, 0, 0), "cm")),
                          axis.title.y = element_text(size = 9.5, 
                                                      margin = unit(c(0, 0.25, 0, 0), "cm")),
                          plot.title = element_text(size = 11.5, face = "bold"),
                          axis.text.x = element_text(size = 9),
                          axis.text.y = element_text(size = 9)) +
                   scale_y_continuous(breaks = seq(-10, 10, 2),
                               labels = seq(-10, 10, 2) %>% 
                               abs %>% paste0 ("%"))
                               


library (ggpubr)

ggarrange(popDistr_2006,
          popDistr_2010,
          popDistr_2014,
          popDistr_2018,
          ncol = 2, nrow = 2, 
          align = "v")

```

<img align="right" src="Pop Pyramid.jpg">





# `ggplot2` Extras - [Extensions](http://www.ggplot2-exts.org/gallery/)

```{r}

library (ggthemes)

gg_plot + theme_light()

gg_plot + theme_excel()

gg_plot + theme_few()

gg_plot + theme_economist()

gg_plot + theme_wsj()

gg_plot + theme_fivethirtyeight()

gg_plot + theme_solarized(base_family =  "mono")

gg_plot + theme_dark()

```

```{r}

  
library (gganimate)

animat <- merged_data %>% 
          ggplot(mapping = aes(x = LE,
                               y = TFR,
                               color = intermediate.region)) +
                  geom_point(size = 6, alpha = 0.7) + 
                  scale_colour_brewer(palette = "Set1") +
                  theme_solarized(base_family =  "mono") + 
                  geom_text(vjust = 0, nudge_y = 0.25,
                            aes(label = `Country Name`),
                            size = 3.5) +
                theme (plot.title = element_text(size = 13, colour = "#DC143C", 
                                                 face = "bold",
                                                 lineheight=1.3),
                       plot.subtitle = element_text(size = 11.5),
                       axis.title.y = element_text(margin = margin(t = 0, r = 9, b = 0, l = 0)),
                       axis.title.x = element_text(margin = margin(t = 7, r = 0, b = 0, l = 0)),
                       legend.position = "bottom") + 
                labs(y = "Total Fertility Rate (TFR)",
                     x = "Life Expectancy (Years)",
                     color = "Sub-region",
                     title = "Relationship between Total Fertility Rate and \nLife Expectancy in sub-Saharan African Countries?",
                     subtitle = "Year: {frame_time}",
                     caption = "Source: WorldBank Open Data \n\nEmmanuel Olamijuwon (twitter.com/eolamijuwon)") +
              shadow_mark(alpha = 0.2, size = 0.8) + 
              transition_time(as.integer(Year)) 



animate(animat, fps = 6, duration = 30, end_pause = 10,
        height=550, width=900,
        res = 80, rewind = FALSE,
        gifski_renderer("FertilityLE.gif"))
  
  
    anim_save("./2020/Workshops/DemogRaphic Research and Data Analysis/Day 3 - Data Visualization/FertilityLE.gif")

```

<img align="right" src="FertilityLE.gif">


# Save/Export plots

```{r}

## Save GGPlot2
         
ggsave ("./2020/Workshops/DemogRaphic Research and Data Analysis/Day 3 - Data Visualization/Pop Pyramid.jpg", dpi = 300, height = 9, width = 11)


## Save Animation

anim_save("./2020/Workshops/DemogRaphic Research and Data Analysis/Day 3 - Data Visualization/FertilityLE.gif")


```

# Lab Exercises - Group Project

Using the Adolescent girls and young women dataset. Create a very appealing visualization that also tells a story. Be prepared to share with the larger group what your team has done. 
