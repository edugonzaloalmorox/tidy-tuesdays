##########################
# Tidytuesday 
# Week: 06/03/2019
# @ EdudinGonzalo
##########################


library(tidyverse)
library(countrycode)
library(maps)
library(tmap)
library(rwlib)

df <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv") %>%
  select(-flag_codes, -flags) %>% 
  group_by(country_code, indicator) %>%
  arrange(country_code, edulit_ind, year)  %>%
  ungroup()



df = df %>%
  mutate(country =  recode(country, "Antigua and Barbuda" = "Antigua",                                 
                           "Bolivia (Plurinational State of)"  = "Bolivia",
                           "Brunei Darussalam" = "Brunei",                                   
                           "Côte d'Ivoire" = "Ivory Coast",                                       
                           "Congo"=  "Republic of Congo"  ,                                               
                           "Cabo Verde" = "Cape Verde",
                           "Lao People's Democratic Republic" = "Laos", 
                           "China, Macao Special Administrative Region"  = "Macao", 
                           "Czechia" = "Czech Republic" ,                                            
                           "Micronesia (Federated States of)" = "Micronesia",                   
                           "United Kingdom of Great Britain and Northern Ireland" = "UK" ,
                           "Iran (Islamic Republic of)"  = "Iran",                          
                           "Saint Kitts and Nevis"  = "Saint Kitts",                              
                           "Republic of Korea"  = "North Korea",                                  
                           "Republic of Moldova" =  "Moldova", 
                           "The former Yugoslav Republic of Macedonia" =  "Macedonia" ,          
                           "Democratic People's Republic of Korea" = "South Korea",               
                           "Russian Federation" = "Russia" ,                                 
                           "Syrian Arab Republic"  = "Syria" ,                              
                           "United Republic of Tanzania" = "Tanzania" ,                         
                           "United States of America" = "USA",                           
                           "Venezuela (Bolivarian Republic of)" =  "Venezuela" ,                  
                           "Viet Nam" =  "Vietnam"))

# keep countries only 

countries = df %>%
  filter(str_detect(country_code, "[:alpha:]"))


continents <- data.frame(country = countries$country) %>% unique()

continents$cont <- countrycode(sourcevar = continents[, "country"],
                               origin = "country.name",
                               destination = "continent")

countries = countries %>% left_join(., continents, by = "country")



# add further information



diff_ratios = countries %>%
  group_by(country, edulit_ind) %>%
  filter(row_number()==1 | row_number()==n()) %>%
  mutate(diff_stud_ratio = 100*((student_ratio - lag(student_ratio))/lag(student_ratio)), 
         number_years = year - lag(year)) %>%
  ungroup()


europe = diff_ratios %>%
  filter(!is.na(diff_stud_ratio), cont == "Europe") %>%
  group_by(edulit_ind) %>%
  arrange(edulit_ind, desc(diff_stud_ratio)) %>%
  slice(c(1:10, seq(n()- 10, n()))) %>%
  ungroup()



 europe %>%
  filter(indicator %in% c("Primary Education", "Secondary Education", "Tertiary Education")) %>%
  group_by(indicator) %>%
  arrange(indicator, desc(diff_stud_ratio)) %>%
  ungroup() %>%
  mutate(country = reorder_within(country, diff_stud_ratio, indicator)) %>%
  ggplot(., aes(country, diff_stud_ratio)) + 
  geom_bar(stat  = "identity", alpha = 0.65) +
  scale_x_reordered() +
  coord_flip()  +
  facet_wrap(. ~ indicator, scales = "free") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(y = "Variation student to teacher ratio (%)", 
       x  = "", 
       title = "Student to teacher ratio variation over time in European countries", 
       subtitle = "Negative values imply fewer students per ratio", 
       caption = "Note: Variation rate between first and last observed year for each country \n
       @EdudinGonzalo | Source: UNESCO") + 
  theme_minimal(12, "Avenir") +
  theme(plot.background=element_rect(fill="floralwhite"),
        panel.background = element_rect(fill = "seashell2",
                                        size = 0.002, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggpubr::font("title", size = 16, face = "bold")

