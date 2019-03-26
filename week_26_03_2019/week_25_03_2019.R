##########################
# Tidytuesday 
# Week: 25/03/2019
# @ EdudinGonzalo
##########################



library(tidyverse)
library(readr)
library(lubridate)
library(anytime)
library(ggpubr)



seattle_pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")

pets = seattle_pets %>%
  mutate(date_clean = anytime(license_issue_date)) %>%
  arrange(date_clean) %>%
 
  
  pets = pets %>%
        mutate(year = lubridate::year(date_clean),
         month = lubridate::month(date_clean, label = TRUE),
         day = lubridate::day(date_clean)) %>%
  select(date_clean, day, month, year,-license_issue_date, everything())

# most frequent names and  date of registration 

col1 = "#d8e1cf" 
col2 = "#438484"



common_names = pets %>%
  group_by(animals_name) %>%
  tally() %>%
  filter(!is.na(animals_name)) %>%
  arrange(desc(n)) %>%
 top_n(15, n)

common_complete = pets %>%
  filter(animals_name %in% unique(common_names$animals_name)) %>%
  group_by(animals_name, month) %>%
  tally() %>%
  arrange(month) %>%
  ggplot(aes(month, animals_name)) + 
  geom_tile(aes(fill = n), colour = "white", na.rm = TRUE) + 
  scale_fill_gradient(low = col1, high = col2, breaks = c(10,20,30,40)) + 
  theme_bw() + theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "bottom", legend.direction="horizontal",
        axis.text.x = element_text(angle = 0, hjust = 1)) +
  guides(fill=guide_legend(title="# Registrations", nrow=1)) +
  labs(title = "Monthly registration of top 15 pet names in Seattle (2003 - 2018)", x ="", y = "", caption = "Source: Seattle pet names | @EdudinGonzalo") + 
  font("title", size = 16, color = col2, face = "bold") 



    
  
  
  
