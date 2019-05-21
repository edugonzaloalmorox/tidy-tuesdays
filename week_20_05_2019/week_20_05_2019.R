##########################
# Tidytuesday 
# Week: 20/05/2019
# @ EdudinGonzalo
##########################

library(tidyverse)

coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")

mismanaged_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-mismanaged-plastic-waste-vs-gdp-per-capita.csv")
                                     
waste_vs_gdp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/per-capita-plastic-waste-vs-gdp-per-capita.csv")


waste = waste_vs_gdp %>%
  janitor::clean_names()%>%
  rename(plastic_waste = per_capita_plastic_waste_kilograms_per_person_per_day,
         gdp = gdp_per_capita_ppp_constant_2011_international_constant_2011_international,
         population = total_population_gapminder) %>%
  filter(year == 2010)



mismanaged =  mismanaged_vs_gdp %>%
  janitor::clean_names()%>%
  rename(mismanaged_pc_plastic = per_capita_mismanaged_plastic_waste_kilograms_per_person_per_day,
         gdp = gdp_per_capita_ppp_constant_2011_international_rate,
         population = total_population_gapminder) %>%
  filter(year == 2010)


coast =  coast_vs_waste %>%
  janitor::clean_names() %>%
  rename(mismanaged_plastic_tonnes= mismanaged_plastic_waste_tonnes,
         population = total_population_gapminder) %>%
  filter(year == 2010)


complete_data = left_join(waste, mismanaged, by = c("entity", "code", "year", "gdp", "population"))
  

continent = codelist_panel %>% select(iso3c, continent) %>% unique()

complete_data = left_join(complete_data, continent, by = c("code" = "iso3c"))

waste_plastic = complete_data %>%
  filter(!is.na(plastic_waste), !is.na(code), !is.na(continent)) %>%
  group_by(entity) %>%
  ungroup() %>%
  ggplot(aes(gdp, plastic_waste)) + 
  geom_point(aes(size = population, color  = continent), alpha = .65 ) +
  geom_text(aes(label = entity), vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10() + 
  scale_y_log10() + 
  scale_size_continuous(labels = scales::comma_format(), range = c(.5,15)) + 
  labs(y = "Kg per person/day", 
       x  = "GDP per capita (log)", 
       title = "Plastic waste per capita and GDP", 
       caption = "Note: Data on plastic waste are referred to 2010. \n GDP per capita constant 2011 international $ \n
       @EdudinGonzalo | Source: Our World in Data") + 
  theme_minimal(16, "Avenir") +
  theme(plot.background=element_rect(fill="floralwhite"),
        panel.background = element_rect(fill = "seashell2",
                                        size = 0.002, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 

ggsave("week_20_05_2019/output/plastic_waste.png", waste_plastic)


mismanagement = complete_data %>%
  filter(!is.na(mismanaged_pc_plastic), !is.na(code), !is.na(continent)) %>%
  group_by(entity) %>%
  ungroup() %>%
  ggplot(aes(gdp, mismanaged_pc_plastic)) + 
  geom_point(aes(size = population, color  = continent), alpha = .65 ) +
  geom_text(aes(label = entity), vjust = 1, hjust = 1, check_overlap = TRUE) +
  scale_x_log10() + 
  scale_y_log10() + 
  scale_size_continuous(labels = scales::comma_format(), range = c(.5,15)) + 
  labs(y = "Kg per person/day", 
       x  = "GDP per capita (log)", 
       title = "Mismananged plastic waste per capita and GDP", 
       caption = "Note: Data on plastic waste are referred to 2010. \n GDP per capita constant 2011 international $ \n
       @EdudinGonzalo | Source: Our World in Data") + 
  theme_minimal(16, "Avenir") +
  theme(plot.background=element_rect(fill="floralwhite"),
        panel.background = element_rect(fill = "seashell2",
                                        size = 0.002, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
  

ggsave("week_20_05_2019/output/plastic_mismanagement.png", mismanagement)




  
