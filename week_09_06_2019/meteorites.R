##########################
# Tidytuesday 
# Week: 11/06/2019
# @ EdudinGonzalo
##########################


library(tidyverse)
library(ggthemes)
library(ggpubr)


meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

big_meteorites = meteorites %>% 
  mutate(decile = percent_rank(mass))  %>%
  filter(decile >= 0.75, year >= 1900 & year <= 2019) 


cols = c("Found" ="#a9c19b",
         "Fell" = "#c1b39b")

plot = ggplot() + 
  borders("world", colour = "#9ba9c1", fill = "#9ba9c1") +
  theme_map(12, "Avenir") +
  geom_point(data = big_meteorites,
             aes(x = long, y = lat, size = mass, colour = fall), alpha = .35) +
  scale_colour_manual(values = cols) +
  labs(title = "Where do meteorites land?",
       subtitle = "Higher proportion of found meteorites (green) vs fell (orange)\n specially in US, Chile, Oman, Morocco, Lybia and Australia", 
        caption = "Note: Sample considers 25% biggest meteorites by mass between 1900 and 2019. \n @EdudinGonzalo | NASA ") + 
  theme(legend.position = "none", 
        legend.spacing.x = unit(0.5, 'cm'),
        legend.text = element_text(margin = margin(t = 10)),
        plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5)) +
  font("title", size = 22, color = "#59616d") +
  font("subtitle", size = 8, color = "#8b96a8") +
  font("caption", size = 6,  color = "#8b96a8" ) +
  coord_fixed(ratio = 1.05)


ggsave("week_09_06_2019/output/meteorites.png", plot,  width = 15, height = 20, units = "cm")




