##########################
# Tidytuesday 
# Week: 19/08/2019
# @ EdudinGonzalo
##########################


library(tidyverse)
library(maps)
library(extrafont)

nuclear_explosions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-20/nuclear_explosions.csv")


top_types = nuclear_explosions %>%
  count(type, sort = TRUE) %>%
  top_n(5, n)

labels <- c(
  USSR = "<img src='https://cdn.britannica.com/36/22536-004-9855C103.jpg'
    width='50' /><br>",
  USA = "<img src='https://cdn.countryflags.com/thumbs/united-states-of-america/flag-800.png'
    width='50' /><br>",
  UK = "<img src='https://cdn.countryflags.com/thumbs/united-kingdom/flag-800.png'
    width='50' /><br>",
  FRANCE =  "<img src='https://cdn.countryflags.com/thumbs/france/flag-800.png'
    width='50' /><br>", 
  INDIA =  "<img src='https://cdn.countryflags.com/thumbs/india/flag-800.png'
    width='50' /><br>",
  CHINA =  "<img src='https://cdn.countryflags.com/thumbs/china/flag-800.png'
    width='50' /><br>" )

nuclear_explosions %>%
  filter(type %in% unique(top_types$type)) %>%
  mutate(kilotons = (yield_lower + yield_upper)/2, 
         decile_kt = ntile(kilotons, 10)) %>%
  ggplot(aes(year, country, colour = type)) + 
  geom_jitter(aes(size = kilotons), alpha = 0.2) + 
  labs(title = "Top 5 types of atomic bombs",
       x= "", 
       size = "Average KT") + 
  theme_minimal() +
  scale_y_discrete(name = NULL,
                   labels = labels) + 
  theme(axis.text.y = element_markdown(color = "black", size = 11), 
        legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(family = "Avenir", size = 36, margin = margin(0, 0, 10, 0)),
        plot.subtitle = element_text(size = 24, margin = margin(0, 0, 30, 0)),
        plot.caption = element_text(family = "Avenir", margin = margin(30, 0, 0, 0))) + 
  guides( size = guide_legend(title.position="top", title.hjust = 1),
          fill = guide_legend(title.position="top", title.hjust = 1,
                              nrow = 1, byrow=FALSE))

