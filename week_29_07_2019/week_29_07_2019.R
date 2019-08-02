##########################
# Tidytuesday 
# Week: 29/07/2019
# @ EdudinGonzalo
##########################

library(tidyverse)
library(gganimate)
library(anytime)

video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")


# get the dates

video_games = video_games %>%
  separate(release_date, c("month", "day", "year"), sep = " ") %>%
  mutate(month_release = anydate(month), 
         month_release = lubridate::month(month_release),
         month_release = str_pad(month_release, 2, side = "left", 0),
         day = gsub(day, ",", ""), 
         date_release = paste(year, month_release, sep = "-")) %>%
  select(-month_release, -day)


# game publishers
game_publishers = video_games  %>% 
      count(publisher, sort = TRUE) %>%
  filter(!is.na(publisher))


# -------------------------------------------

video_games$year = as.numeric(video_games$year)


top_6 = video_games %>%
  filter(publisher %in% c("Big Fish Games", "SEGA", "Strategy First", "Ubisoft", "Square Enix", "Sekai Project")) %>%
  count(year, publisher) %>%
  filter(!is.na(year)) %>%
  ggplot(aes(year, n, group = publisher, colour = publisher)) + 
  geom_line() + 
  labs(title = 'Top 5 video game publishers over time',
       subtitle = "By number of video games released in 2004-2018",
       y = 'Number of video games released', 
       x = "",
       caption = "@EdudinGonzalo | Source: Steam Spy") + 
  theme_minimal() + 
  facet_wrap(.~publisher, scales = "free_y") + 
  transition_reveal(year) +
  theme(legend.position = "none",
        plot.title = element_text(size=20))
  
  
anim_save("./week_29_07_2019/output/video_games.gif", top_6)








