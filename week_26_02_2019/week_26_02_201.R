##########################
# Tidytuesday 
# Week: 26/02/2019
# @ EdudinGonzalo
##########################




library(tidyverse)
library(readr)
library(janitor)
library(hrbrthemes)


full_train = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-26/full_trains.csv")


line_departures = full_train %>%
  tabyl(departure_station, year)  %>%
  gather(year, lines, -departure_station) %>%
  arrange(departure_station)

traffic =  full_train %>%
  group_by(departure_station, year) %>%
  summarise(trains = sum(total_num_trips))



traffic$year = as.character(traffic$year)

complete_data = line_departures %>%
  left_join(., traffic, by = c("departure_station", "year"))


sncf_plot = complete_data %>%
   filter(!departure_station %in% c("PARIS LYON","PARIS MONTPARNASSE"), year %in% c("2015", "2018")) %>%
  ggplot(., aes(lines, str_to_title(departure_station))) +
  geom_line(aes(group = departure_station), color = 'grey50', alpha = 0.5) +
  geom_point(aes(color = year, size = trains), alpha = 0.875) +
  scale_colour_viridis_d(name ="Year", guide = guide_legend(title.position = "top", nrow = 1, title.hjust = 0.5, option = "cividis")) +
  scale_x_continuous(limits = c(0, 80), 
                     breaks = seq(0, 80, by = 5)) +
  labs(y= "", x= "Number of lines", size="Traffic (number of trains)",
       title =  "How many lines & trains from each destination?", 
       subtitle = "Most stations have reduced the number of lines since 2015",
       caption =  "Paris Lyon and Paris Montparnasse excluded from the analysis \n Source: SNCF \n @EdudinGonzalo") +
  theme_ipsum(base_size = 6.5) + 
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
       legend.position = "bottom",
        legend.background = element_blank(),
        legend.direction="horizontal",
        text = element_text(family = "Helvetica")) +
 guides(size = guide_legend(title.position="top", title.hjust = 0.5))



ggsave("week_26_02_2019/sncf.png", sncf_plot)

