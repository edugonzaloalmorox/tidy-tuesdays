##########################
# Tidytuesday 
# Week: 09/07/2019
# @ EdudinGonzalo
##########################



library(tidyverse)
library(tidytext)
library(ggpubr)


wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")



# teams 

check = squads %>%
  filter(club != "Unattached") %>%
  group_by(club, pos) %>%
  tally() %>%
  arrange(pos, desc(n))


teams = check %>%
  ungroup() %>%
  group_by(pos) %>%
  top_n(5, n)


  
  
plot = teams %>%
  ungroup %>%
  mutate(position = case_when(pos == "GK" ~ "Goalkepper", 
                              pos == "DF" ~ "Defender",
                              pos == "MF" ~ "Mildfield",
                              pos == "FW" ~ "Forward"),
         position =  ordered(position, c("Goalkepper", "Defender", "Mildfield", "Forward")),
         club = reorder_within(club, n, position)) %>%
  ggplot(aes(club, n, fill = position)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~position, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(y = "Number of players" ,
       x = NULL,
       title = "Which team provides more players for each position?",
       subtitle = "Top 5 teams with more players",
       caption = " @EdudinGonzalo | Source: Women's World Cup - data.world") + 
  theme_minimal(9, "Avenir") + 
  theme(plot.background= element_rect(fill="mintcream"),
        panel.background = element_rect(fill = "mintcream",
                                        size = 0.005, linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  font("title", size = 14, color = "deepskyblue4") +
  font("subtitle", size = 9, color = "darkcyan")
  

ggsave("week_09_07_2019/output/teams.png", plot,  width = 20, height = 12, units = "cm")
 
  




