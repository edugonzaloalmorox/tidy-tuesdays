################################
# TidyTuesday
# US PhD Awarded by Field
# 19/02/2019
# @EduGonzalo
################################



library(tidyverse)
library(gganimate)


grads = read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

check = grads %>%
  group_by(broad_field, year) %>%
  tally(n_phds) %>%
  mutate(lab_clean = case_when(broad_field == "Education" ~ "Edu.",
                               broad_field == "Humanities and arts" ~ "Hum.",
                               broad_field == "Mathematics and computer sciences" ~ "Sci.",
                               broad_field == "Engineering" ~ "Eng.",
                               broad_field == "Life sciences" ~ "Lif.",
                               broad_field == "Psychology and social sciences" ~ "Soc.", 
                               broad_field == "Other" ~ "Oth."))





plot = ggplot(check, aes(year, n, group = broad_field, colour = broad_field)) + 
  geom_line() + 
  geom_segment(aes(xend = 2017, yend = n), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  scale_x_continuous(breaks = c(2008:2017)) +
  geom_text(aes(x = 2017.5, label = lab_clean), hjust = 0) + 
  transition_reveal(year) + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'US PhDs Awarded by Board Field since 2008',
       y = 'Total number of PhDs awarded', 
       x = " ",
       caption = "@EdudinGonzalo") + 
  theme_minimal() + 
  theme(legend.position = "bottom", 
        plot.margin = margin(1.5, 1, 1, 1.5), 
        legend.title = element_blank())

anim_save(filename =  "tidytuesday.gif", animation = plot)