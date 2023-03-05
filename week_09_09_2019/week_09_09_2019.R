##########################
# Tidytuesday 
# Week: 09/09/2019
# @ EdudinGonzalo
##########################

library(tidyverse)
library(anytime)

tx_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv")

safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")

top_acc = safer_parks %>%
  mutate(date = anydate(acc_date),
        year = lubridate::year(date),
         week_day = lubridate::wday(date, label = TRUE)) %>%
  group_by(year) %>%
  count(injury_desc)  %>%
  top_n(5, n) %>%
  arrange(desc(year), desc(n))
