library(tidyverse)
library(tidytext)

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")



horror_movies %>% 
  separate()