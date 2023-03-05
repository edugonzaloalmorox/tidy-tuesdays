library(tidyverse)
library(ggthemes)
library(showtext)
library(plotly)

simpsons <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv")

simpsons


# data prep -------------------

most_appearences =   simpsons %>%
  count(guest_star, sort = TRUE) %>%
  top_n(25, n)

most_appearences
 
 appearences = simpsons %>%
   mutate(season = str_pad(season, 2, "left", pad = "0")) %>%
   group_by(guest_star, season) %>%
   tally() %>%
   arrange(season, guest_star, desc(n)) %>%
   rename(appearences_season = n)
 
 appearences

   appearences_ext = simpsons %>%
   mutate(season = str_pad(season, 2, "left", pad = "0")) %>%
   group_by(guest_star, season) %>%
   summarise(unique_roles = n_distinct(role)) %>%
     arrange(season, guest_star) %>%
     right_join(., appearences, by = c("guest_star", "season")) %>%
     arrange(season, guest_star)
 

appearences_ext

# plot  ----------------
 
   g_simpsons = appearences_ext %>%
   filter(guest_star %in% unique(most_appearences$guest_star), season != "Movie") %>%
   ggplot(aes(season, guest_star, color = unique_roles)) +
   geom_point(aes(size = appearences_season)) + 
   scale_colour_viridis_c(option = "viridis", begin = 0, end = 1, direction = 1) + 
   labs(title = "Frequent cameos in The Simpsons", 
        colour = "# roles in \nthe season", 
        y = "", 
        x = "", 
        caption = "Source: Wikipedia | @EdudinGonzalo") +
   theme_minimal() +
   theme(legend.position = "right",
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), 
         text=element_text(family="Comic Sans MS"),
         plot.title = element_text(color = "dodgerblue3", size = 15, face = "bold"),
         plot.background=element_rect(fill="floralwhite"),
         panel.background = element_rect(fill = "lightgoldenrodyellow",
                                         size = 0.002, linetype = "solid")) + 
   guides(size = guide_legend(title.position="top", title.hjust = 1), 
          colour = guide_colourbar(title.position="top", title.hjust = 0.5)) 
  

# interaction --------------------
 
 plotly_simpsons = ggplotly(g_simpsons)
 
 
 plotly_simpsons

# save -------------------------------
 
 htmlwidgets::saveWidget(plotly_simpsons, "simpsons.html") 
 


