library(tidyverse)
library(countrycode)
library(lubridate)
library(viridis)

hotels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv')



# Data Prep ----------------------------

counts_events = hotels %>%
  mutate(month = lubridate::month(reservation_status_date, label = TRUE),
         year =  lubridate::year(reservation_status_date)) %>%
  count(country, month, year, reservation_status) %>%
  arrange(country, year, month)
  
 
dates = seq(ymd(min(hotels$reservation_status_date)),
    ymd(max(hotels$reservation_status_date)), 
    by = "month")


 
dates_df = data.frame(dates = rep(seq(ymd(min(hotels$reservation_status_date)),
                                      ymd(max(hotels$reservation_status_date)), 
                                      by = "month"),
                                  each = length(unique(hotels$country))*3),
                      countries = rep(unique(hotels$country), 
                                      by = length(dates)*3), 
                      reservation_status = rep(c("Check-Out", "Canceled", "No-Show"),
                                               by = 3)) %>%
  arrange(countries, dates) %>%
  mutate(month = lubridate::month(dates, label = TRUE),
         year =  lubridate::year(dates)) %>%
  select(-dates, country = countries) %>%
  left_join(., counts_events, by = c("country", "month", "year", "reservation_status")) %>%
  mutate(n = ifelse(is.na(n), 0, n)) %>%
  as.tibble()




# Plot ------------------------------------------------------------
dates_df %>% 
  filter(country %in% c("PRT", "GBR", "FRA", "ESP", "DEU"),
         !reservation_status %in% c("No-Show")) %>%
  ggplot(aes(x=month, y=country,fill= n))+
  geom_tile(color="white", size=0.2, alpha = 0.85) +
  scale_fill_viridis(option = "plasma") +

  labs(x="",y="")+
  facet_grid(year ~ reservation_status) +
  scale_y_discrete(expand=c(0,0)) +
  theme_minimal(base_size=8, "Monaco") +
  labs(title = "Hotel Check-Outs and Cancellations",
       caption = "@EdudinGonzalo / Source: Tidytuesday - Hotel Bookings") +
  theme(
    legend.text=element_text(face="bold"),
    axis.ticks=element_line(size=0.4),
    plot.background=element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.border=element_blank())
  
  
  
