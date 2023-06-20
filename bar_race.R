install.packages('tidyverse')
install.packages('rvest')
install.packages('lubridate')
install.packages('readxl')
install.packages('devtools')
install.packages('rworldmap')
install.packages('countrycode')

devtools::install_github("rensa/ggflags")
devtools::install_github("thomasp85/scico")



library(tidyverse)
library(readxl)
library(ggflags)
library(countrycode)
library(tidytext)
library(scico)

df = read_excel('/Users/personas/Downloads/prc_ppp_ind_page_spreadsheet.xlsx', sheet = 3, range = 'A9:Q52' )


df_long = df |>
  filter(TIME != 'GEO (Labels)') |>
  rename(country = TIME) |>
  gather('year', 'ppp', -country) |>
  mutate(country = ifelse(country == 'Germany (until 1990 former territory of the FRG)', 
                          'Germany', 
                          ifelse(country == 'European Union - 27 countries (from 2020)', 'Europe', country)))


countries = unique(df_long$country)
flags = countrycode(countries, 'country.name', 'unicode.symbol')


# English to ISO


df_flags = tibble(countries, flags) |> 
  filter(!is.na(flags)) |>
  mutate(iso2 = countrycode(countries, origin = 'country.name', destination = 'iso2c') )

df_clean = df_flags |>
  left_join(df_long, by = c('countries' = 'country')) |>
  filter(ppp != ':' ,
         !countries %in% c('United States',
                                         'Japan', 
                                         'Bosnia and Herzegovina',
                                         'North Macedonia', 'Montenegro', 'Albania', 'Türkiye', 'Serbia'), 
         !is.na(iso2)) |>
  mutate(ppp = as.numeric(ppp)) |>
  group_by(year) |>
  top_n(25) |>
  arrange(year, -ppp) |>
  mutate(rank_ = row_number(), year = as.numeric(year) ) |>
  ungroup() |>
  arrange(year) |>
  mutate(ppp = round(ppp, 0))

p.static = ggplot(df_clean, aes(rank_, group = countries
                  )) +
  geom_tile(aes(y = ppp/2,
                height = ppp,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(countries, " ")), vjust = 0.2, hjust = 1, size = 6) +
  geom_text(aes(y = round(ppp, 0), label = round(ppp,0), hjust = 0), size = 6) +
  ggflags::geom_flag(aes(y =0, country = str_to_lower(iso2)), size = 10) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(size = 0.1, color = "grey"),
        panel.grid.minor.x = element_line(size = 0.1, color = "grey"),
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size=20, face = "italic", color = "grey",  hjust = 0.5, vjust = -1),
        plot.background = element_blank(),
        plot.margin = margin(4, 4, 4, 4, "cm")) +
  labs(title = 'Purchasing power parities in Europe: {closest_state}',
       subtitle = 'Source: Eurostat', 
       caption = '@EdudinGonzalo')

  





anim_p = p.static + 
  transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)


animate(anim_p, 200, fps = 20, , width = 1200, height = 1500, renderer = gifski_renderer("Downloads/gganim_pv_v2.gif"))


