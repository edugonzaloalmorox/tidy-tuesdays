##########################
# Tidytuesday 
# Week: 06/04/2020
# @ EdudinGonzalo
##########################


library(tidyverse)
library(countrycode)
library(emo)

# Load tour data -------------------------------

devtools::install_github("thebioengineer/tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2020-04-07')

stage = tuesdata$stage_data %>% tibble()
stages = tuesdata$tdf_stages %>% tibble()
winners = tuesdata$tdf_winners %>% tibble()

# Drop NA
stages_clean = stages %>%
  filter(!is.na(Winner_Country))

# Get emojis ---------------------------------

emoji_to_link <- function(x) {
  paste0("https://emojipedia.org/emoji/",x) %>%
    read_html() %>%
    html_nodes("tr td a") %>%
    .[1] %>%
    html_attr("href") %>%
    paste0("https://emojipedia.org/", .) %>%
    read_html() %>%
    html_node('div[class="vendor-image"] img') %>%
    html_attr("src")
}

link_to_img <- function(x, size = 25) {
  paste0("<img src='", x, "' width='", size, "'/>")
}

emojis = emo::jis

# Get flags emojis ----------------------------
flag_emojis = emojis %>%
  filter(subgroup == "country-flag", name %in% unique(stages_clean_exp$win_country)) %>%
  select(emoji, name) %>%
  mutate(url = map_chr(emoji, slowly(~emoji_to_link(.x), rate_delay(1))),
          label = link_to_img(url))


# Countries standarisation  -----------------------

stages_clean_exp = countrycode::codelist %>%
  tibble() %>%
  select(ioc, country.name.en) %>%
  filter(ioc %in% unique(stages_clean$Winner_Country)) %>%
  right_join(., stages_clean, by = c("ioc" = "Winner_Country"))%>%
  rename(win_country = country.name.en)

# Link emojis and tour info -------------------------
stages_clean_exp = stages_clean_exp %>%
  left_join(., flag_emojis, by = c("win_country" = "name"))


# Analysis --------------------------------------
stages_clean_exp %>%
  janitor::clean_names() %>%
  mutate(type = str_to_title(type)) %>%
  group_by(country, win_country) %>%
  summarise(number_wins = n(), 
            total_distance = sum(distance))  



  
# Plot ---------------------------

plot_tour = stages_clean_exp %>%
  janitor::clean_names() %>%
  mutate(type = str_to_title(type)) %>%
  group_by(type, emoji, label, win_country) %>%
  summarise(number_wins = n(), 
            mean_distance = mean(distance))   %>%
filter(type %in% c("Mountain Stage",
                   "Individual Time Trial",
                   "Flat Stage",
                   "High Mountain Stage" )) %>%
  ggplot(aes( number_wins, mean_distance, label = label)) +
  geom_point() +
  geom_richtext(aes(y = mean_distance),
                fill = NA,
                label.color = NA, 
                label.padding = grid::unit(rep(0, 4), "pt"
                                           ) ) +
  geom_smooth(method='lm',
              size = 0.5,
              colour="#ffc108",
              size=0.3, 
              se = FALSE, 
              linetype = "dashed") +
  facet_wrap(~ type, scales = "free") +
  labs(title = "Winning Countries and Types of Stages in Le Tour de France", 
               x = "Number of wins", 
               y = "Mean Distance", 
        caption = "@EdudinGonzalo / Source: Tidytuesday - tdf (Alastair Rushworth)") +
  theme(text = element_text(family = "PT Sans Narrow Bold"),
        strip.background =element_rect(fill="#fcef03"),
        panel.background = element_rect(fill = "#fffce3"),
        plot.title = element_text(size=16))

ggsave( "/Users/Personas/Dropbox/side_projects/github/tidy-tuesdays/week_06_04_2020/figs/plot_tour.png", plot_tour )

plot_tour



 