library(tidyverse)
library(tidytext)
library(showtext)
library(ggthemes)
library(viridis)


christmas_songs <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_songs.csv")

christmas_lyrics <- readr::read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-24/christmas_lyrics.tsv")


words_plot = christmas_lyrics %>%
  unnest_tokens(word, lyric) %>%
  count(track_title, sort = TRUE) %>%
  mutate(track_title = gsub("Ft. The Andrews Sisters", "", track_title)) %>%
  top_n(20) %>%
  ggplot(aes(fct_reorder(track_title, n), n)) +
  geom_col() +
  coord_flip() +
  labs(x = "",
       y = "Number of words",
       title = "Christmas songs with more words (1958 - 2016)", 
       caption = "@EdudinGonzalo | Source: Billboard Top 100") +
  theme_minimal() +
  theme(text=element_text(size= 6, family="Monaco")) +
  scale_color_brewer(palette = "Dark1")


ggsave( "week_30_12_2019/output/words_plot.png", words_plot)

#sentiments
bing = get_sentiments("bing")

sentiments_plot = christmas_lyrics %>%
  unnest_tokens(word, lyric) %>%
  inner_join(bing) %>%
  group_by(title) %>%
  count(sentiment) %>%
  ungroup %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(sentiment = as.factor(sentiment),
         title = reorder_within(title, n, sentiment)) %>%
  ggplot(aes(title, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "",
       y = "Number of words",
       title = "Sentiments in Christmas songs (1958 -2016)", 
       subtitle = "By number of positive/negative words", 
       caption = "@EdudinGonzalo | Source: Billboard Top 100") +
  theme_minimal() +
  theme(text=element_text(size= 6, family="Monaco")) +
  scale_color_brewer(palette = "Dark1")

ggsave( "week_30_12_2019/output/sentiments_plot.png", sentiments_plot)


words_sentiment = christmas_lyrics %>%
  unnest_tokens(word, lyric) %>%
  inner_join(bing) %>%
  group_by(word) %>%
  count(sentiment) %>%
  ungroup %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(sentiment = as.factor(sentiment),
         word = reorder_within(word, n, sentiment)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "",
       y = "Number of words",
       title = "Negative and positive words in Christmas songs (1958 -2016)",
       caption = "@EdudinGonzalo | Source: Billboard Top 100") +
  theme_minimal() +
  theme(text=element_text(size= 7.5, family="Monaco")) +
  scale_color_brewer(palette = "Dark2")

ggsave( "week_30_12_2019/output/words_sentiment_plot.png", words_sentiment)

# sentiments per time

sentiment_time = christmas_lyrics %>%
  mutate(week = anytime::anydate(weekid),
         year = lubridate::year(week)) %>%
  unnest_tokens(word, lyric) %>%
  inner_join(bing) %>%
  group_by(year) %>%
  count(sentiment) %>%
  ggplot(aes(year, n, colour = sentiment)) +
  geom_line() +
  geom_point() +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "",
       y = "Number of words",
       title = "Sentiments in Christmas songs over time (1958 -2016)",
       subtitle = "By number of positive/negative words" ,
       caption = "@EdudinGonzalo | Source: Billboard Top 100",
       colour = "") +
  theme_minimal() +
  theme(text=element_text(size= 7.5, family="Monaco")) +
  scale_color_brewer(palette = "Dark2")

ggsave("week_30_12_2019/output/sentiment_time.png", sentiment_time)




