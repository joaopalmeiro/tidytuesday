library(tidyverse)
library(ggtext)
library(here)
library(hrbrthemes)

PATH <- here("2020-08-11")

tick <- "\u2502"

avatar <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv"
  )

avatar_imdb <-
  readr::read_csv(
    paste0(PATH, "/imdb-20200811_1321.csv"),
    col_types = cols(
      season = col_integer(),
      episode = col_integer(),
      title = col_character(),
      total_votes = col_integer(),
      rating = col_double()
    )
  ) %>% filter(!episode == 0)

head(avatar)
head(avatar_imdb)

avatar %>%
  summarise_all(list(~ sum(is.na(.))))

avatar %>%
  filter(is.na(imdb_rating)) %>%
  group_by(chapter) %>%
  tally()

episode_cols <-
  c(
    "book",
    "book_num",
    "chapter",
    "chapter_num",
    "writer",
    "director",
    "imdb_rating"
  )

episodes <- avatar %>%
  select(all_of(episode_cols)) %>%
  distinct()
head(episodes)

episodes %>%
  group_by(book_num) %>%
  tally()

episodes %>%
  summarise_all(list(~ sum(is.na(.))))

episodes %>%
  filter(is.na(imdb_rating))

group <- episodes %>%
  group_by(book) %>%
  summarise(imdb_rating = mean(imdb_rating, na.rm = TRUE))

group

episodes %>% ggplot(aes(x = imdb_rating, y = book)) +
  geom_point(
    shape = tick,
    size = 2,
    alpha = .4
  ) +
  geom_point(data = group, size = 4, shape = tick)
