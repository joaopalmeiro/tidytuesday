library(tidyverse)
library(ggtext)
library(here)
library(hrbrthemes)

source(here("utils", "theme.R"))

PATH <- here("2020-08-11")
TICK <- "\u2502"

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
head(avatar_imdb)

episodes %>%
  left_join(avatar_imdb, by = c("book_num" = "season", "chapter_num" = "episode")) %>%
  rename(tidy_tuesday = imdb_rating, imdb = rating) %>%
  select(book_num, chapter_num, tidy_tuesday, imdb) %>%
  filter(tidy_tuesday != imdb | is.na(tidy_tuesday))

episodes %>%
  group_by(book_num) %>%
  tally()

episodes %>%
  summarise_all(list(~ sum(is.na(.))))

episodes %>%
  filter(is.na(imdb_rating))

group <- episodes %>%
  group_by(book_num) %>%
  summarise(imdb_rating = mean(imdb_rating, na.rm = TRUE))

group_imdb <- avatar_imdb %>%
  group_by(season) %>%
  summarise(rating = mean(rating))

group
group_imdb

avatar_imdb %>% ggplot(aes(x = rating, y = season)) +
  geom_point(
    shape = TICK,
    size = 2,
    alpha = .4
  ) +
  geom_point(data = group_imdb, size = 4, shape = TICK) +
  base_theme()
