# Source
#
# Author: IMDb
# URL: https://www.imdb.com/
# Accessed: Tuesday, August 11, 2020

library(rvest)
library(stringr)
library(here)

ID <- "tt0417299"
URL <- paste0("http://www.imdb.com/title/", ID, "/")

SEASON_NUMBER_PATTERN <- "(\\d+)"
SEASON_AND_EPISODE_PATTERN <- "S(\\d+), Ep(\\d+)"

series <- html_session(URL)
series <- series %>% jump_to("episodes")

seasons <- series %>%
  html_nodes("#bySeason") %>%
  html_text() %>%
  str_squish()

seasons <- seasons %>%
  str_extract_all(SEASON_NUMBER_PATTERN) %>%
  unlist()
seasons

data <-
  tibble(
    season = integer(),
    episode = integer(),
    title = character(),
    total_votes = integer(),
    rating = numeric()
  )

for (season in seasons) {
  cat("Getting season", season, "data...", "\n")

  query <- paste0("?season=", season)
  series_season <- series %>% jump_to(query)

  eplist <- series_season %>%
    html_node(".eplist") %>%
    html_text() %>%
    str_match_all(SEASON_AND_EPISODE_PATTERN)

  title <- series_season %>%
    html_nodes("a[itemprop=name]") %>%
    html_text()

  total_votes <- series_season %>%
    html_nodes(".ipl-rating-star__total-votes") %>%
    html_text() %>%
    str_extract_all("\\d+(?:,\\d+)?") %>%
    unlist() %>%
    {
      as.integer(gsub(",", "", .))
    }

  rating <- series_season %>%
    html_nodes(".ipl-rating-star.small > .ipl-rating-star__rating") %>%
    html_text() %>%
    as.numeric()

  season_data <-
    tibble(
      season = eplist[[1]][, 2] %>% as.integer(),
      episode = eplist[[1]][, 3] %>% as.integer(),
      title = title,
      total_votes = total_votes,
      rating = rating
    )

  data <- bind_rows(data, season_data)
}

head(data)

path <- here(
  "2020-08-11",
  paste0(
    "imdb-",
    format(Sys.time(), "%Y%m%d_%H%M"),
    ".csv"
  )
)
# write_csv(data, path)
