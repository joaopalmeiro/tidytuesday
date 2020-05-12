library(tidyverse)
library(ggtext)
library(here)
library(hrbrthemes)

volcano <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv"
  )

volcanos_by_country <-
  volcano %>%
  group_by(country) %>%
  tally(sort = TRUE)

volcanos_pt <-
  volcano %>%
  filter(grepl("portugal", country, ignore.case = TRUE)) %>%
  select(
    volcano_number,
    volcano_name,
    last_eruption_year,
    contains("population")
  )

volcanos_pt <- volcanos_pt %>% pivot_longer(
  -c(volcano_number, volcano_name, last_eruption_year),
  names_to = "population",
  values_to = "count"
)

volcanos_pt <- volcanos_pt %>%
  mutate(population = as.integer(str_extract(population, "[:digit:]+")))

volcanos_pt

volcanos_pt %>% ggplot(aes(x = population, y = volcano_name, size = count)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(breaks = unique(volcanos_pt$population))
