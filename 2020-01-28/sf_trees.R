library(tidyverse)
library(lubridate)
library(dplyr)

# Dataset.
sf_trees <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv'
  )
glimpse(sf_trees)

# San Francisco Tree Guide table (https://sfenvironment.org/sites/default/files/fliers/files/sf_tree_guide.pdf).
sf_tree_guide <- readr::read_delim("sf_tree_guide.csv", delim = ";")
glimpse(sf_tree_guide)

sum(duplicated(sf_trees$tree_id))

is.na(sf_trees) %>% colSums()

count_takeover <- sf_trees %>% dplyr::count(caretaker, sort = TRUE)
print(count_takeover, n = nrow(count_takeover))
sum(count_takeover$n) == nrow(sf_trees)

count_species <- sf_trees %>% dplyr::count(species, sort = TRUE)
print(count_species, n = nrow(count_species))
sum(count_species$n) == nrow(sf_trees)

sf_trees %>% filter(species == "::")

sf_tree_work <-
  sf_trees %>% separate(
    species,
    c("species_name", "species_common_name"),
    sep = " :: ",
    remove = TRUE
  )

sf_tree_work$species_name <-
  str_remove(sf_tree_work$species_name, "(\\s?::)")
sf_tree_work$species_name[sf_tree_work$species_name == ""] <- NA

count_species_common_name <-
  sf_tree_work %>% dplyr::count(species_name, sort = FALSE)
print(count_species_common_name, n = nrow(count_species_common_name))

count_species_scientific_name <-
  sf_tree_work %>% dplyr::count(species_name, sort = FALSE)
print(count_species_scientific_name,
      n = nrow(count_species_scientific_name))

# Acer buergeranum -> Acer buergerianum
ids_pre_replacement <-
  sf_tree_work %>% filter(species_name == "Acer buergeranum")
ids_pre_replacement <- ids_pre_replacement$tree_id
ids_pre_replacement

sf_tree_work$species_name <-
  str_replace(sf_tree_work$species_name,
              "Acer buergeranum",
              "Acer buergerianum")
ids_post_replacement <-
  sf_tree_work %>% filter(species_name == "Acer buergerianum")
ids_post_replacement <- ids_post_replacement$tree_id
ids_post_replacement

sum(ids_pre_replacement == ids_post_replacement) == length(ids_pre_replacement)

# Magnolia grandiflora 'Saint Mary' -> Magnolia grandiflora 'St. Mary'
ids_pre_replacement <-
  sf_tree_work %>% filter(species_name == "Magnolia grandiflora 'Saint Mary'")
ids_pre_replacement <- ids_pre_replacement$tree_id
ids_pre_replacement

sf_tree_work$species_name <-
  str_replace(
    sf_tree_work$species_name,
    "Magnolia grandiflora 'Saint Mary'",
    "Magnolia grandiflora 'St. Mary'"
  )
ids_post_replacement <-
  sf_tree_work %>% filter(species_name == "Magnolia grandiflora 'St. Mary'")
ids_post_replacement <- ids_post_replacement$tree_id
ids_post_replacement

sum(ids_pre_replacement == ids_post_replacement) == length(ids_pre_replacement)

# Join both datasets.
sf_tree_merged <-
  sf_tree_work %>% left_join(
    sf_tree_guide %>% select(`Scientific Name`, `Water Use`),
    by = c("species_name" = "Scientific Name")
  ) %>% rename(water_use = `Water Use`)
glimpse(sf_tree_merged)

# Date manipulation.
sf_tree_merged$year <- year(sf_tree_merged$date)
sf_tree_merged$month <- month(sf_tree_merged$date, label = TRUE)
sf_tree_merged$week_day <- wday(sf_tree_merged$date, label = TRUE)
sf_tree_merged$week_day_no <-
  wday(sf_tree_merged$date, label = FALSE)
sf_tree_merged$month_year <-
  factor(paste(sf_tree_merged$month, sf_tree_merged$year, sep = " "))
sf_tree_merged$week_year <- week(sf_tree_merged$date)

# lubridate: each week starts on Sunday (by default).
first_day_of_month_wday <- function(dx) {
  day(dx) <- 1
  wday(dx)
}

# The -1 is necessary because when the month starts on a Sunday this adjustment is not necessary.
sf_tree_merged$week_month <-
  ceiling((
    day(sf_tree_merged$date) + first_day_of_month_wday(sf_tree_merged$date) - 1
  ) / 7)

# Dataset for the calendar plot.
sf_tree_calendar <-
  sf_tree_merged %>% drop_na(date) %>% group_by(date, week_month, week_day, month, year) %>% tally()
glimpse(sf_tree_calendar)

sf_tree_calendar_sanity_check <-
  sf_tree_merged %>% drop_na(date) %>% group_by(date) %>% tally()
sum(sf_tree_calendar_sanity_check$n == sf_tree_calendar$n) == nrow(sf_tree_calendar_sanity_check)

count_year <-
  sf_tree_calendar %>% ungroup() %>% dplyr::count(year, sort = FALSE)
print(count_year, n = nrow(count_year))

# Calendar plot.
sf_tree_calendar %>% filter(year >= 2010 & year <= 2019) %>%
  ggplot(aes(week_month, week_day, fill = n)) +
  geom_tile(colour = "white") + 
  facet_grid(year ~ month) + 
  scale_fill_gradient(low = "#EAFBEA", high = "#1F6650") +
  labs(x="",
       y="",
       title = "", 
       subtitle="", 
       fill="") + theme_bw()

# ggsave("calendar_plot.png")
