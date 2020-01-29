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
sf_tree_merged$month_year <-
  factor(paste(sf_tree_merged$month, sf_tree_merged$year, sep = " "))
sf_tree_merged$week_year <- week(sf_tree_merged$date)

# sf_tree_merged <- sf_tree_merged %>%
#   group_by(month_year) %>%
#   mutate(week_month_teste = 1 + week_year - min(week_year))

teste <- sf_tree_merged %>% select(date, week_month_teste, teste2)
