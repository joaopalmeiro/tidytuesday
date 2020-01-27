library(tidyverse)

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

count_takeover <- sf_trees %>% count(caretaker, sort = TRUE)
print(count_takeover, n = nrow(count_takeover))
sum(count_takeover$n) == nrow(sf_trees)

count_species <- sf_trees %>% count(species, sort = TRUE)
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
