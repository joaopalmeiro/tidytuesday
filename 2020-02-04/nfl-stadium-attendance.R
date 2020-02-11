library(tidyverse)

# Datasets.
attendance <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')
games <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/games.csv')
liga_portugal <- read_csv("liga_portugal_spectators_stats.csv")

glimpse(attendance)
glimpse(liga_portugal)

is.na(attendance) %>% colSums()
is.na(liga_portugal) %>% colSums()

count_year <- attendance %>% count(year, sort = TRUE)
print(count_year, n = nrow(count_year))

cardinals_2000 <- attendance %>% filter(year == 2000 & team_name == "Cardinals")

sum(cardinals_2000$weekly_attendance, na.rm = TRUE) == mean(cardinals_2000$total)
