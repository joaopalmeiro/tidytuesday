library(tidyverse)

passwords <-
  read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv'
  )

passwords

nrow(passwords)
nrow(distinct(passwords))

# Drop rows containing missing values
narows <- unique(which(is.na(passwords), arr.ind = TRUE)[, 1])

slice(passwords, narows)

passwordswork <- passwords %>% drop_na()
passwordswork

length(unique(passwordswork$password)) == nrow(passwordswork)

# Levenshtein distance
levenshtein.distance <- function(df, col, word, topn) {
  distances <- mapply(adist,
                      df[[col]],
                      word,
                      MoreArgs = list(partial = FALSE, ignore.case = FALSE))
  
  distancesdf <- bind_cols(password = df[[col]], distance = distances)
  
  topndf <- arrange(distancesdf, desc(distance))
  
  return(topndf[1:topn, ])
}

levenshtein.distance(passwordswork, "password", "password", 10)
