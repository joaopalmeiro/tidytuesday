# Source 
# 
# Author: Liga Portugal
# Title: Stats
# URL: https://www.ligaportugal.pt/en/homepage/
# Accessed: 2020-02-11

library(rvest)

get_liga_portugal_stats <-
  function(competition = "Premier League",
           language = "en") {
    # homepage <- paste("https://www.ligaportugal.pt/", language,"/homepage/", sep="")
    base_URL <-
      paste("https://www.ligaportugal.pt/", language, "/", sep = "")
    spectators <- "liga/estatisticas/espectadores/clube/"
    
    first_season_start_year <- 2009
    last_season_end_year <- 2019
    
    final_table <- tibble(
      Ranking = character(),
      Team = character(),
      Competition = character(),
      `Spectators in the last matchweek` = character(),
      `Number of home games` = integer(),
      `Average attendance` = character(),
      `Average ocupation %` = character(),
      `Season total` = character()
    )
    
    while (first_season_start_year < last_season_end_year) {
      if (competition == "Premier League") {
        season <-
          paste(first_season_start_year,
                first_season_start_year + 1,
                "/",
                sep = "")
        
        if (first_season_start_year == 2009) {
          premier_league_name <- "ligasagres"
          
          s <-
            read_html(paste(
              base_URL,
              spectators,
              season,
              premier_league_name,
              sep = ""
            ))
          
          table <- s %>%
            html_node("table") %>%
            html_table(fill = TRUE)
          
          final_table <- bind_rows(final_table, table)
        } else if (first_season_start_year >= 2010 &
                   first_season_start_year < 2014) {
          premier_league_name <- "ligazonsagres"
          
          s <-
            read_html(paste(
              base_URL,
              spectators,
              season,
              premier_league_name,
              sep = ""
            ))
          
          table <- s %>%
            html_node("table") %>%
            html_table(fill = TRUE)
          
          final_table <- bind_rows(final_table, table)
        } else {
          premier_league_name <- "liganos"
          
          s <-
            read_html(paste(
              base_URL,
              spectators,
              season,
              premier_league_name,
              sep = ""
            ))
          
          table <- s %>%
            html_node("table") %>%
            html_table(fill = TRUE)
          
          final_table <- bind_rows(final_table, table)
        }
        
        first_season_start_year <- first_season_start_year + 1
      }
    }
    
    return(final_table)
  }

stats <- get_liga_portugal_stats()
stats

is.na(stats) %>% colSums()

write_csv(stats, "liga_portugal_spectators_stats.csv")
