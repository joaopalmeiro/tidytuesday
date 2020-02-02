library(tidyverse)
library(lubridate)
library(dplyr)
library(ggtext)

packageVersion("ggplot2")
packageVersion("ggtext")

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
title_style <- function() {
  theme(
    plot.title.position = "plot",
    plot.title = element_text(colour = "#2F2F2F"),
    plot.subtitle = element_markdown(colour = "#2F2F2F")
  )
}

line_break_year <- function(year) {
  paste(substr(year, 1, ceiling(nchar(year) / 2)),
        substr(year, ceiling(nchar(year) / 2) + 1, nchar(year)),
        sep = "\n")
}

get_missing_dates <-
  function(dates_not_missing, start_year, end_year) {
    dates <-
      seq(as.Date(paste(start_year, "-01-01", sep = "")), as.Date(paste(end_year, "-12-31", sep =
                                                                          "")), by = "days")
    
    dates = dates[!(dates %in% dates_not_missing)]
    
    dates_df <- tibble(
      date = dates,
      week_month = ceiling((
        day(dates) + first_day_of_month_wday(dates) - 1
      ) / 7),
      week_day = wday(dates, label = TRUE),
      month = month(dates, label = TRUE),
      year = year(dates),
      n = NA
    )
    
    return(dates_df)
  }

get_calendar_plot <-
  function(df,
           start_year,
           end_year,
           week_month_col,
           week_day_col,
           count_col,
           year_col,
           month_col,
           date_col) {
    black_color <- "#2F2F2F"
    gray_color <- alpha("#CBCBCB", 0.2)
    brown_color <- "#E6A157"
    
    df_work <-
      df %>% filter(get(year_col) >= start_year &
                      get(year_col) <= end_year)
    
    # Five-number summary.
    min_count <- min(df_work[[count_col]])
    median_count <- median(df_work[[count_col]])
    max_count <- max(df_work[[count_col]])
    # middle_count <- ceiling((max_count - min_count) / 2)
    
    date_w_max_tree <-
      (df_work %>% filter(get(count_col) == max_count))$date
    
    lower_quartile_count <-
      ceiling(quantile(df_work[[count_col]], probs = c(0.25))[[1]])
    upper_quartile_count <-
      ceiling(quantile(df_work[[count_col]], probs = c(0.75))[[1]])
    
    breaks_legend <-
      c(min_count,
        lower_quartile_count,
        median_count,
        upper_quartile_count,
        max_count)
    # breaks_legend <- c(min_count, middle_count, max_count)
    
    missing_dates_df <-
      get_missing_dates(df_work[[date_col]], start_year, end_year)
    
    days_wo_trees <- nrow(missing_dates_df)
    
    df_work <-
      bind_rows(df_work, missing_dates_df)
    
    total_days <- nrow(df_work)
    
    month_year_w_max_mean <- df_work %>%
      group_by(year_c = get(year_col),
               month_c = get(month_col)) %>%
      summarise(mean = sum(get(count_col), na.rm = TRUE) / length(get(count_col)))
    
    month_year_w_max_mean <-
      month_year_w_max_mean %>% ungroup() %>% filter(mean == max(mean))
    
    if (start_year == end_year) {
      title <-
        bquote("Number of trees planted in" ~ bold(.(toString(start_year))) ~ "in San Francisco.")
    } else {
      title <-
        bquote("Number of trees planted between" ~ bold(.(toString(start_year))) ~ "and" ~ bold(.(toString(end_year))) ~ "in San Francisco.")
    }
    
    # 01/01/2019 was the day with the most trees planted (16).
    # On average, February 2019 was the month with the most trees planted (15 per day).
    
    subtitle <-
      paste(
        "There were no trees planted in <span style = 'color:",
        brown_color,
        ";'>**",
        days_wo_trees,
        "**</span>/",
        total_days,
        " days. ",
        date_w_max_tree,
        " was the day with the most trees planted",
        sep = ""
      )
    
    base_size <- 10
    half_line <- base_size / 2
    
    calendar_plot <-
      df_work %>%
      ggplot(aes_string(week_month_col, week_day_col, fill = count_col)) +
      geom_tile(mapping = aes_string(colour = count_col), size = 0) +
      scale_color_gradient(
        low = "#EAFBEA",
        high = "#1F6650",
        guide = guide_colorbar(
          title.position = "top",
          label.position = "bottom",
          order = 1
        )
      ) +
      geom_tile(colour = "white") +
      coord_fixed(ratio = 1) +
      facet_grid(reformulate(month_col, year_col),
                 labeller = labeller(.rows = line_break_year)) +
      scale_fill_gradient(
        low = "#EAFBEA",
        high = "#1F6650",
        na.value = brown_color,
        breaks = breaks_legend,
        guide = guide_legend(
          title.position = "top",
          label.position = "bottom",
          order = 2
        )
      ) +
      scale_x_discrete(limits = c(1, 6)) +
      scale_y_discrete(breaks = c("Sun", "Sat")) +
      list(
        theme_bw(base_size = base_size) + title_style() + theme(
          panel.grid = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(colour = gray_color),
          legend.position = "top",
          legend.direction = "horizontal",
          axis.title = element_blank(),
          legend.margin = margin(unit(0, "cm")),
          axis.ticks = element_blank(),
          legend.key.height = unit(0.2, "cm"),
          legend.text = element_text(size = 8, colour = black_color),
          legend.title = element_text(size = 8, colour = black_color),
          strip.text = element_text(colour = black_color),
          axis.text = element_text(colour = black_color),
          plot.margin = margin(half_line, half_line, half_line, half_line),
          strip.text.y = element_text(angle = 0, margin = margin(l = 0.8 * half_line /
                                                                   2)),
          strip.placement = "outside",
          strip.text.x = element_text(margin = margin(b = 0.8 * half_line /
                                                        2))
        )
      ) +
      labs(
        title = title,
        subtitle = subtitle,
        fill = "Five-number summary",
        colour = "Continuous summary"
      )
    
    return(calendar_plot)
  }

get_calendar_plot(sf_tree_calendar,
                  2017,
                  2019,
                  "week_month",
                  "week_day",
                  "n",
                  "year",
                  "month",
                  "date")

# ggsave("calendar_plot.png")
