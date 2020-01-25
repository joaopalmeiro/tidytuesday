library(tidyverse)
library(scales)

# Dataset.
spotify_songs <-
  read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv'
  )
spotify_songs

glimpse(spotify_songs)

names(spotify_songs)
track_features <- names(spotify_songs)[12:23]
track_features

track_features[track_features == "duration_ms"] <- "duration_min"
track_features

# Drop rows containing missing values.
spotify_songs_work <- spotify_songs %>% drop_na()
spotify_songs_work

glimpse(spotify_songs_work)

# Duplicated rows.
spotify_songs_work[duplicated(spotify_songs_work$track_id),]
spotify_songs_work[duplicated(spotify_songs_work$track_id),][1, 1]

# Same track, different playlists.
check_track_id <-
  spotify_songs_work[spotify_songs_work$track_id == "1HfMVBKM75vxSfsQ5VefZ5",]

# Drop duplicated rows.
spotify_songs_work <-
  spotify_songs_work[!duplicated(spotify_songs_work$track_id),]
glimpse(spotify_songs_work)

# Check rows with more than one artist per track.
feat_names <- "(feat.|featuring|ft.)"

feat <-
  spotify_songs_work %>% filter(grepl(
    paste("\\s", feat_name, "\\s", sep = ""),
    track_artist,
    ignore.case = TRUE
  ))
glimpse(feat)

spotify_songs_work %>% filter(grepl("Z-Ro", track_artist, ignore.case = TRUE))

# Collaborations.
get_feat_instances <- function(df, artist_1, artist_2) {
  out <- tryCatch({
    reg_expr <- paste("(", artist_1, "|", artist_2, ")", sep = "")
    reg_expr_artist_1 <- paste("^", artist_1, "$", sep = "")
    reg_expr_artist_2 <- paste("^", artist_2, "$", sep = "")
    
    artists <-
      df %>% filter(grepl(reg_expr, track_artist, ignore.case = TRUE))
    
    sample_artist_1 <-
      artists %>% filter(grepl(reg_expr_artist_1, track_artist, ignore.case = TRUE)) %>% group_by(track_artist) %>% sample_n(1)
    sample_artist_2 <-
      artists %>% filter(grepl(reg_expr_artist_2, track_artist, ignore.case = TRUE)) %>% group_by(track_artist) %>% sample_n(1)
    
    if (nrow(sample_artist_1) == 0 && nrow(sample_artist_2) == 0) {
      stop(paste(
        "There is no music tracks by ",
        artist_1,
        " or by ",
        artist_2,
        ".\n",
        sep = ""
      ))
    } else if (nrow(sample_artist_1) == 0) {
      stop(paste("There is no music tracks by ",
                 artist_1,
                 ".\n",
                 sep = ""))
    } else if (nrow(sample_artist_2) == 0) {
      stop(paste("There is no music tracks by ",
                 artist_2,
                 ".\n",
                 sep = ""))
    }
    
    feat_reg_expr <-
      paste(
        "(",
        artist_1,
        "|",
        artist_2,
        ")",
        "[[:space:]]+",
        feat_names,
        "[[:space:]]+",
        "(",
        artist_1,
        "|",
        artist_2,
        ")",
        sep = ""
      )
    
    feat <-
      artists %>% filter(grepl(feat_reg_expr, track_artist, ignore.case = TRUE))
    
    if (nrow(feat) == 0) {
      stop(
        paste(
          artist_1,
          "and",
          artist_2,
          "don't collaborate on any music track.\n",
          sep = " "
        )
      )
    }
    
    sample_feat <-
      feat %>%
      group_by(track_artist) %>%
      sample_n(1)
    
    sample_feat$track_artist <-
      sub(feat_names, 'ft.', sample_feat$track_artist, ignore.case = TRUE)
    
    df_ready <- add_column(bind_rows(sample_artist_1, sample_feat, sample_artist_2),
               group = 1)
    
    # Convert ms to min.
    df_ready$duration_ms <- df_ready$duration_ms / (60*1000)
    
    df_ready %>% rename(duration_min = duration_ms) 
    
  },
  error = function(cond) {
    message(cond)
    return(NA)
  })
  return(out)
}

get_feat_instances(spotify_songs_work, "Big Moe", "Z-Ro")
get_feat_instances(spotify_songs_work, "Big Moe", "Z-Ro")$track_artist
get_feat_instances(spotify_songs_work, "Big Moe", "Z-Ro")$danceability
get_feat_instances(spotify_songs_work, "Big Moe", "Z-Ro")$group
get_feat_instances(spotify_songs_work, "Big Moe", "Z-Ro")$duration_min

get_feat_instances(spotify_songs_work, "TPE", "Adam Marano")
get_feat_instances(spotify_songs_work, "Randy Leroy", "Bollebof")
get_feat_instances(spotify_songs_work, "Cheka", "Michael Stuart")
get_feat_instances(spotify_songs_work, "Big Moe", "Ronnetta Spencer")
get_feat_instances(spotify_songs_work, "SWV", "Wu-Tang Clan")

feat_work <-
  get_feat_instances(spotify_songs_work, "Big Moe", "Z-Ro")
feat_work
feat_work$danceability
# feat_work$duration_ms
feat_work$duration_min

feat_work_2 <-
  get_feat_instances(spotify_songs_work, "SWV", "Wu-Tang Clan")
feat_work_2
feat_work_2$danceability

# Style function.
# Inspired by the BBC style (https://bbc.github.io/rcookbook/).
clean_style <- function() {
  fonttitle <- "Roboto"
  fontsubtitle <- "Roboto Thin"
  
  theme(
    plot.title = element_text(family = fonttitle,
                              color = "#2F2F2F"),
    plot.subtitle = element_text(
      family = fontsubtitle,
      size = 8,
      color = "#2F2F2F"
    ),
    axis.title = element_blank(),
    axis.text = element_text(family = fonttitle,
                             color = "#2F2F2F"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )
}

# Slope chart.
first_uppercase <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

plot_triple_slope_chart <- function(df, x, y, group) {
  if (sum(df[[y]] %% 1 == 0) > 0) {
    label_format <- number_format(big.mark = ',')
  } else {
    label_format <- number_format(accuracy = 0.01, decimal.mark = '.')
  }
  
  title <-
    bquote(
      "Difference in" ~ bold(.(y)) ~ "between the collaboration track and a random track by each of the artists."
    )
  
  plot <-
    df %>%
    ggplot(aes_string(x = x, y = y, group = group)) +
    geom_line(size = 1, colour = "#2F2F2F") +
    geom_point(
      data = df[2, ],
      colour = "#2F2F2F",
      fill = "white",
      size = 6,
      shape = 21,
      mapping = aes_string(x = x, y = y)
    ) +
    geom_point(size = 4, colour = "#2F2F2F") +
    scale_x_discrete(position = "top") +
    scale_y_continuous(breaks = seq(min(df[[y]]),
                                    max(df[[y]]),
                                    length.out = 3),
                       labels = label_format) +
    list(theme_minimal() + clean_style()) +
    labs(title = title, subtitle = "")
  
  return(plot)
}

plot_triple_slope_chart(feat_work, "track_artist", "danceability", "group")
# ggsave("spotify_songs_1.png")

plot_triple_slope_chart(feat_work, "track_artist", "duration_min", "group")
plot_triple_slope_chart(feat_work, "track_artist", "instrumentalness", "group")

plot_triple_slope_chart(feat_work_2, "track_artist", "danceability", "group")

# Small multiple.
plot_multiple_slope_chart <- function(df, x, track_features, group) {
  y_axis_breaks <- function(y) { seq(min(y), max(y), length.out = 3) }
  
  label_format <- function(l) {
    scaled = case_when(l >= 1E3 ~ paste0(formatC(l/1E3, digits = 0, big.mark = ",", format = "f"), "K"),
                       l %% 1 > 0 ~ paste0(formatC(l, format = "f", digits = 2, decimal.mark = '.')),
                       TRUE ~ paste0(l))
    return(scaled)
  }
  
  plot <-
    df %>%
    select(c(x, group, track_features)) %>%
    pivot_longer(cols = track_features) %>%
    ggplot(aes_string(x = x, y = "value", group = group)) +
    geom_line(size = 0.5) +
    geom_point(size = 3) +
    scale_x_discrete(position = "top") +
    scale_y_continuous(breaks = y_axis_breaks, expand=expand_scale(0,0), labels = label_format) +
    list(theme_minimal() + clean_style() + theme(panel.spacing = unit(2, "lines"))) +
    facet_wrap(~ name, scale = "free_y") + 
    coord_cartesian(clip = "off")
  
  return(plot)
}

plot_multiple_slope_chart(feat_work, "track_artist", track_features, "group")

feat_work[track_features]
feat_work$tempo
feat_work$instrumentalness

feat_work %>%
  select(c("track_artist", "group", track_features)) %>%
  pivot_longer(cols = track_features)
