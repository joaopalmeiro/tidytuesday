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
spotify_songs_work[duplicated(spotify_songs_work$track_id), ]
spotify_songs_work[duplicated(spotify_songs_work$track_id), ][1, 1]

# Same track, different playlists.
check_track_id <-
  spotify_songs_work[spotify_songs_work$track_id == "1HfMVBKM75vxSfsQ5VefZ5", ]

# Drop duplicated rows.
spotify_songs_work <-
  spotify_songs_work[!duplicated(spotify_songs_work$track_id), ]
glimpse(spotify_songs_work)

# Check rows with more than one artist per track (according to track_artist).
feat_names <- "(feat.|featuring|ft.)"

feat <-
  spotify_songs_work %>% filter(grepl(
    paste("\\s", feat_names, "\\s", sep = ""),
    track_artist,
    ignore.case = TRUE
  ))
glimpse(feat)

spotify_songs_work %>% filter(grepl("Z-Ro", track_artist, ignore.case = TRUE))

# Collaborations (according to track_artist).
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
    
    df_ready <-
      add_column(bind_rows(sample_artist_1, sample_feat, sample_artist_2),
                 group = 1)
    
    # Convert ms to min.
    df_ready$duration_ms <- df_ready$duration_ms / (60 * 1000)
    
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
  get_feat_instances(spotify_songs_work, "SWV", "Wu-Tang Clan")
feat_work
feat_work$danceability

feat_work_2 <-
  get_feat_instances(spotify_songs_work, "Big Moe", "Z-Ro")
feat_work_2
feat_work_2$danceability
feat_work_2$loudness
feat_work_2$duration_min

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
      color = "#2F2F2F",
      margin = margin(0, 0, 20, 0)
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
      data = df[2,],
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
# ggsave("spotify_songs_slope_chart.png")

plot_triple_slope_chart(feat_work, "track_artist", "duration_min", "group")
plot_triple_slope_chart(feat_work, "track_artist", "instrumentalness", "group")

plot_triple_slope_chart(feat_work_2, "track_artist", "danceability", "group")

# Small multiple.
# Slope charts to compare a collaborative track and a random track by each of the artists.
dark_clean_style <- function() {
  fonttitle <- "Roboto"
  fontsubtitle <- "Roboto Thin"
  
  theme(
    plot.title = element_text(family = fonttitle,
                              color = "white"),
    plot.subtitle = element_text(
      family = fontsubtitle,
      size = 8,
      color = "white",
      margin = margin(0, 0, 20, 0)
    ),
    axis.title = element_blank(),
    axis.text = element_text(family = fonttitle,
                             color = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(color = "#2F2F2F", fill = "#2F2F2F"),
    strip.background = element_rect(fill = NA, color = NA),
    panel.background = element_rect(fill = "#2F2F2F", color  =  NA),
    strip.text = element_text(color = "white")
  )
}

plot_multiple_slope_chart <-
  function(df,
           x,
           track_features,
           group,
           title_verbose = TRUE,
           dark = FALSE,
           night_sky = FALSE) {
    y_axis_breaks <- function(y) {
      seq(min(y), max(y), length.out = 3)
    }
    
    label_format <- function(l) {
      scaled = case_when(
        l >= 1E3 ~ paste0(formatC(
          l / 1E3,
          digits = 0,
          big.mark = ",",
          format = "f"
        ), "K"),
        (abs(l) >= 0.01 & l %% 1 > 0) ~ paste0(formatC(
          l,
          format = "f",
          digits = 2,
          decimal.mark = '.'
        )),
        TRUE ~ paste0(l)
      )
      return(scaled)
    }
    
    if (title_verbose) {
      title <-
        bquote(bold(.(df[[x]][1])) ~ "vs" ~ bold(.(df[[x]][2])) ~ "vs" ~ bold(.(df[[x]][3])))
    } else {
      title <-
        bquote(.(df[[x]][1]) ~ bold(.("ft.")) ~ .(df[[x]][3]))
    }
    
    if (dark) {
      point_line_color <- "white"
      overall_style <- dark_clean_style()
    } else {
      point_line_color <- "#2F2F2F"
      overall_style <- clean_style()
    }
    
    if (night_sky) {
      small_multiple_theme <- theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(
          family = "Roboto Thin",
          size = 8,
          color = "white"
        )
      )
    } else {
      small_multiple_theme <-
        theme(panel.grid.major.y = element_line(size = 0.1, colour = point_line_color))
    }
    
    plot <-
      df %>%
      select(c(x, group, track_features)) %>%
      pivot_longer(cols = track_features) %>%
      ggplot(aes_string(x = x, y = "value", group = group)) +
      geom_line(size = 0.5, colour = point_line_color) +
      geom_point(size = 3, colour = point_line_color) +
      scale_x_discrete(position = "top", labels = NULL) +
      scale_y_continuous(breaks = y_axis_breaks,
                         expand = expand_scale(0, 0),
                         labels = label_format) +
      list(
        theme_minimal() + overall_style + theme(
          plot.margin = unit(c(1, 1, 1, 1), "lines"),
          panel.spacing = unit(2, "lines"),
          strip.text.x = element_text(margin = margin(0, 0, 10, 0))
        ) + small_multiple_theme
      ) +
      facet_wrap( ~ name, scale = "free_y") +
      coord_cartesian(clip = "off") +
      labs(title = title,
           subtitle = "A comparison between a collaborative track and a random track by each of the artists.")
    
    return(plot)
  }

plot_multiple_slope_chart(
  feat_work,
  "track_artist",
  track_features,
  "group",
  dark = TRUE,
  night_sky = TRUE
)
# ggsave("spotify_songs_small_multiple_slope_chart_night_sky.png")

plot_multiple_slope_chart(feat_work,
                          "track_artist",
                          track_features,
                          "group")
# ggsave("spotify_songs_small_multiple_slope_chart.png")

# Collaborations (according to track_name).
feat_names_track_name <- "(?:feat.|featuring|ft.|ft|feat)"
feat_names_track_name_with <- "(?:feat.|featuring|ft.|ft|feat|with)"

feat_track_name <-
  spotify_songs_work %>% filter(grepl(
    paste("(\\s|\\()", feat_names_track_name, "\\s", sep = ""),
    track_name,
    ignore.case = TRUE
  ))
glimpse(feat_track_name)

# Split track_name column.
# The new column contains only the first artist of the collaboration.
track_name_regex <-
  paste(
    "(?:\\s|\\(|\\[)",
    feat_names_track_name_with,
    "\\s",
    "([\\w\\s\\.]+)(?:\\.|\\]|\\))",
    sep = ""
  )

feat_artist_col <-
  str_match(spotify_songs_work$track_name,
            regex(track_name_regex, ignore_case = TRUE))
feat_artist_col

feat_artists_df <-
  spotify_songs_work %>%
  mutate(feat_artist = feat_artist_col[, 2])

get_two_artists_df <-
  function(df,
           artist_1_col,
           artist_2_col,
           artist_1,
           artist_2) {
    out <- tryCatch({
      out_df <- df %>%
        filter(grepl(
          paste("(", artist_1, "|", artist_2, ")", sep = ""),
          get(artist_1_col),
          ignore.case = TRUE
        ))
      
      masked_df <- out_df %>%
        filter(grepl(
          paste("(", artist_1, "|", artist_2, ")", sep = ""),
          get(artist_2_col),
          ignore.case = TRUE
        ))
      
      if (nrow(masked_df) == 0) {
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
      
      # Convert ms to min.
      out_df$duration_ms <- out_df$duration_ms / (60 * 1000)
      
      out_df %>% rename(duration_min = duration_ms)
    },
    error = function(cond) {
      message(cond)
      return(NA)
    })
    return(out)
  }

lamar_rihanna_df <-
  get_two_artists_df(feat_artists_df,
                     "track_artist",
                     "feat_artist",
                     "Kendrick Lamar",
                     "Rihanna")

# Freedman–Diaconis rule.
fd <- function(x) {
  n <- length(x)
  r <- IQR(x)
  
  2 * r / n ^ (1 / 3)
}

lamar_rihanna_df %>%
  select(c('track_artist', track_features)) %>%
  pivot_longer(cols = track_features)

# Small multiple.
# Density estimation of audio features.
# Comparison between the tracks of two artists and the mean of the tracks in which they collaborate.
plot_densities <-
  function(df,
           artist_1,
           artist_2,
           artist_col,
           feat_col,
           track_features) {
    density_colors <- c("#AA5353", "#72AFA4")
    
    feat_df <- df %>%
      filter(grepl(
        paste("(", artist_1, "|", artist_2, ")", sep = ""),
        get(feat_col),
        ignore.case = TRUE
      )) %>% select(c(artist_col, track_features)) %>%
      pivot_longer(cols = track_features) %>%
      group_by(name) %>%
      summarise(mean_value = mean(value))
    
    subtitle <-
      bquote(
        "A comparison between" ~ .(artist_1) * "," ~ .(artist_2) * ", and the mean of their collaborations."
      )
    
    df %>%
      select(c(artist_col, track_features)) %>%
      pivot_longer(cols = track_features) %>%
      ggplot(aes(x = value)) +
      geom_density(
        aes_string(color = artist_col),
        alpha = 0.5,
        size = 0.5,
        key_glyph = "timeseries"
      ) +
      scale_color_manual(values = density_colors) +
      geom_vline(
        data = feat_df,
        aes(
          xintercept = mean_value,
          linetype = paste(artist_1, "feat.", artist_2, sep = " ")
        ),
        color = "#CBCBCB",
        key_glyph = "vline"
      ) +
      geom_hline(yintercept = 0,
                 size = 0.5,
                 colour = "#CBCBCB") +
      facet_wrap(~ name, ncol = 3, scales = 'free') +
      list(
        theme_minimal() +
          clean_style() +
          theme(
            axis.text.y = element_blank(),
            panel.grid = element_blank(),
            legend.position = "top",
            legend.title = element_blank(),
            legend.text = element_text(colour = "#2F2F2F"),
            legend.margin = margin(c(0, 0, 0, 0)),
            legend.box.spacing = unit(c(0, 0, 1, 0), "lines")
          )
      ) +
      labs(title = "Density estimation of audio features", subtitle = subtitle)
  }

plot_densities(
  lamar_rihanna_df,
  "Kendrick Lamar",
  "Rihanna",
  "track_artist",
  "feat_artist",
  track_features
)

# ggsave("spotify_songs_small_multiple_kde.png")
