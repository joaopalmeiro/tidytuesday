library(tidyverse)

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
    
    feat <- artists %>% filter(grepl(feat_reg_expr, track_artist, ignore.case = TRUE))
    
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
    
    sample_feat <- feat %>% group_by(track_artist) %>% sample_n(1)
    
    add_column(bind_rows(sample_artist_1, sample_feat, sample_artist_2),
               group = 1)
    
  },
  error = function(cond) {
    message(cond)
    return(NA)
  })
  return(out)
}

get_feat_instances(spotify_songs_work, "Big Moe", "Z-Ro")
get_feat_instances(spotify_songs_work, "Big Moe", "Z-Ro")$group

get_feat_instances(spotify_songs_work, "TPE", "Adam Marano")
get_feat_instances(spotify_songs_work, "Randy Leroy", "Bollebof")
get_feat_instances(spotify_songs_work, "Cheka", "Michael Stuart")
get_feat_instances(spotify_songs_work, "Big Moe", "Ronnetta Spencer")
get_feat_instances(spotify_songs_work, "SWV", "Wu-Tang Clan")

feat_work <-
  get_feat_instances(spotify_songs_work, "Big Moe", "Z-Ro")
feat_work_2 <-
  get_feat_instances(spotify_songs_work, "SWV", "Wu-Tang Clan")

# Slope chart.
ggplot(data = feat_work, aes(x = track_artist, y = danceability, group = group)) +
  geom_line(aes(alpha = 1), size = 2) +
  geom_point(aes(alpha = 1), size = 4) +
  scale_x_discrete(position = "top")
