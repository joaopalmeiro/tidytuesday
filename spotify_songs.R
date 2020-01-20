library(tidyverse)

# Dataset.
spotify_songs <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
spotify_songs

glimpse(spotify_songs)

# Drop rows containing missing values.
spotify_songs_work <- spotify_songs %>% drop_na()
spotify_songs_work

glimpse(spotify_songs_work)

# Duplicated rows.
spotify_songs_work[duplicated(spotify_songs_work$track_id), ]
spotify_songs_work[duplicated(spotify_songs_work$track_id), ][1, 1]

# Same track, different playlists.
check_track_id <- spotify_songs_work[spotify_songs_work$track_id == "1HfMVBKM75vxSfsQ5VefZ5", ]

# Drop duplicated rows.
spotify_songs_work <- spotify_songs_work[!duplicated(spotify_songs_work$track_id), ]
glimpse(spotify_songs_work)
