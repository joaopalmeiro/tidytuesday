library(tidyverse)
library(ggtext)
library(here)
library(hrbrthemes)

penguins <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv"
  )

# geom_flat_violin()
source(
  "https://gist.githubusercontent.com/joaopalmeiro/a1935c6bcc172bcf32f04559259b2c4a/raw/10eb18d60b250854cadaeb3090c044272f345f95/geom_flat_violin.R"
)

head(penguins)

penguins %>%
  group_by(species) %>%
  tally(sort = TRUE)

penguins %>%
  group_by(island) %>%
  tally(sort = TRUE)

penguins %>% drop_na(body_mass_g) %>%
  ggplot(aes(x = body_mass_g, y = species, fill = species)) +
  geom_violin(position = position_nudge(x = 0, y = .2), alpha = .8) +
  geom_point(
    aes(x = body_mass_g, color = species),
    position = position_jitter(height = .15),
    size = .5,
    alpha = .8
  ) +
  geom_boxplot(width = .1,
               alpha = .5,
               outlier.shape = NA) +
  theme_ipsum_rc(grid = "X", axis = "x") +
  theme(legend.position = "none")

# Raincloud plot: boxplot + raw jittered data + split-half violin plot
# Source: https://orchid00.github.io/tidy_raincloudplot and https://micahallen.org/2018/03/15/introducing-raincloud-plots/
rc_plot <-
  penguins %>% drop_na(body_mass_g) %>%
  ggplot(aes(x = body_mass_g, y = species, fill = species)) +
  geom_flat_violin(position = position_nudge(x = 0, y = .2), alpha = .8) +
  # geom_flat_violin(position = position_nudge(x = 0, y = .2), alpha= .8, draw_quantiles = c(0.25, 0.5, 0.75)) +
  # geom_violin(position = position_nudge(x = 0, y = .2), alpha= .8) +
  geom_point(
    aes(x = body_mass_g, color = species),
    position = position_jitter(height = .15),
    size = .5,
    alpha = .8
  ) +
  geom_boxplot(width = .1,
               alpha = .5,
               outlier.shape = NA) +
  theme_ipsum_rc(grid = "X", axis = "x") +
  theme(legend.position = "none")

rc_plot
