library(tidyverse)
library(hrbrthemes)
library(here)

polls <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv'
  )
rankings <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv'
  )

critic_names <- polls %>% group_by(critic_name) %>% tally()
critic_names %>% distinct(n)

glimpse(polls)

df <-
  polls %>% group_by(critic_name) %>% summarise(min_year = min(year), max_year = max(year))
df <-
  df %>% mutate(
    critic_name = factor(critic_name),
    critic_name = factor(critic_name, levels = rev(levels(critic_name)))
  )

df

# Source: https://stackoverflow.com/questions/32123288/position-ggplot-text-in-each-corner
# annotations <- data.frame(
#   xpos = c(-Inf, -Inf, Inf, Inf),
#   ypos =  c(-Inf, Inf, -Inf, Inf),
#   annotateText = c(
#     "Bottom Left (h0,v0)",
#     "Top Left (h0,v1)",
#     "Bottom Right (h1,v0)",
#     "Top Right (h1,v1)"
#   ),
#   hjustvar = c(0, 0, 1, 1),
#   vjustvar = c(0, 1, 0, 1)
# )
annotations <- data.frame(
  xpos = c(-Inf, Inf),
  ypos =  c(Inf,-Inf),
  annotateText = c("Critics", "Release Year"),
  hjustvar = c(0, 1),
  vjustvar = c(1, 0) # -0.05
)

# Title
# Highlight shortest and longest
# Margins
# Font
# Color
# Gridlines

ggplot(df) +
  geom_segment(aes(
    x = critic_name,
    xend = critic_name,
    y = min_year,
    yend = max_year
  ),
  color = "grey") +
  geom_point(
    aes(x = critic_name, y = min_year),
    color = rgb(0.2, 0.7, 0.1, 1.0),
    size = 1
  ) +
  geom_point(
    aes(x = critic_name, y = max_year),
    color = rgb(0.7, 0.2, 0.1, 0.5),
    size = 1
  ) +
  geom_label(
    data = annotations,
    colour = "#CCCCCC",
    fill = "white",
    family = "Roboto Condensed",
    fontface = "plain",
    label.padding =  unit(0.1, "lines"),
    label.r =  unit(0, "lines"),
    label.size = 0,
    size = 11.5 / .pt,
    aes(
      y = xpos,
      x = ypos,
      hjust = hjustvar,
      vjust = vjustvar,
      label = annotateText
    )
  ) +
  coord_flip() +
  theme_ipsum_rc() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )

# A3 (vertical)
ggsave(
  here(
    "2020-04-14",
    paste0(
      "best-rap-artists-plot-",
      format(Sys.time(), "%Y%m%d_%H%M"),
      ".png"
    )
  ),
  width = 297,
  height = 420,
  units = "mm"
)
