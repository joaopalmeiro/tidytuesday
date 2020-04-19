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
  polls %>% group_by(critic_name) %>% summarise(
    min_year = min(year),
    max_year = max(year),
    time_span = max_year - min_year
  )
df <-
  df %>% mutate(
    critic_name = factor(critic_name),
    critic_name = factor(critic_name, levels = rev(levels(critic_name)))
  )
df

time_span_count <- df %>% group_by(time_span) %>% tally()
time_span_count

highlight <-
  df %>% filter(time_span == min(time_span) |
                  time_span == max(time_span))
highlight

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

chart_colors <-
  list(
    main = "#4A5568",
    white = "white",
    light_grey = "#EBEBEB",
    dark_grey = "#CCCCCC",
    min = "#DD7373",
    max = "#B33951"
  )

ggplot(df) +
  geom_segment(
    aes(
      x = critic_name,
      xend = critic_name,
      y = min_year,
      yend = max_year
    ),
    color = chart_colors$main,
    size = 0.5,
    alpha = 1.0
  ) +
  geom_segment(
    data = highlight,
    aes(
      x = critic_name,
      xend = critic_name,
      y = min_year,
      yend = max_year
    ),
    color = chart_colors$main,
    size = 0.5,
    alpha = 1.0
  ) +
  geom_point(
    aes(x = critic_name, y = min_year),
    color = chart_colors$min,
    size = 1,
    alpha = 1
  ) +
  geom_point(
    aes(x = critic_name, y = max_year),
    color = chart_colors$max,
    size = 1,
    alpha = 1
  ) +
  geom_label(
    data = annotations,
    colour = paste0(chart_colors$main, "4D"),
    fill = chart_colors$white,
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
    axis.title.y = element_blank(),
    axis.text = element_text(color = chart_colors$main),
    panel.grid.major = element_line(color = chart_colors$light_grey),
    panel.grid.minor = element_line(color = chart_colors$light_grey),
    plot.margin = margin(0, 0, 0, 0)
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
