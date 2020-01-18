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
levenshtein.distance <- function(df, col, word) {
  distances <- mapply(adist,
                      df[[col]],
                      word,
                      MoreArgs = list(partial = FALSE, ignore.case = FALSE))
  
  passwordcol <- factor(df[[col]], levels = df[[col]])
  
  distancesdf <-
    bind_cols(password = passwordcol, distance = distances)
  
  topndf <- arrange(distancesdf, desc(distance))
  
  topndf$password <-
    factor(topndf$password, levels = topndf$password)
  
  return(topndf)
}

password.test <-
  levenshtein.distance(passwordswork, "password", "password")
print(password.test, n = 10)

count.equal.last.distance <- function(df, col, topn) {
  colref <- df[[col]]
  distanceref <- colref[topn]
  
  counttop <- sum(colref[1:topn] == distanceref)
  counttotal <- sum(colref == distanceref)
  
  return(counttotal - counttop)
}

count.equal.last.distance(password.test, "distance", 10)

clean.style <- function() {
  fonttitle <- "Roboto"
  fontsubtitle <- "Roboto Thin"
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(family = fonttitle,
                                       color = "#2F2F2F"),
    plot.subtitle = ggplot2::element_text(
      family = fontsubtitle,
      size = 8,
      color = "#2F2F2F"
    ),
    axis.title = ggplot2::element_text(
      family = fontsubtitle,
      size = 8,
      color = "#2F2F2F"
    ),
    axis.text = ggplot2::element_text(family = fonttitle,
                                      color = "#2F2F2F"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank()
  )
}

get.plot <- function(df, col.pass, col.distance, word, topn) {
  df.pre.plot <- levenshtein.distance(df, col.pass, word)
  
  count <-
    count.equal.last.distance(df.pre.plot, col.distance, topn)
  
  df.to.plot <- df.pre.plot[1:topn,]
  
  max.distance <- max(df.to.plot[[col.distance]])
  min.distance <- min(df.to.plot[[col.distance]])
  
  title <-
    bquote(
      "Top" ~ bold(.(toString(topn))) ~ "of the longest Levenshtein distances between" ~
        bold(.(word)) ~ "and common passwords."
    )
  
  subtitle <-
    bquote(
      "There are more" ~ .(toString(count)) ~ "passwords with a Levenshtein distance equal to" ~
        .(toString(min.distance)) ~ "."
    )
  
  bar.plot <-
    ggplot(df.to.plot, aes_string(x = col.pass, y = col.distance)) +
    geom_bar(stat = "identity",
             fill = "#DFCDC3") +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, max.distance),
      breaks = seq(0, max.distance, 1)
    ) +
    list(theme_minimal() + clean.style()) +
    geom_hline(yintercept = 0,
               size = 0.5,
               colour = "#2F2F2F") +
    geom_hline(yintercept = max.distance,
               size = 0.5,
               colour = "#2F2F2F") +
    labs(
      title = title,
      subtitle = "",
      x = "Password",
      y = "Levenshtein distance"
    )
  
  if (count > 0) {
    bar.plot <- bar.plot + labs(subtitle = subtitle)
  }
  
  return(bar.plot)
}

get.plot(passwordswork, "password", "distance", "p", 10)

ggsave("passwords.png")
