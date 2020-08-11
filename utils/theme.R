COLORS <-
  list(
    b = "#44475A",
    w = "#FFFFFF",
    dg = "#4A5568",
    lg = "#B0BFC7"
  )

base_theme <-
  function(base_size = 11.5,
           axis_title_size = 9,
           axis_text_size = base_size,
           axis_color = COLORS$b,
           tick_color = COLORS$b,
           axis_text_color = COLORS$b) {
    half_base_size <- base_size / 2

    theme(
      panel.background = element_blank(),

      axis.line = element_line(color = axis_color, size = 0.15),

      axis.ticks = element_line(color = axis_color, size = 0.15),
      axis.ticks.length = unit(half_base_size, "pt"),

      axis.text = element_text(color = axis_text_color, size = axis_text_size),
      axis.text.x = element_text(
        margin = margin(t = 0.8 * half_base_size / 2),
        vjust = 1
      ),
      axis.text.x.top = element_text(
        margin = margin(b = 0.8 * half_base_size / 2),
        vjust = 0
      ),
      axis.text.y = element_text(
        margin = margin(r = 0.8 * half_base_size / 2),
        hjust = 1
      ),
      axis.text.y.right = element_text(
        margin = margin(l = 0.8 * half_base_size / 2),
        hjust = 0
      ),
      
      axis.title = element_text(size = axis_title_size),
      axis.title.x = element_text(
        margin = margin(t = half_base_size / 2),
        vjust = 1
      )
    )
  }
