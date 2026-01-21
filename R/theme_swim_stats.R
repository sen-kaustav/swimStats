library(ggplot2)

theme_swim_stats <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.margin = margin(l = 5, r = 5, b = 10),
      axis.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.line.x = element_line(linewidth = 0.5, color = "grey30"),
      axis.text.y = element_blank(),
      axis.text.x = element_text(
        size = rel(1),
        face = "bold",
        family = "Num Font"
      )
    )
}
