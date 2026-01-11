theme_swim_stats <- function(base_family = "Inter", base_size = 14) {
  theme_minimal(base_family = base_family, base_size = base_size) +
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
        family = "Fira Code"
      )
    )
}
