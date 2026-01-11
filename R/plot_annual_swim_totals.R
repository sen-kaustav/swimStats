plot_annual_swim_totals <- function(df_swims, year) {
  date_start <- ymd(paste0(year, "0101"))
  date_end <- ceiling_date(date_start, unit = "year") - 1
  date_breaks <- seq(date_start, date_end, by = "1 month")

  df_plot <-
    df_swims |>
    mutate(
      activity_month = floor_date(activity_date, unit = "month"),
      activity_year = year(activity_date)
    ) |>
    filter(activity_year == year) |>
    summarise(tot_swim = sum(distance), .by = activity_month)

  plot <- df_plot |>
    ggplot(aes(activity_month)) +
    geom_segment(
      aes(y = 0, yend = tot_swim),
      color = "steelblue",
      alpha = 0.8,
      linewidth = 20
    ) +
    geom_text(
      aes(y = tot_swim, label = comma(tot_swim, accuracy = 1)),
      vjust = 0,
      nudge_y = 100,
      size = 3.5
    ) +
    scale_x_date(
      limits = c(date_start, date_end),
      breaks = date_breaks,
      date_labels = "%b",
      expand = expansion(mult = c(0.05, 0))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_swim_stats()

  plot
}
