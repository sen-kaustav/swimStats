library(tidyverse)

plot_monthly_compare <- function(df_swims, year, month) {
  curr_date <- ymd(paste(year, month, 1, sep = "-"))
  prev_date <- curr_date - months(1)

  month_dates <-
    tibble(
      date = c(
        seq(curr_date, curr_date + months(1) - days(1), by = "day"),
        seq(prev_date, prev_date + months(1) - days(1), by = "day")
      )
    ) |>
    filter(date <= Sys.Date())

  month_swims <-
    df_swims |>
    mutate(
      activity_year = year(activity_date),
      activity_month = month(activity_date)
    ) |>
    filter(
      (activity_year == year(curr_date) & activity_month == month(curr_date)) |
        (activity_year == year(prev_date) & activity_month == month(prev_date))
    ) |>
    select(activity_date, distance)

  plot_data <-
    month_dates |>
    left_join(month_swims, by = join_by(date == activity_date)) |>
    mutate(
      distance = replace_na(distance, 0),
      month = if_else(month(date) == month(curr_date), "Current", "Previous"),
      month = factor(month, levels = c("Previous", "Current")),
      day = day(date)
    ) |>
    group_by(month) |>
    mutate(distance_cum = cumsum(distance))

  ggplot(plot_data, aes(day, distance_cum, colour = month)) +
    geom_step(size = 1.5) +
    geom_point(
      data = filter(plot_data, month == "Previous") |> slice_tail(n = 1),
      size = 4
    ) +
    geom_point(
      data = filter(plot_data, month == "Current") |> slice_tail(n = 1),
      size = 4
    ) +
    scale_x_continuous(limits = c(1, 31), breaks = seq(1, 31, 2)) +
    scale_y_continuous(
      name = "Cumulative Distance (in meters)",
      labels = scales::number_format(scale = 1 / 1000, suffix = "k"),
      sec.axis = dup_axis(name = ""),
      limits = c(0, NA)
    ) +
    scale_color_manual(
      values = c("Current" = "indianred1", "Previous" = "grey85")
    ) +
    theme_minimal(base_family = "Num Font", base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_text(
        size = rel(0.8),
        face = "bold",
        color = "grey70",
        margin = margin(r = 10, l = 5)
      )
    )
}
