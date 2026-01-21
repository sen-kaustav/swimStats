library(tidyverse)

plot_monthly_swim_calendar <- function(df_swims, year, month) {
  df_swims_month <- df_swims_month(df_swims, year, month) |>
    summarise(across(where(is.numeric), sum), .by = activity_date) |>
    arrange(activity_date)

  month_fmt <- stringr::str_pad(month, width = 2, side = "left", pad = 0)

  date_start <- ymd(paste0(year, month_fmt, "01"))
  date_end <- ceiling_date(date_start, unit = "month") - 1

  df_plot <- tibble(date = seq(date_start, date_end, by = "1 day")) |>
    mutate(
      day = wday(date, label = TRUE, week_start = 1),
      day_of_month = day(date),
      week_of_month = ceiling(
        (day(date) + wday(date_start, week_start = 1) - 1) / 7
      )
    ) |>
    left_join(df_swims_month, by = join_by(date == activity_date))

  plot <-
    ggplot(mapping = aes(day, week_of_month)) +
    geom_point(
      data = filter(df_plot, is.na(distance)),
      size = 12,
      alpha = 0.4,
      color = "grey80"
    ) +
    geom_point(
      data = filter(df_plot, !is.na(distance)),
      size = 12,
      alpha = 0.6,
      color = "indianred1"
    ) +
    geom_text(
      data = filter(df_plot, is.na(distance)),
      aes(label = day_of_month),
      color = "grey80",
      size = 5,
      fontface = "bold",
      family = "Num Font"
    ) +
    geom_text(
      data = filter(df_plot, !is.na(distance)),
      aes(label = day_of_month),
      color = "indianred3",
      size = 5,
      fontface = "bold",
      family = "Num Font"
    ) +
    scale_y_reverse(expand = expansion(mult = 0.1)) +
    scale_x_discrete(position = "top") +
    # labs(title = glue::glue("{month} {year}")) +
    theme_swim_stats() +
    theme(
      plot.margin = margin(b = 10),
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        color = "grey70",
        margin = margin(b = 10)
      ),
      axis.line.x = element_blank(),
      axis.text.x = element_text(size = rel(1.5))
    )

  plot
}
