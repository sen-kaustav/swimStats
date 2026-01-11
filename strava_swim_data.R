library(rStrava)
library(tidyverse)
library(scales)

token <- httr::config(token = readRDS(".httr-oauth")[[1]])

my_activities <- get_activity_list(token)

df_activity <- compile_activities(my_activities) |>
  as_tibble()

df_swims <-
  df_activity |>
  filter(sport_type == "Swim") |>
  mutate(
    activity_date = date(ymd_hms(start_date_local)),
    distance = distance * 1000 # convert to meters
  ) |>
  select(
    name,
    activity_date,
    moving_time,
    distance
  ) |>
  summarise(
    across(.cols = c(moving_time, distance), .fns = sum),
    .by = c(name, activity_date)
  )

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

plot_monthly_swim_totals <- function(df_swims, year) {
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

plot_monthly_swim_totals(df_swims, year = 2025)


date_start <- ymd("20260101")
date_end <- ceiling_date(date_start, unit = "month") - 1

df_swims_month <-
  df_swims |>
  filter(year(activity_date) == 2026, month(activity_date) == 01) |>
  arrange(activity_date)

df_test <-
  tibble(
    date = seq(date_start, date_end, by = "1 day")
  ) |>
  mutate(
    day = wday(date, label = TRUE, week_start = 1),
    day_of_month = day(date),
    week_of_month = ceiling(
      (day(date) + wday(date_start, week_start = 1) - 1) / 7
    )
  )

df_test <-
  df_test |>
  left_join(df_swims_month, by = join_by(date == activity_date))

ggplot(mapping = aes(day, week_of_month)) +
  geom_point(
    data = filter(df_test, is.na(name)),
    size = 20,
    alpha = 0.4,
    color = "grey80"
  ) +
  geom_point(
    data = filter(df_test, !is.na(name)),
    size = 20,
    alpha = 0.6,
    color = "indianred1"
  ) +
  geom_text(
    data = filter(df_test, is.na(name)),
    aes(label = day_of_month),
    color = "grey80",
    size = 5,
    fontface = "bold",
    family = "Fira Code"
  ) +
  geom_text(
    data = filter(df_test, !is.na(name)),
    aes(label = day_of_month),
    color = "indianred3",
    size = 5,
    fontface = "bold",
    family = "Fira Code"
  ) +
  scale_y_reverse(expand = expansion(mult = 0.15)) +
  scale_x_discrete(position = "top") +
  theme_swim_stats() +
  theme(
    plot.margin = margin(t = 10, b = 10),
    axis.line.x = element_blank(),
    axis.text.x = element_text(size = rel(1.5))
  )

# mutate(
#   start_date_local = ymd_hms(start_date_local),
#   distance = distance * 1000, # convert to meters
#   moving_time = moving_time / 60, # convert to minutes
#   pace = 360 / average_speed, # pace = seconds per 100m
#   pace_fmt = seconds_to_period(pace),
#   activity_year = year(start_date_local),
#   activity_month = month(start_date_local),
#   activity_day = day(start_date_local)
# )

# speed = kms / hr

# pace = secs per 100m
