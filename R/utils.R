library(tidyverse)

#' @export
filter_months <- function(df_swims, year) {
  df_swims |>
    filter(year(activity_date) == year) |>
    mutate(month = month(activity_date, label = TRUE)) |>
    pull() |>
    unique() |>
    sort()
}

#' @export
df_swims_month <- function(df_swims, year, month) {
  df_swims |>
    filter(
      year(activity_date) == year,
      month(activity_date, label = TRUE) == month
    )
}

#' @export
get_num_sessions <- function(df_swims, year, month) {
  nrow(df_swims_month(df_swims, year, month))
}

#' @export
get_dist_swam <- function(df_swims, year, month) {
  df_month <- df_swims_month(df_swims, year, month)
  dist <- sum(df_month$distance)
  dist_format <- scales::comma(dist)
  paste0(dist_format, " meters")
}

#' @export
get_avg_pace <- function(df_swims, year, month) {
  df_month <- df_swims_month(df_swims, year, month)
  avg_pace <- 100 * sum(df_month$moving_time) / (sum(df_month$distance) * 60)
  minutes <- floor(avg_pace)
  seconds <- round((avg_pace - minutes) * 60)
  paste0(minutes, ":", sprintf("%02d", seconds))
}
