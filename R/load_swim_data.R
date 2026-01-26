library(tidyverse)

load_swim_data <- function() {
  df_activity <- read_csv(
    "https://raw.githubusercontent.com/sen-kaustav/stravaData/refs/heads/main/swim_data.csv"
  )

  df_swims <-
    df_activity |>
    filter(sport_type == "Swim") |>
    mutate(
      activity_date = lubridate::date(ymd_hms(start_date_local)),
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

  df_swims
}
