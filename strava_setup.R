library(rStrava)

my_token <- httr::config(
  token = strava_oauth(
    app_name = "swim-stats",
    app_client_id = Sys.getenv("STRAVA_CLIENT_ID"),
    app_secret = Sys.getenv("STRAVA_CLIENT_SECRET"),
    app_scope = "activity:read_all",
    cache = TRUE
  )
)
