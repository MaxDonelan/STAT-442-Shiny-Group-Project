# Runs once when the app starts (sourced from app.R), not per user session.
# Data prep for the Interactive Map tab (server_interactive_map.R).

# Crashes with valid coordinates, plus the hour as a number (0-23) so the
# hour slider can filter it directly. Coordinates are rounded to 5 decimals
# (about 1 m), which shrinks what is sent to the browser.
im_crashes <- accident_cleaned |>
  filter(
    between(Latitude, -90, 90),
    between(Longitude, -180, 180)
  ) |>
  mutate(
    Latitude  = round(Latitude, 5),
    Longitude = round(Longitude, 5),
    hour_num  = as.integer(Hour) - 1L
  )
