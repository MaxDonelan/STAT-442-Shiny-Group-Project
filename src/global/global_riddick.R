# Runs once when the app starts (sourced from app.R), not per user session.
# Data prep for the Rural vs. Urban (server_riddick.R) tab; the matching server file only holds reactives and outputs.
# ==================================================
# 1. PREPARE MAP DATA
# ==================================================

# Join population and VMT information to the state map with Alaska and Hawaii
# moved below the lower 48 (next to Texas) for the static maps. The shifted
# outlines are pre-built by src/unsourced/prepare_app_data.R.
map_data <- us_states_shifted |>
  left_join(
    State_comparison,
    by = c("NAME" = "State")
  )


# Reshape fatal crash rates into rural and urban columns
crash_rate_data <- Crash_rates |>
  select(State, Area, Fatal_Crash_Rate) |>
  pivot_wider(
    names_from = Area,
    values_from = Fatal_Crash_Rate
  ) |>
  mutate(
    Rate_Difference = Rural - Urban
  )


# Join crash rates to state map
crash_map_data <- us_states |>
  left_join(
    crash_rate_data,
    by = c("NAME" = "State")
  )
