# Runs once when the app starts (sourced from app.R).
# Plot styling helpers shared by tabs whose plots follow the dark mode toggle.

# Text color that matches the app's light/dark mode (dark is the default)
plot_foreground <- function(mode) {
  if (identical(mode, "light")) "#212529" else "#DEE2E6"
}

# Transparent plot, panel and legend backgrounds so plots take on the color of
# the card they sit in. Use with renderPlot(..., bg = "transparent").
theme_transparent <- function() {
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA)
  )
}
