# ui for the interactive map tab

nav_panel(
  title = "Interactive Map",
  icon = icon("fa-regular fa-map"),
  layout_sidebar(
    sidebar = "Eventually I'll add some options for filtering here",
    leafletOutput("leaflet_map")
  )
)