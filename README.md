# STAT-442-Shiny-Group-Project

This repository has the code for the STAT 442 Group Shiny App project.

Published [here](https://01a1cc30-9eaa-e1d3-8b95-848454a470a6.share.connect.posit.cloud/)

## App Tabs

All data comes from the NHTSA Fatality Analysis Reporting System (FARS). Tabs use 2024 data unless noted otherwise.

### Interactive Map
A Leaflet map of every fatal crash in 2024, with nearby crashes grouped into clusters that split apart as you zoom in. The sidebar filters crashes by number of fatalities, hour of day, date range, weather, and light conditions, and a Reset button restores the defaults. Click an individual crash point to see its details. Crashes with an unknown time only appear when all hours are selected.

### Male V.S. Female
Compares traffic fatalities by sex for 2023, 2024, or both years combined. Three charts show the male/female split overall, within each road classification, and in rural vs. urban areas. A checkbox narrows the data from all fatalities (drivers, passengers, and pedestrians) to drivers only. Records with unknown sex are excluded.

### Analysis by State
A choropleth map of U.S. states that can be colored by rate per 100,000 people or by percentage. Filters choose the year (2023 or 2024), time of day (morning, afternoon, night), crash type (all, impaired, or distracted), and whether to count crashes or fatalities.

### Seasons
Shows how fatal crashes and fatalities change over the year, for the whole country or one selected state. Summary value boxes, a season-by-season comparison chart, a monthly trend chart, and a table of season totals are included. Seasons are defined as winter (Dec–Feb), spring (Mar–May), summer (Jun–Aug), and fall (Sep–Nov); counts are not adjusted for traffic volume or season length.

### Survival Analysis
A clustered map of people involved in fatal crashes. As you pan and zoom, the sidebar updates survival statistics (survival rate = survivors / people involved) for whatever area is currently visible. Crashes with unknown locations are left off the map.

### State Survival Comparison
A horizontally scrolling interactive bar chart that compares survivors and deaths for each state. States are grouped by region, with regions and the states within them ranked from highest to lowest survival rate. Bar heights show the actual number of people involved. These rates describe people in recorded fatal crashes, not the chance of surviving all traffic accidents.

### Rural vs. Urban
Pick one of three views: side-by-side maps of each state's rural and urban population share, a map of total vehicle miles traveled (VMT) by state, or an interactive map of rural and urban fatal crash rates per 100 million VMT. Hover over a state in the crash-rate map to compare its rural and urban rates; a positive difference means the rural rate is higher.
