# Use VMT to compare rural vs. urban driving
# Compare population, VMT, and fatal crash rates

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

# Load datasets
VMT <- read.csv("VMT_2024.csv")
accidents <- read.csv("accident.csv")
R_U_2024_pop <- read.csv("rural_urban_population_2024.csv")

# Clean state names
accidents$STATENAME <- trimws(as.character(accidents$STATENAME))
VMT$State <- trimws(as.character(VMT$State))

# Remove Fclass if you aren't analyzing road classifications
VMT <- VMT %>%
  select(-any_of("Fclass"))

# Convert VMT to numeric
VMT$VMT <- as.numeric(gsub(",", "", as.character(VMT$VMT)))

# Filter for 2024 if a Year column exists
if ("Year" %in% names(VMT)) {
  VMT <- VMT %>%
    filter(as.numeric(as.character(Year)) == 2024)
}

#code to make ruraly vs urban percents/dist, show total VMT by state, look at fatal crash rates by r v u
Population_by_state <- R_U_2024_pop %>%
  select(State, Rural_Pct, Urban_Pct)

VMT_by_state <- VMT %>%
  group_by(State, Area) %>%
  summarise(
    VMT = sum(VMT, na.rm = TRUE),
    .groups = "drop"
  )

Total_VMT_by_state <- VMT_by_state %>%
  group_by(State) %>%
  summarise(
    Total_VMT = sum(VMT, na.rm = TRUE),
    .groups = "drop"
  )

VMT_percentages <- VMT_by_state %>%
  group_by(State) %>%
  mutate(
    Total_VMT = sum(VMT, na.rm = TRUE),
    VMT_Pct = VMT / Total_VMT * 100
  ) %>%
  ungroup()

Fatal_crashes_by_state <- accidents %>%
  mutate(
    Area = case_when(
      grepl("rural", RUR_URBNAME, ignore.case = TRUE) ~ "Rural",
      grepl("urban", RUR_URBNAME, ignore.case = TRUE) ~ "Urban",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Area)) %>%
  group_by(STATENAME, Area) %>%
  summarise(
    Fatal_Crashes = n(),
    .groups = "drop"
  )

Crash_rates <- Fatal_crashes_by_state %>%
  left_join(
    VMT_by_state,
    by = c("STATENAME" = "State", "Area")
  ) %>%
  mutate(
    Fatal_Crash_Rate =
      Fatal_Crashes / VMT * 100000000
  )

# VMT column names
names(VMT)

# Rural/urban categories in VMT
unique(VMT$Area)

# Accident column names
names(accidents)

# Rural/urban categories in accidents
unique(accidents$RUR_URBNAME)

#code to graph/map the side by side comparison + make it interactive
#toggle 1 = population dist
#toggle 2 = total vmt by state
# toggle 3 = rates with state level interaction when using the 3rd graph





#extra code to do some fun analysis if it comes to mind 