
# ============================================================
# Data Cleaning: National Fatal Crash Speed Analysis
# Years: 2023 and 2024
# Author: Thomas Shields
#
# Purpose:
#   1. Combine FARS accident, vehicle, and person datasets
#   2. Include all US states
#   3. Identify drivers involved in fatal crashes
#   4. Connect driver sex to vehicle speed
#   5. Prepare road speed limit and road classification data
#   6. Save cleaned datasets for the Shiny application
# ============================================================

# ------------------------------------------------------------
# 1. Load libraries
# ------------------------------------------------------------

library(dplyr)


# ------------------------------------------------------------
# 2. Set up file paths
# ------------------------------------------------------------

years <- c(2023, 2024)

# All raw files should be located in the data folder.
# Expected filenames:
#
# accident_2023.csv
# accident_2024.csv
# vehicle_2023.csv
# vehicle_2024.csv
# person_2023.csv
# person_2024.csv

# Create data folder if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data")
}


# ------------------------------------------------------------
# 3. Function to read and standardize FARS datasets
# ------------------------------------------------------------

read_fars <- function(dataset, year) {
  
  file_path <- file.path(
    "data",
    paste0(dataset, "_", year, ".csv")
  )
  
  if (!file.exists(file_path)) {
    stop(
      paste("Missing file:", file_path)
    )
  }
  
  data <- read.csv(
    file_path,
    stringsAsFactors = FALSE
  )
  
  # Standardize column names
  names(data) <- toupper(names(data))
  
  # Ensure consistent crash identification
  if (!"ST_CASE" %in% names(data)) {
    stop(
      paste("ST_CASE is missing from:", file_path)
    )
  }
  
  data <- data %>%
    mutate(
      YEAR = year,
      ST_CASE = as.character(ST_CASE)
    )
  
  # Standardize vehicle number when available
  if ("VEH_NO" %in% names(data)) {
    data <- data %>%
      mutate(
        VEH_NO = as.character(VEH_NO)
      )
  }
  
  data
}


# ------------------------------------------------------------
# 4. Read and combine accident datasets
# ------------------------------------------------------------

accidents <- bind_rows(
  lapply(years, function(y) {
    read_fars("accident", y)
  })
)

# Convert important accident variables

accidents <- accidents %>%
  mutate(
    FATALS = as.numeric(FATALS),
    VE_TOTAL = as.numeric(VE_TOTAL),
    PERSONS = as.numeric(PERSONS)
  )

# Keep all states.
# No South Dakota filtering is performed.


# ------------------------------------------------------------
# 5. Read and combine vehicle datasets
# ------------------------------------------------------------

vehicles <- bind_rows(
  lapply(years, function(y) {
    read_fars("vehicle", y)
  })
)

# Required vehicle variables

required_vehicle_columns <- c(
  "ST_CASE",
  "VEH_NO",
  "TRAV_SP",
  "VSPD_LIM"
)

missing_vehicle_columns <- setdiff(
  required_vehicle_columns,
  names(vehicles)
)

if (length(missing_vehicle_columns) > 0) {
  stop(
    paste(
      "Missing vehicle columns:",
      paste(missing_vehicle_columns, collapse = ", ")
    )
  )
}

vehicles <- vehicles %>%
  mutate(
    TRAV_SP = as.numeric(TRAV_SP),
    VSPD_LIM = as.numeric(VSPD_LIM)
  )


# ------------------------------------------------------------
# 6. Read and combine person datasets
# ------------------------------------------------------------

persons <- bind_rows(
  lapply(years, function(y) {
    read_fars("person", y)
  })
)

# Required person variables

required_person_columns <- c(
  "ST_CASE",
  "VEH_NO",
  "PER_TYP",
  "SEX"
)

missing_person_columns <- setdiff(
  required_person_columns,
  names(persons)
)

if (length(missing_person_columns) > 0) {
  stop(
    paste(
      "Missing person columns:",
      paste(missing_person_columns, collapse = ", ")
    )
  )
}

persons <- persons %>%
  mutate(
    PER_TYP = as.numeric(PER_TYP),
    SEX = as.numeric(SEX)
  )


# ------------------------------------------------------------
# 7. Identify drivers
# ------------------------------------------------------------

# FARS PER_TYP = 1 identifies drivers.

drivers <- persons %>%
  filter(PER_TYP == 1) %>%
  mutate(
    SEX_LABEL = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      TRUE ~ "Unknown"
    )
  )


# ------------------------------------------------------------
# 8. Clean vehicle speed variables
# ------------------------------------------------------------

vehicles <- vehicles %>%
  mutate(
    TRAV_SP_STATUS = case_when(
      TRAV_SP == 0 ~ "Stopped",
      TRAV_SP >= 1 & TRAV_SP <= 151 ~ "Reported",
      TRAV_SP == 997 ~ "Over 151 mph",
      TRAV_SP == 998 ~ "Not Reported",
      TRAV_SP == 999 ~ "Unknown",
      TRUE ~ "Other / Missing"
    ),
    
    TRAV_SP_ANALYSIS = case_when(
      TRAV_SP >= 1 & TRAV_SP <= 151 ~ TRAV_SP,
      TRUE ~ NA_real_
    )
  )

# ------------------------------------------------------------
# 9. Combine vehicle and driver information
# ------------------------------------------------------------

# Join using:
# YEAR + ST_CASE + VEH_NO
#
# This identifies the driver associated with each vehicle.

driver_vehicles <- vehicles %>%
  inner_join(
    drivers,
    by = c("YEAR", "ST_CASE", "VEH_NO"),
    suffix = c("_VEH", "_PER")
  )


# ------------------------------------------------------------
# 10. Add accident information
# ------------------------------------------------------------

# Select useful accident-level variables.
# any_of() allows the script to work if optional
# variables are absent from a particular dataset.

accident_info <- accidents %>%
  select(
    any_of(c(
      "YEAR",
      "ST_CASE",
      "STATE",
      "FATALS",
      "VE_TOTAL",
      "PERSONS",
      "COUNTY",
      "ROUTE",
      "FUNC_SYS",
      "RUR_URB",
      "LATITUDE",
      "LONGITUD"
    ))
  )

# Connect driver/vehicle information to crash information

driver_speed_thomas <- driver_vehicles %>%
  left_join(
    accident_info,
    by = c("YEAR", "ST_CASE"),
    suffix = c("", "_ACC")
  )


# ------------------------------------------------------------
# 11. Add analysis-friendly variables
# ------------------------------------------------------------

driver_speed_thomas <- driver_speed_thomas %>%
  mutate(
    YEAR = as.integer(YEAR),
    
    # Road classification
    ROAD_TYPE = case_when(
      FUNC_SYS == 1 ~ "Interstate",
      FUNC_SYS == 2 ~ "Other Freeway / Expressway",
      FUNC_SYS == 3 ~ "Other Principal Arterial",
      FUNC_SYS == 4 ~ "Minor Arterial",
      FUNC_SYS == 5 ~ "Major Collector",
      FUNC_SYS == 6 ~ "Minor Collector",
      FUNC_SYS == 7 ~ "Local Road",
      TRUE ~ NA_character_
    ),
    
    # Rural vs. urban
    AREA_TYPE = case_when(
      RUR_URB == 1 ~ "Rural",
      RUR_URB == 2 ~ "Urban",
      TRUE ~ NA_character_
    )
  )

# ------------------------------------------------------------
# 12. Create summary datasets
# ------------------------------------------------------------

# Driver records by sex and year

driver_sex_summary_thomas <- driver_speed_thomas %>%
  group_by(YEAR, SEX_LABEL) %>%
  summarise(
    DRIVER_COUNT = n(),
    .groups = "drop"
  )


# Road classification by sex and year


road_sex_summary_thomas <- driver_speed_thomas %>%
  filter(
    !is.na(ROAD_TYPE),
    SEX_LABEL %in% c("Female", "Male")
  ) %>%
  count(YEAR, ROAD_TYPE, SEX_LABEL) %>%
  group_by(YEAR, ROAD_TYPE) %>%
  mutate(
    PERCENTAGE = n / sum(n) * 100
  ) %>%
  ungroup()


# Rural vs. urban by sex and year

rural_urban_summary_thomas <- driver_speed_thomas %>%
  filter(
    !is.na(AREA_TYPE),
    SEX_LABEL %in% c("Female", "Male")
  ) %>%
  count(YEAR, SEX_LABEL, AREA_TYPE) %>%
  group_by(YEAR, SEX_LABEL) %>%
  mutate(
    PERCENTAGE = n / sum(n) * 100
  ) %>%
  ungroup()



# ------------------------------------------------------------
# 13. Save cleaned datasets
# ------------------------------------------------------------

saveRDS(
  accidents,
  "data/accidents_thomas.rds"
)

saveRDS(
  vehicles,
  "data/vehicles_thomas.rds"
)

saveRDS(
  persons,
  "data/persons_thomas.rds"
)

saveRDS(
  driver_speed_thomas,
  "data/driver_speed_thomas.rds"
)

saveRDS(
  driver_sex_summary_thomas,
  "data/driver_sex_summary_thomas.rds"
)

saveRDS(
  road_sex_summary_thomas,
  "data/road_sex_summary_thomas.rds"
)

saveRDS(
  rural_urban_summary_thomas,
  "data/rural_urban_summary_thomas.rds"
)



# ------------------------------------------------------------
# 14. Print basic cleaning diagnostics
# ------------------------------------------------------------

cat("\n========================================\n")
cat("THOMAS: FARS DATA CLEANING COMPLETE\n")
cat("========================================\n")

cat("\nAccident records:", nrow(accidents))
cat("\nVehicle records:", nrow(vehicles))
cat("\nPerson records:", nrow(persons))
cat("\nDriver records:", nrow(drivers))
cat("\nJoined driver/vehicle records:",
    nrow(driver_vehicles))
cat("\nFinal analysis records:",
    nrow(driver_speed_thomas))

cat("\n\nDrivers by sex:\n")

print(
  driver_speed_thomas %>%
    count(SEX_LABEL)
)

cat("\nRecords by year:\n")

print(
  driver_speed_thomas %>%
    count(YEAR)
)


cat("\nRoad classification counts:\n")

print(
  driver_speed_thomas %>%
    count(ROAD_TYPE, sort = TRUE)
)

cat("\nRural / urban counts:\n")

print(
  driver_speed_thomas %>%
    count(AREA_TYPE, sort = TRUE)
)

cat("\nRoad classification by sex:\n")

print(road_sex_summary_thomas)

cat("\n========================================\n")
