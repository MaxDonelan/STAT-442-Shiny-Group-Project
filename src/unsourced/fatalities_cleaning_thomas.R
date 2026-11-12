
# ============================================================
# Data Cleaning: Fatalities by Sex (Thomas's tab)
# Years: 2023 and 2024
# Author: Thomas Shields
#
# Input:  data/persons_thomas.rds (built by speed_cleaning_thomas.R)
# Output: data/fatalities_thomas.rds
#
# One row per person killed (male or female only), with only the
# columns the Shiny tab needs, already recoded into labels.
# ============================================================

library(dplyr)

thomas_persons <- readRDS("data/persons_thomas.rds")

thomas_fatalities <- thomas_persons |>
  filter(
    INJ_SEV == 4,        # Fatal Injury (K)
    SEX %in% c(1, 2)     # drop unknown / not reported sex
  ) |>
  mutate(
    YEAR = as.integer(YEAR),

    SEX_LABEL = factor(
      if_else(SEX == 1, "Male", "Female"),
      levels = c("Male", "Female")
    ),

    IS_DRIVER = PER_TYP == 1,

    # Who died ------------------------------------------------
    PERSON_TYPE = factor(
      case_when(
        PER_TYP == 1      ~ "Driver",
        PER_TYP == 2      ~ "Passenger",
        PER_TYP == 5      ~ "Pedestrian",
        PER_TYP %in% 6:7  ~ "Bicyclist",
        TRUE              ~ "Other"
      ),
      levels = c("Driver", "Passenger", "Pedestrian", "Bicyclist", "Other")
    ),

    # Age (998 / 999 = not reported / unknown) ------------------
    AGE = if_else(AGE <= 120, as.numeric(AGE), NA_real_),

    # Seat belt use ---------------------------------------------
    # Only occupants of motor vehicles in transport, excluding
    # motorcycles (BODY_TYP 80-89), which are coded "None Used"
    # because riders don't have seat belts.
    BELT_STATUS = case_when(
      !PER_TYP %in% c(1, 2, 9)          ~ NA_character_,
      BODY_TYP %in% 80:89               ~ NA_character_,
      REST_USE %in% c(1:4, 6, 8, 10:12) ~ "Restrained",
      REST_USE == 20                    ~ "Not restrained",
      TRUE                              ~ "Unknown"
    ),
    BELT_STATUS = factor(
      BELT_STATUS,
      levels = c("Restrained", "Not restrained", "Unknown")
    ),

    # Blood alcohol (drivers only) ------------------------------
    # ALC_RES is BAC in thousandths (80 = 0.08); 940+ is not a result.
    BAC_GROUP = case_when(
      PER_TYP != 1               ~ NA_character_,
      ALC_RES == 0               ~ "0.00 (sober)",
      ALC_RES >= 1 & ALC_RES < 80 ~ "0.01-0.07",
      ALC_RES >= 80 & ALC_RES <= 940 ~ "0.08+ (legally drunk)",
      TRUE                       ~ "Not tested / unknown"
    ),
    BAC_GROUP = factor(
      BAC_GROUP,
      levels = c("0.00 (sober)", "0.01-0.07",
                 "0.08+ (legally drunk)", "Not tested / unknown")
    ),

    # Road classification ---------------------------------------
    ROAD_TYPE = factor(
      case_when(
        FUNC_SYS == 1 ~ "Interstate",
        FUNC_SYS == 2 ~ "Other Freeway / Expressway",
        FUNC_SYS == 3 ~ "Other Principal Arterial",
        FUNC_SYS == 4 ~ "Minor Arterial",
        FUNC_SYS == 5 ~ "Major Collector",
        FUNC_SYS == 6 ~ "Minor Collector",
        FUNC_SYS == 7 ~ "Local Road",
        TRUE          ~ NA_character_
      ),
      levels = c("Interstate", "Other Freeway / Expressway",
                 "Other Principal Arterial", "Minor Arterial",
                 "Major Collector", "Minor Collector", "Local Road")
    ),

    # Rural vs. urban -------------------------------------------
    AREA_TYPE = factor(
      case_when(
        RUR_URB == 1 ~ "Rural",
        RUR_URB == 2 ~ "Urban",
        TRUE         ~ NA_character_
      ),
      levels = c("Rural", "Urban")
    )
  ) |>
  select(
    YEAR, SEX_LABEL, IS_DRIVER, PERSON_TYPE, AGE,
    BELT_STATUS, BAC_GROUP, ROAD_TYPE, AREA_TYPE
  )

saveRDS(thomas_fatalities, "data/fatalities_thomas.rds")

# Quick checks
cat("Rows:", nrow(thomas_fatalities), "\n")
print(count(thomas_fatalities, SEX_LABEL))
print(count(thomas_fatalities, SEX_LABEL, BELT_STATUS))
print(count(thomas_fatalities, SEX_LABEL, BAC_GROUP))
