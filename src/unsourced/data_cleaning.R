library(readr)
library(stringr)
library(tidyverse)

person <- read_csv("data/person.csv")

person_cleaned <- person |>
  transmute(
    Hour = HOUR,
    `Harm Event` = HARM_EVNAME, # Lots of small categories
    Role = PER_TYPNAME,
    Rurality = RUR_URBNAME,
    `Direction of First Impact` = IMPACT1NAME,
    Age = AGE,
    Sex = SEXNAME,
    Severity = INJ_SEVNAME,
    Seat = SEAT_POSNAME,
    Restraint = REST_USENAME,
    Alcohol = DRINKINGNAME,
    `Person Location` = LOCATIONNAME
  ) |>
  filter(
    Sex %in% c("Male", "Female"),
    Rurality %in% c("Urban", "Rural"),
    Hour <=  24,
    Age <= 100
  )

person_cleaned <- person_cleaned |>
  mutate(
    Fatal = factor(case_when(
      str_detect(Severity, "Fatal") ~ "Dead",
      str_detect(Severity, "Not Reported|Prior") ~ "Unknown/Other",
      .default = "Alive"
    ), levels = c("Dead", "Alive", "Unknown/Other")),
    Role = case_when(
      str_detect(Role, "Driver") ~ "Driver",
      str_detect(Role, "Passenger") ~ "Passenger",
      str_detect(Role, "Pedestrian") ~ "Pedestrian",
      str_detect(Role, "Bicyclist") ~ "Bicyclist",
      .default = "Other/Unknown"  
    )
  ) |>
  select(-Severity)
  

write_csv(person_cleaned, "data/person_cleaned.csv")

# accident ---------------------------------------------------------------------

accident <- read_csv("data/accident.csv")

accident_cleaned <- accident |> 
  transmute(
    State = STATENAME,
    ST_CASE = ST_CASE,
    Date = as.Date(paste0(YEAR, "-", MONTH, "-", DAY)),,
    # FARS labels the midnight hour "0:00am-0:59am"; rename it to match the
    # levels below, otherwise those ~1,500 crashes become NA
    Hour = factor(if_else(HOURNAME == "0:00am-0:59am", "12:00am-12:59am", HOURNAME),
                  levels = c(
                    "12:00am-12:59am",
                    "1:00am-1:59am",
                    "2:00am-2:59am",
                    "3:00am-3:59am",
                    "4:00am-4:59am",
                    "5:00am-5:59am",
                    "6:00am-6:59am",
                    "7:00am-7:59am",
                    "8:00am-8:59am",
                    "9:00am-9:59am",
                    "10:00am-10:59am",
                    "11:00am-11:59am",
                    "12:00pm-12:59pm",
                    "1:00pm-1:59pm",
                    "2:00pm-2:59pm",
                    "3:00pm-3:59pm",
                    "4:00pm-4:59pm",
                    "5:00pm-5:59pm",
                    "6:00pm-6:59pm",
                    "7:00pm-7:59pm",
                    "8:00pm-8:59pm",
                    "9:00pm-9:59pm",
                    "10:00pm-10:59pm",
                    "11:00pm-11:59pm"
                  )),
    Longitude = LONGITUD,
    Latitude = LATITUDE,
    Rurality = RUR_URBNAME,
    `Road Type` = FUNC_SYSNAME,
    `Harm Event` = HARM_EVNAME,
    `Location WRT Road` = REL_ROADNAME,
    `Work Zone` = WRK_ZONENAME,
    `Light Conditions` = LGT_CONDNAME,
    Weather = WEATHERNAME,
    Fatalities = FATALS
  )

saveRDS(accident_cleaned, "data/accident_cleaned.rds")

