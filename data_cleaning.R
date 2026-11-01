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

