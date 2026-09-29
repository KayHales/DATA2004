# Lab 2

# We'll start by working with the actual crashes and persons fully 
library(tidyverse)

crashes <- read_csv("data/raw/crashes.csv")
persons <- read_csv("data/raw/person.csv")

# 1: Make a table of person records for female pedestrians 

## what does one row represent? 

# 2: Keep only the crashes that involved at least one female pedestrian. 
# Keep COLLISION_ID, BOROUGH, and the five vehicle type columns. 
# how can we select every variable that starts with "VEHICLE TYPE CODE"?

pedestrian_f <- persons |> 
  filter(PERSON_SEX == "F", PERSON_TYPE == "Pedestrian")

ped_f_crash <- crashes |> 
  semi_join(pedestrian_f, join_by(COLLISION_ID))
ped_f_crash

female_ped_crash <- ped_f_crash |> 
  select(COLLISION_ID, BOROUGH, starts_with("VEHICLE TYPE CODE"))

glimpse(female_ped_crash)

# does one row still represent one crash? check it. 

# why a filtering join instead of a mutating join? 

# 3: Right now the vehicle types are columns. We want one row per vehicle. 
# before writing your code, how many rows should we have? 
female_crash_long <- female_ped_crash |> 
  pivot_longer(
    cols = starts_with("VEHICLE TYPE CODE"),
    names_to = "vehicle_slot",
    values_to = "vehicle_type"
  )

glimpse(female_crash_long)

# how many missing values are in the new dataframe? 
female_crash_long |> 
  summarise(
    missing = sum(is.na(vehicle_type))
  )

female_crash_long |> 
  group_by(vehicle_slot) |> 
  summarise(
    missing = sum(is.na(vehicle_type)),
    rows = n()
  )
# why do we think that slots 3, 4, and 5 have so many more missing values? 

# does every crash have a first vehicle recorded? 

# are we safe to drop missing values?
vehicle_records <- female_crash_long |> 
  filter(!is.na(vehicle_type))
# 5: what kinds of vehicles are involved in crashes with a female pedestrian? 
vehicle_records |> 
  count(vehicle_type, sort = TRUE) |> 
  print(n = 40)

