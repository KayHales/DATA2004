# pacakges
library(tidyverse)

# let's start with the same persons_core and crashes_core 
crashes <- read_csv("data/raw/crashes.csv")
persons <- read_csv("data/raw/person.csv")

crashes_core <- crashes |> 
  select(
    COLLISION_ID,
    `CRASH DATE`,
    BOROUGH,
    `NUMBER OF PERSONS INJURED`,
    `NUMBER OF PERSONS KILLED`
  )

persons_core <- persons |> 
  select(
    UNIQUE_ID,
    COLLISION_ID,
    PERSON_TYPE,
    PERSON_INJURY,
    PERSON_AGE,
    PERSON_SEX
  )

glimpse(crashes_core)
glimpse(persons_core)

# let's do a brief review of our mutating joins :) 

## what are the primary keys? what about the foreign key? 

## if

# Which crashes involved at least one bicyclist? I want one row per crash. 
bicyclists <- persons_core |> 
  filter(PERSON_TYPE == "Bicyclist")

nrow(bicyclists)

# why doesn't ^ answer our question? What join would you use to do it right?
bicyclist_matches <- crashes_core |> 
  inner_join(bicyclists, join_by(COLLISION_ID))

nrow(bicyclist_matches)
n_distinct(bicyclist_matches$COLLISION_ID)

# use nrow() on the join and n_distinct() on that join's collision ID. Why are they different? 

# we can answer this by thinking about the grain.
# we're joining persons to the crashes grain, so what does one row represent? 

# is it every crash involving a bicyclist? let's check out the first 10 rows.  
bicyclist_matches |> 
  slice_head(n = 10)

# our mutating join adds columns so it has changed our grain, but we don't want it to right now. 

# so we'll need to use *filtering* grains
# we got exposed to one filtering grain already: anti_join(). let's check out the other 
bicyclist_crashes <- crashes_core |> 
  semi_join(bicyclists, join_by(COLLISION_ID))

nrow(bicyclist_crashes)
n_distinct(bicyclist_crashes$COLLISION_ID)
glimpse(bicyclist_crashes)

# this doesn't add more columns, so we're not working with crash-bicyclists combination

# now do anti_join for crashes that do not involve a bicyclist. 


# now it's y'all's turn: identify crashes that involve at least one pedestrian, 
# one row per crash. 

## after that, narrow it down. crashes where at least one pedestrian was recorded as female. 


