# packages
library(tidyverse)

crashes <- read_csv("data/raw/crashes.csv")
persons <- read_csv("data/raw/person.csv")

glimpse(crashes)
glimpse(persons)

# What is the grain of each of these tables?

# What variable appears in both? Does it do the same job in both?

# Let's trim these down to a bit more manageable of a size :) 

# for crashes, let's get COLLISION_ID, CRASH DATE, BOROUGH, NUMBER OF PERSONS INJURED
# and NUMBER OF PERSONS KILLED

crashes_core <- crashes |>
  select(
    COLLISION_ID,
    `CRASH DATE`,
    BOROUGH,
    `NUMBER OF PERSONS INJURED`,
    `NUMBER OF PERSONS KILLED`
  )

glimpse(crashes_core)

# for persons, let's get UNIQUE_ID, COLLISION_ID, PERSON_TYPE, PERSON_INJURY, PERSON_AGE,
# and PERSON_SEX

persons_core <- persons |>
  select(
    UNIQUE_ID,
    COLLISION_ID,
    PERSON_TYPE,
    PERSON_INJURY,
    PERSON_AGE,
    PERSON_SEX
  )

glimpse(persons_core)

# You work for the NYC Department of Transportation and you have been given a media request: 
# How do person-level injury outcomes compare across boroughs? 

# Can you answer this with one dataframe alone? Probably not, or else we wouldn't do this on 
# our join day :)

# So we have to put these together 

# Let's check our keys. Let's review briefly:
# A primary key uniquely identifies a row in its OWN table. 
# A foreign key points at the primary key of ANOTHER table. 

# Let's take a look at these two *_ID columns in our tables. A way we can try to 
# identify a primary vs a foreign key is if we see repeated values. 

crashes_core |>
  count(COLLISION_ID) |>
  filter(n > 1)

persons_core |>
  count(UNIQUE_ID) |>
  filter(n > 1)

persons_core |>
  count(COLLISION_ID, sort = TRUE)

# Why does COLLISION_ID repeat in the person table?
# What do you think this tells us about the real world? What does that repetition represent?

# The reading also says to check for missing keys. Why would that matter?

crashes_core |>
  filter(is.na(COLLISION_ID))

persons_core |>
  filter(is.na(COLLISION_ID))


# Cardinality

# Cardinality is how many rows on each side can share a key value. 
## one-to-one: each key appears once in both
## one-to-many: unique on the left, repeats on the right
## many-to-many: repeats on both sides

# Which one do we have? 

# What do you think will happen to our number of rows after we join? 
nrow(crashes_core)
nrow(persons_core)

# Let's do our join. What should we join by? What's your guess for the number of rows? 

# We'll start by doing a join that keeps observations where both cases exist. 
# Can anyone remember which join this is? 

# Use the console if you don't remember. 
crash_people_inner <- crashes_core |>
  inner_join(persons_core, join_by(COLLISION_ID))

nrow(crashes_core)
nrow(crash_people_inner)

# Why did the table get bigger?

# We've fundamentally changed our data. So now we have to restate the grain. 
# What does one row represent? 

# We said this was one-to-many. We can tell R that and make it verify.

crashes_core |>
  inner_join(persons_core, join_by(COLLISION_ID), relationship = "one-to-many")

# What happens if we claim something false?

crashes_core |>
  inner_join(persons_core, join_by(COLLISION_ID), relationship = "one-to-one")

# Coverage

# The table got bigger. Does that mean nothing was lost? 
# We can use anti_join() to do this. 

crashes_without_people <- crashes_core |>
  anti_join(persons_core, join_by(COLLISION_ID))

nrow(crashes_without_people)

people_without_crashes <- persons_core |>
  anti_join(crashes_core, join_by(COLLISION_ID))

nrow(people_without_crashes)

# Look at the two calls above. All we did was change the df we put before the pipe and inside 
# the join call. Why does that matter? 

# Multiplication and loss are not opposites. Both happened.
# Coverage is: which rows on each side found a partner?

# What kind of crash has no person records?
# What kind of person record has no crash?


##### Make R check that too #####

# If we EXPECT every person to have a crash, we can say so:

persons_core |>
  left_join(crashes_core, join_by(COLLISION_ID), unmatched = "error")

# What did that tell us?
# ^ errors if any left
#   row finds no match.
#
#   silent NA vs. loud
#   error. you choose.


##### ============================================================== #####
##### YOUR TURN                                                      #####
##### ============================================================== #####
#
# Build a person-level table that includes borough.
#
# Before you write any code:
#   - What should one row represent when you're done?
#   - Which table goes on the left?
#   - Should the row count change? Write your prediction down.
#
# Then:
#   - Join them.
#   - Verify your row count.
#   - Declare the cardinality with relationship = and see if R agrees.
#   - Use anti_join() to look at whatever didn't match.
#   - Count records by BOROUGH, PERSON_TYPE, and PERSON_INJURY.
#
# ^ persons on the left.
#   left_join. row count
#   unchanged. that's the
#   point of left_join —
#   the reading says the
#   output always has the
#   same rows as x.
#
#   relationship here is
#   "many-to-one".
#
#   circulate. two tables
#   on the same wall = a
#   30-second room-wide
#   interrupt.


person_crashes <- persons_core |>
  left_join(crashes_core, join_by(COLLISION_ID), relationship = "many-to-one")

nrow(persons_core)
nrow(person_crashes)

person_crashes |>
  count(BOROUGH, PERSON_TYPE, PERSON_INJURY)

# Some of those rows have NA for borough. Where did those come from?

# Would it be safe to drop them? How would you decide?
# ^ do NOT let them
#   filter(!is.na(...))
#   without counting what
#   it removes first.


##### The part that doesn't error #####

# I'm working with NYC DOT. They ask:
# How many people were injured in crashes this year?

crashes_core |>
  summarise(total_inj = sum(`NUMBER OF PERSONS INJURED`, na.rm = TRUE))

person_crashes |>
  summarise(total_inj = sum(`NUMBER OF PERSONS INJURED`, na.rm = TRUE))

# Both ran. Both used the same column. Why are they different?

# Which one is right?
# ^ the first.
#
#   NUMBER OF PERSONS
#   INJURED is a CRASH
#   level value. the join
#   copied it onto every
#   person row.
#
#   a crash with 4 people
#   injured now has that
#   4 sitting on 4 rows.

# Nothing warned us. relationship = didn't catch it.
# unmatched = didn't catch it. The join was correct.
#
# What went wrong?
# ^ the join was fine.
#   the SUM was wrong.
#
#   joining changed the
#   grain, and a column
#   that was one value
#   per crash became one
#   value per person.


##### Where we landed #####

# A mutating join adds columns from one table to another.
#
# Cardinality tells you what it will do to your rows.
# Coverage tells you what didn't find a match.
#
# You can make R check both — relationship = and unmatched = turn your
# assumptions into errors instead of surprises.
#
# But neither of them protects you from the last one. After a join,
# every column is at the grain of the OUTPUT, not the grain it came
# from.
#
# Tuesday: your rows have to match your question.
# Thursday: pivoting changes what a row means.
# Today: so does joining — and the values come along for the ride.
#
#
# Thursday: what if you want the match but not the columns?