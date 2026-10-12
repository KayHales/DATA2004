# Lab 2: is this file ready to analyze?

library(tidyverse)


# you're a junior analyst at a state library association. a colleague
# downloaded the 2024 Public Libraries Survey and wants to compare
# library systems on visits, circulation, and programs.

# your supervisor wants an audit before anyone analyzes anything.

# nothing today is new. you've done every piece of this already.


# grain and key

libraries <- read_csv(
  "data/raw/PLS_FY24_AE_pud24i.csv",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

glimpse(libraries)

# one row is one one library system. 

# the download also has an "outlet" file. it has more rows.
# why would it? (the user's guide will tell you.)

## this is one row per outlet, so centrla library, branch, bookmobile

# which column should identify a row?


# what did those tell you?
# what would it have meant if the count() came back with rows?


# let's focus on VISITS for now. what does this column mean?

# VISITS counts visits to the library in a year.
# before you run anything: what values would be impossible?


# how many different negative values? how many rows of each?

# is a negative number here bad data, missing data, or a code?
# can you tell from the data alone?

# does R think any of these are missing?

is.na(libraries$VISITS) |>
  table()

# on tuesday, NA told us THAT something was missing but not WHY.
# what's different here?

# the survey doesn't use NA values, it uses codes. R cannot tell that -1 or -3 is missing. 


# go to the user's guide. for each negative value:
# what does it mean, in the guide's words? where did you find it?

# do the two codes mean the same thing?
# No. One is something wasn't reported. The other is temporarily closed. 


# every numeric column has a flag column. find the one for VISITS.



# what does the flag tell you? does it tell the two codes apart?
# 


# make a clean version. VISITS stays exactly as it is.

libraries <- libraries |>
  mutate(visits_clean = if_else(VISITS %in% c(-1, -3), NA_real_, VISITS))

# why list the codes instead of writing VISITS < 0?

# both codes just turned into NA. what did we lose?
# where can we still find it?



# did it do what we meant? three questions:
# same number of library systems?
# did every code become NA?
# did anything else become NA?

# does any of this matter?


# why is the raw mean lower? what's in each denominator?
# Raw has 9,249 systems and clean has 9,175. Small differences because it's 0.8% difference. 



# your turn :)
# get in your group project groups

# do that process for each of the following:
# TOTATTEN - program attendance
# TOTPRO - number of programs
# TOTCIR - total circulation

# you're not solving a new problem. same steps, different variable.
# everything you need is in the VISITS section.

# what should it measure? what would be impossible?


# range. any odd values? how many of each?


# what does the user's guide say they mean?
# ("we couldn't find it" is an answer. "we assumed" isn't.)


# what's the flag column? (the names get shortened. look for it.)


# clean version. keep the raw column.


# check it: same rows? every code became NA? nothing else did?

# mean before and after. big change or small?

# where are they? 

# recoding to NA fixes the number. does it finish the job?

# are the coded rows spread out, or do they bunch up?

# if someone compares circulation across states, what goes wrong?


# this is for you to answer
# is this file ready to analyze as is? 3-4 sentences.
