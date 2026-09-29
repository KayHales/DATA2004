# packages
library(tidyverse)

# import and look 
data <- read_csv(
  "data/raw/co-est2025-alldata.csv",
  locale = locale(encoding = "Latin1"), 
  col_types = cols(.default = col_character())
)

glimpse(data)
names(data)

# What can we determine from this? What can we not? 

# What have you been given? How would you figure out what this csv is without
# being told?

# Find the documentation and record 
## Who produced this? 
## What does the file contain? 
## What time period does it cover? 

# Build a diagnostic view
## Find the documentation and look at the code above. What variables determine what one 
## row represents? 

data |> 
  slice_head(n = 10)

# Looking at your table:
## Does every row appear to represent th same kind of geographic observation?
## Which row(s) look different? 
## What in your table tells you this? 
## Is there a variable that appears to encode that difference? 

# Declare the grain
# One row represents ______


# Let's convert two columns to numeric 
## `POPESTIMATE2025`
## `NPOPCHG2025`

data_numeric <- data |> 
  mutate(
    pop2025 = as.numeric(POPESTIMATE2025),
    popchange2025 = as.numeric(NPOPCHG2025)
  )

# What is the total population in the US in 2025 according to this file? 
data_numeric |> 
  filter(SUMLEV == "040") |> 
  summarise(
    total_pop = sum(POPESTIMATE2025, na.rm = TRUE)
  )

# Which Kentucky counties grew the most from 2024 to 2025? 
data_numeric |> 
  filter(STNAME == "Kentucky", 
         SUMLEV != "040") |> 
  select(CTYNAME, popchange2025) |> 
  arrange(desc(popchange2025))

# How many counties or county-equivalent records are in this file? 

data |> 
  filter(SUMLEV == "050") |> 
  count()



