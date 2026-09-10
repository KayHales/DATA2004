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

# What have you been given? 

# Find the documentation and record 
## Who produced this? 
## What does the file contain? 
## What time period does it cover? 

# Build a diagnostic view
## Find the documentation and look at the code above. What variables determine what one 
## row represents? 

data_trimmed <- data |> 
  select(SUMLEV, REGION, DIVISION, STATE, COUNTY, STNAME, CTYNAME,
         POPESTIMATE2024, POPESTIMATE2025, NPOPCHG2025)

# Looking at your table:
## Does every row appear to represent th same kind of geographic observation?
### No. State-county
## Which row(s) look different? 
### 040 = state; 050 = county 
## What in your table tells you this? 
### SUMLEV column
## Is there a variable that appears to encode that difference? 
### SUMLEV column

data_trimmed |> 
  slice_head(n = 10)

# Declare the grain
## One row represents a geographic location
## One row represents either a county or a state. 
## One row represents a mixed state-county grain. 

data_trimmed_numeric <- data_trimmed |> 
  mutate(
    pop2025 = as.numeric(POPESTIMATE2025),
    pop2024 = as.numeric(POPESTIMATE2024),
    popchg2025 = as.numeric(NPOPCHG2025)
  )

data_trimmed_numeric |> 
  filter(STNAME == "Kentucky") |> 
  slice_head(n = 10)

data_trimmed_numeric |> 
  filter(SUMLEV == "040") |> 
  summarise(
    total_pop_2025 = sum(pop2025)
  )

# Let's convert two columsns to numeric 
## `POPESTIMATE2025`
## `NPOPCHG2025`

# What is the total population in the US in 2025 according to this file? 
data_trimmed_numeric <- data_trimmed |> 
  mutate(
    pop2025 = as.numeric(POPESTIMATE2025),
    pop2024 = as.numeric(POPESTIMATE2024),
    popchg2025 = as.numeric(NPOPCHG2025)
  )

data_trimmed_numeric |> 
  filter(SUMLEV == "040") |> 
  summarise(
    total_pop_2025 = sum(pop2025)
  )

# 341,784,857
 
# Which Kentucky counties grew the most from 2024 to 2025? 
data_trimmed_numeric |> 
  filter(STNAME == "Kentucky", SUMLEV == "050") |> 
  filter(popchg2025 > 0) |> 
  select(CTYNAME, popchg2025) |> 
  arrange(desc(popchg2025)) |> 
  print(n = 81) 

data_trimmed_numeric |> 
  filter(SUMLEV == "050", STNAME == "Kentucky", popchg2025 > 0) |> 
  mutate(
    popchgvalid = pop2025 - pop2024
  ) |> 
  select(CTYNAME, popchgvalid) |> 
  arrange(desc(popchgvalid)) |> 
  print(n = 120)

# How many counties or county-equivalent records are in this file? 
data_trimmed_numeric |> 
  filter(SUMLEV == "050") |> 
  nrow()







