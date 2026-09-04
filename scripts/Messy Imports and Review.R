# library
library(tidyverse)

# Since we're not using Posit Cloud anymore, we have to do a few things.  
# First, create folders for scripts/ data/ and outputs/ in the Files pane. 
# Inside of data/ add a sub folder called raw/

# Our data downloaded to our computer and not our project so we 
# will need to first move it to our project. 
 
getwd() # our working directory. helpful to not use local paths 
list.files() # let's us see our folders and files in our wd

# Download the data from the website on Canvas. You should have a .xls file 
# called 'API_SP.POP.TOTL_DS2_en_excel_v2_33073.xls' and a zipped folder with three 
# .csv files in it. 
file.exists("API_SP.POP.TOTL_DS2_en_excel_v2_33073.xls")

# Since it's not in our project folder, we need to move it from Downloads in your 
# file explorer. 
population_csv <- read_csv("data/raw/API_SP.POP.TOTL_DS2_en_csv_v2_33112.csv")
# How do we take a look at our data?
glimpse(population_csv)

# Does that look right? Use View(population_csv) to take a closer look
population_csv |> 
  select(`Data Source`) |> 
  slice_head(n = 15) 

population_csv <- read_csv("data/raw/API_SP.POP.TOTL_DS2_en_csv_v2_33112.csv",
                           skip = 4)
glimpse(population_csv) # any columns that we should explore more?
sum(is.na(population_csv$...71))

population_csv <- read_csv("data/raw/API_SP.POP.TOTL_DS2_en_csv_v2_33112.csv",
                           skip = 4, col_select = -...71)
glimpse(population_csv)

# Now to Excel
library(readxl)
population_xls <- read_excel("data/raw/API_SP.POP.TOTL_DS2_en_excel_v2_33073.xls")
glimpse(population_xls)

population_xls <- read_excel("data/raw/API_SP.POP.TOTL_DS2_en_excel_v2_33073.xls",
                             skip = 3,
                             sheet = "Data")
glimpse(population_xls)

# So let's check out something new: JavaScript Object Notation (JSON)
# install.packages("jsonlite")
library(jsonlite)

download.file(
  "https://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL?date=2020%3A2024&format=json&per_page=20000",
  destfile = "data/raw/population_json.json",
  mode = "wb"
)

# We will use more of things like ^ as we progress through the class. 
population_json_raw <- read_json("data/raw/population_json.json")
glimpse(population_json_raw)

# Notice how in the glimpse call it says "List of 2". 
population_json_raw[[1]] # the metadata for the json file
population_json_raw[[2]] # our observations

# That still gives us a list. If we used just ^ that,
# we would still have a list and would need to take a few more steps to get it workable.  
# So we'll add an additional argument to our read_json() call. 
population_json <- read_json("data/raw/population_json.json", 
                             simplifyVector = TRUE)
glimpse(population_json)

population_json[[1]]
population_json[[2]]

population_obs <- population_json[[2]] # let's just keep the observations

glimpse(population_obs)
# Notice how indicator and country are both data frames, not variables. 
# So we still have some nested data here. Let's see what's happening in those. 

head(population_obs$country)
head(population_obs$indicator)

# So inside of both country and indicator, we have an id and we have a value. 
# We can use the $ operator to get these nested values 
population_obs$value
population_obs$country$value
population_obs$indicator$value


# But we still need to get this into a tidy format, plus we have dataframes for indicator 
# and country. How would I make this a tibble and flatten it out? Let's look at what 
# the glimpse output gives us again. I'll get us started:
population_tidy <- population_json |> 
  as_tibble()

population_tidy <- population_obs |> 
  as_tibble() |> 
  transmute(
    `Country Name` = country$value,
    `Country Code` = countryiso3code,
    `Indicator Name` = indicator$value,
    `Indicator Code` = indicator$id,
    Year = as.integer(date),
    Population = value
  )

glimpse(population_tidy)