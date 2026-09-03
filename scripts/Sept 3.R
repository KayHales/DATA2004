# Library
# install.packages("tidyverse")
library(tidyverse)
# What is our wd? What is a wd in general? 
getwd()
# Download the files from Canvas. We need to move them to our wd. 
population_csv <- read_csv("data/raw/API_SP.POP.TOTL_DS2_en_csv_v2_33112.csv")

population_csv <- read_csv("data/raw/API_SP.POP.TOTL_DS2_en_csv_v2_33112.csv",
                       skip = 4)
glimpse(population_csv)
population_csv <- population_csv |> 
  select(-...71)
glimpse(population_csv)
# But we need to take a look at it first. 
# install.packages("readxl")
library(readxl)

population_xls <- read_excel("data/raw/API_SP.POP.TOTL_DS2_en_excel_v2_33073.xls",
                             sheet = "Data", skip = 3)
glimpse(population_xls)
# Let's bring in the csv and take a look at it. 
download.file(
  url = "https://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL?date=2020%3A2024&format=json&per_page=20000",
  destfile = "data/raw/population_json.json",
  mode = "wb"
)

# install.packages("jsonlite")
library(jsonlite)
population_json_raw <- read_json("data/raw/population_json.json")
glimpse(population_json_raw)

population_obs <- population_json_raw[[2]]
glimpse(population_obs)

population_json_simple <- read_json("data/raw/population_json.json",
                                 simplifyVector = TRUE)
glimpse(population_json_simple)
population_obs <- population_json_simple[[2]]
glimpse(population_obs)
