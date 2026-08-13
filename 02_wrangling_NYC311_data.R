# Set up and some prep

library(tidyverse)

in_dir <- "nyc311_raw" # where CSVs are located
out_dir <- "nyc311_2023" # where final product will be 

dir.create(out_dir, showWarnings = FALSE) # make sure the path to out_dir exists 

files <- c( # just so i don't have to type a bunch of names on repeat :)
  "JanNYC.csv",
  "FebMarNYC.csv",
  "AprMayNYC.csv",
  "JunJulNYC.csv",
  "AugSepNYC.csv",
  "OctNovNYC.csv",
  "DecNYC.csv"
)

# Make sure the names are consistent across CSVs; select columns i want to retain
names(read_csv(
  file.path(in_dir, files[1]),
  n_max = 5,
  col_types = cols(.default = col_character())
))

keep <- c(
  "Unique Key",
  "Created Date",
  "Closed Date",
  "Resolution Action Updated Date",
  "Agency",
  "Agency Name",
  "Problem (formerly Complaint Type)",
  "Problem Detail (formerly Descriptor)",
  "Status",
  "Borough"
)

# Read + trim in one pass
read_trim <- function(path) {
  message("reading ", basename(path), " ...")
  out <- read_csv(
    path,
    col_select = all_of(keep),
    col_types = cols(.default = col_character()),
    show_col_types = FALSE,
    progress = FALSE
  )
  message("   ", format(nrow(out), big.mark = ","), " rows")
  gc()
  out
}

full_2023 <- map(file.path(in_dir, files), read_trim) %>% 
  list_rbind() %>% 
  rename(
    unique_key = `Unique Key`,
    created_date = `Created Date`,
    closed_date = `Closed Date`,
    resolution_date = `Resolution Action Updated Date`,
    agency = Agency,
    agency_name = `Agency Name`,
    complaint_type = `Problem (formerly Complaint Type)`,
    descriptor = `Problem Detail (formerly Descriptor)`,    status = Status,
    borough = Borough
  )

nrow(full_2023)
n_distinct(full_2023$unique_key)

full_2023 <- full_2023 %>% 
  distinct(unique_key, .keep_all = TRUE)

# Resulting CSV for class
write_csv(full_2023, file.path(out_dir, "nyc311_2023_full.csv.gz"))
