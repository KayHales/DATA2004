# Create student sample of NYC data
library(tidyverse)

set.seed(2004)

student_sample <- full_2023 %>%
  slice_sample(n = 50000)

nrow(student_sample)
n_distinct(student_sample$unique_key)

student_diag <- student_sample %>%
  mutate(
    created = mdy_hms(created_date, quiet = TRUE),
    closed = mdy_hms(closed_date, quiet = TRUE),
    updated = mdy_hms(resolution_date, quiet = TRUE)
  )

student_diag %>%
  summarise(
    n = n(),
    created_unparsed =
      sum(
        !is.na(created_date) &
          is.na(created)
      ),
    
    closed_blank =
      sum(is.na(closed_date)),
    
    closed_unparsed =
      sum(
        !is.na(closed_date) &
          is.na(closed)
      ),
    
    negative_duration =
      sum(
        closed < created,
        na.rm = TRUE
      ),
    
    updated_after_close =
      sum(
        updated > closed + days(30),
        na.rm = TRUE
      )
  )

student_diag %>%
  count(status,
    closed_blank = is.na(closed_date)
  ) %>%
  arrange(desc(n))

student_diag %>%
  summarise(
    first_created = min(created, na.rm = TRUE),
    last_created = max(created, na.rm = TRUE)
  )

write_csv(
  student_sample,
  file.path(
    out_dir,
    "nyc311_2023_sample_50000.csv.gz"
  )
)