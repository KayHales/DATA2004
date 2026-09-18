# package
library(tidyverse)

# import chocolate.csv, name it chocolate_raw

# inspect 

# create chocolate from chocolate_raw with only these columns: review_date, company_location,
# cocoa_percent, rating

# filter for France and only ratings greater than or equal to 3.5. 

# filter for France, Canada, and the U.S.A.

# create a table taht shows the 10 highest rated chocolate bars. 

# Mutate
## Let's start. How would I just look at the mean cocoa_percent? 
mean(chocolate$cocoa_percent) 
class(chocolate$cocoa_percent) 

# We can't take the mean of a character. 
chocolate |> 
  mutate(
    cocoa_num = as.numeric(cocoa_percent)
  ) |> 
  select(cocoa_num)

# Why does that ^ return only NA values? Notice what is inside 
# the column: "77%" that % sign prevents us from using as.numeric()
chocolate |> 
  mutate(
    cocoa_num = parse_number(cocoa_percent)
  ) 

chocolate <- chocolate |> 
  mutate(
    cocoa_num = parse_number(cocoa_percent)
  ) 
mean(chocolate$cocoa_num)

# create a grouped summary of company_location that gives us
# the number of times that country appears
# mean ratings 
# median ratings
# arrange the table from highest to lowest average ratings

# missingness
# you can find the sum of missing values like this 
sum(is.na(chocolate$rating)) # any column in the df will work. 

# how would you show the number of missing values in review_date for each company_location?

# viz review 

# how would you make a basic histogram of rating? 

# a basic barplot?

# a basic boxplot for rating? 

# a scatterplot between cocoa_num and rating?

# we can add aesthetics too, e.g.,
chocolate |> 
  filter(
    company_location %in% c("U.S.A.", "Canada", "France", "U.K.")
  ) |> 
  ggplot(aes(x = cocoa_num, y = rating, color = company_location)) +
  geom_point()

# for the plot above: 
# add a regression line to the plot. 
# add a title, subtitle, and give better names to the axes