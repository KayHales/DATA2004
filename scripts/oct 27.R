library(tidyverse)
library(httr2)

# since today is a handful of new things, i thought it'd be nice to set us up with 
# a simple description of the goals for today :) 

# so far working with acquisition we've done 
# one request -> one response -> open it -> make a table 

# but today the middle step multiplies: 
# one *or more* requests -> a list of responses -> open each -> stack -> table

# in the weekly, you could deal with limits in how many records you could pull 
# with per_page = 300. it was an easy fix. 
# but most APIs aren't as nice to us. the chicago art institute gives us a hard limit 
# of 100. we can't increase that. 

# so let's say that the World Bank API gave us a hard limit of 50 and i wanted you to get
# all 266 records. how many requests would we need to make?

# let's do a request 
pop_req <- request("https://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL") |> 
  req_url_query(date = 2023, format = "json")

# let's check out the first 2 pages 
pop_req |> 
  req_url_query(page = 1)

pop_req |> 
  req_url_query(page = 2)

# check the url. what's changing? 
request("https://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL") |> 
  req_url_query(date = 2023, format = "json") |> 
  req_perform(path = "data/raw/wdi.json")

wdi <- jsonlite::read_json("data/raw/wdi.json")
str(wdi)

# why wouldn't we just copy-paste req_url_query(page = n)?

# we can tell R "i want to do this part next" with httr2. we have to tell it: 
## which parameter changes, this example is "page"
## how to tell how many pages exist 

# and we can do this with a convenient little recipe: \(resp)

# before we run the code below, let's learn what \(resp) has going on 
# we've used functions all semester. these are functions 
mean(c(2, 4, 6)) 
str(pop_req)

# you put something in, get something out. 
# but so far we've only used functions that someone else gives us. 
# we can make our own functions :D
add_one <- function(x) {
  x + 1
}

add_one(5)
add_one(10)

# function(x): "i'm giving you a recipe, the ingredient is called x)\"
# x + 1: here's what to do with x 
# and then we supply x in our function. 

# \x is a shortcut to do the exact same thing. so these are equivalent 
add_one <- function(x) {x + 1}
add_one <- \(x) {x + 1}

# let's do an example that applies to responses and requests 
first <- pop_req |> 
  req_perform()
get_pages <- \(resp) {resp_body_json(resp)[[1]]$pages}
get_pages(first)

# now you try: 
# write a function called get_records: takes one repsonse, gives back just the records. 
# test it on first. how many records? 





# so "\" is just shorthand for "function"

pop_resps <- pop_req |> 
  req_throttle(rate = 30 / 60) |> 
  req_perform_iterative(
    next_req = iterate_with_offset(
      "page",
      resp_pages = \(resp) resp_body_json(resp)[[1]]$pages
    ),
    path = "data/raw/wb_pop_page_{i}.json"
  )

# before we walk through this, let's focus on our function again 
# resp_pages = \(resp) resp_body_json(resp)[[1]]$pages notice that we didn't 
# store this as a function with "<-". we don't need this function after this call. 


# DONT YOU DARE SCROLL DOWN. you know who you are! 
# let's answer a few questions :) 
## how many requests did it end? how did it know when to stop? 
## what' in pop_resps?
## look at your path. what did path = ...{i}... do?
## what do you think req_throttle() does? why would the world bank care? 
length(pop_resps)







# what did that do?

# let's break it down into the steps: 
## Step 1: send pop_req with page = 1. iterate_with_offset starts at 1 by default 
## Step 2: runs resp_pages on that first response, once. it gets 6, so it knows when to stop
## Step 3: make the next request by adding 1 to page (offset = 1), sends it, and 
## repeats through page 6. 
## Step 4: save each response to its own file. {i} is the request number
## Step 5: return a list of 6 respones. 

# in other words, we're trying to make this easier on ourselves 
# we tell R to ask for page 1, look at the first element to see how many pages 
# there are in total. then ask for page 2, then 3, then 4,5, and 6. and then stop. 

# let's actually pull the records out of every response and stack them. 
# how long was 
# we'll start by just opening the firs two responses. we'll use the function 
# y'all wrote! 

page1 <- get_records(pop_resps[[1]])
page2 <- get_records(pop_resps[[2]])
length(c(page1, page2))

# but everything we're doing today is to avoid having to type thing on a loop! 
# let's have httr2 do it for us 
pop_records <- pop_resps |> 
  resps_data(get_records)

# how many records should there be? 
length(pop_records)


# now let's make it a table. 
# call the table pop: include iso3, country, population. you've already done this. 
# i'll get you started: 
pop <- tibble(rec = pop_records) |> 
  hoist(rec,
        iso3 = "countryiso3code",
        country = c("country", "value"),
        population = "value") |> 
  select(-rec)

pop

str(pop$rec[[1]], max.level = 2)

# there's an issue we'll try to fix on Thursday: 
# if you rerun this, it'll rewrite everything and won't warn you or tell you. 
# what if we wanted to run this again, but only *if* we don't have certain files?