# how many anems did we tag each year?
library(dplyr)
source("scripts/field_helpers.R")

# for 2017, how many pit tags have a number and a N for recap?
leyte <- read_db("Leyte")

fish <- leyte %>% 
  tbl("clownfish") %>% 
  filter(!is.na(tag_id)) %>%
  filter(recap == "N" | is.na(recap)) %>% 
  select(fish_table_id, anem_table_id, recap, tag_id) %>% 
  collect()

day <- fish_date(fish$fish_table_id)
fish <- left_join(fish, day, by = c("fish_table_id", "anem_table_id"))

sev <- fish %>% 
  filter(grepl("2017", date))
# 367
six <- fish %>% 
  filter(grepl("2016", date))
# 552

fiv <- fish %>% 
  filter(grepl("2015", date))
#473

nums <- c(367, 552, 473)
mean(nums)
