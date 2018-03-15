# a script to assign lat longs to tag_ids to see where these fish are
library(lubridate)
library(stringr)
# set up
source("scripts/field_helpers.R")

# find all past tag_ids from database
clown <- read.csv(stringsAsFactors = F, file = "data/clownfish.csv") %>% 
  filter(!is.na(tag_id), 
    tag_id != "NULL") %>% 
  select(anem_table_id, tag_id)

# attach to observation time
anem <- read.csv(stringsAsFactors = F, file = "data/anemones.csv") %>% 
  filter(anem_table_id %in% clown$anem_table_id) %>% 
  select(anem_table_id, dive_table_id, obs_time, anem_id) %>% 
  mutate(anem_table_id = as.integer(anem_table_id), 
    dive_table_id = as.integer(dive_table_id))
clown <- left_join(clown, anem, by = "anem_table_id")

# attach to date
dive <- read.csv(stringsAsFactors = F, file = "data/diveinfo.csv") %>% 
  filter(dive_table_id %in% clown$dive_table_id) %>% 
  select(dive_table_id, date, site)
clown <- left_join(clown, dive, by = "dive_table_id")

clown <- clown %>% 
  mutate(obs_time = str_c(date, obs_time, sep = " "), 
    obs_time = ymd_hms(obs_time), 
    obs_time = force_tz(obs_time, tzone = "Asia/Manila"))

