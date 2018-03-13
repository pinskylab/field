# how many fish were tagged in the past that we have not captured this season
library(dplyr)
library(lubridate)

# get tagged fish
# in the field, get from saved csv
fish <- read.csv("data/clownfish.csv", stringsAsFactors = F) %>% 
  filter(tag_id != "NULL") %>% 
  select(tag_id, anem_table_id)

# connect to dive site
anem <- read.csv("data/anemones.csv", stringsAsFactors = F) %>% 
  filter(anem_table_id %in% fish$anem_table_id) %>% 
  select(anem_table_id, dive_table_id) %>% 
  mutate(anem_table_id = as.integer(anem_table_id), 
    dive_table_id = as.integer(dive_table_id))
fish <- left_join(fish, anem, by = "anem_table_id")
rm(anem)

dive <- read.csv("data/diveinfo.csv", stringsAsFactors = F) %>% 
  filter(dive_table_id %in% fish$dive_table_id) %>% 
  select(dive_table_id, site)
fish <- left_join(fish, dive, by = "dive_table_id") %>% 
  select(-contains("table_id"))
rm(dive)
past_tags <- fish
rm(fish)


# which fish have been tagged year

# # if data is accessible in google sheets:
# library(googlesheets)
# # gs_auth(new_user = TRUE) # run this if having authorization problems
# mykey <- '1symhfmpQYH8k9dAvp8yV_j_wCTpIT8gO9No4s2OIQXo' # access the file
# entry <-gs_key(mykey)
# clown <-gs_read(entry, ws='clownfish')
# dive <- gs_read(entry, ws="diveinfo")

# # load data from saved if network connection is lost
# # THIS HAS TO BE MANUALLY UPDATED WITH MOST CURRENT VERSION OF SAVED FILE  - COULD WRITE CODE TO FIND AND LOAD THE MOST CURRENT VERSION ####
load(file = "data/clown_2018-03-12 20:24:41.Rdata")
load(file = "data/dive_2018-03-12 20:24:41.Rdata")

clown <- clown %>% 
  filter(!is.na(dive_num)) %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "9851_", "985153000")) %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "9861_", "986112100")) %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "9820_", "982000411")) %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "9821_", "982126052")) %>% 
  # fix 6 digit entries
  mutate(tag_id = stringr::str_replace(tag_id, "^95", "98212605295"), 
    tag_id = stringr::str_replace(tag_id, "^818", "982000411818"),
    tag_id = stringr::str_replace(tag_id, "^1", "9861121001"),
    tag_id = stringr::str_replace(tag_id, "^3", "9851530003"),
    tag_id = stringr::str_replace(tag_id, "^4", "9851530004"),
    tag_id = stringr::str_replace(tag_id, "^6", "9821260526"))

dive <- dive %>%
  select(dive_num, date, site, municipality, cover, gps)
dive <- distinct(dive)

# ---------------------------------------------
#   add dates and sites to anems
# ---------------------------------------------

clown <- left_join(clown, dive, by = "dive_num")

# remove lines that are not anemones
clown <- clown %>%
  filter(!is.na(anem_id)) %>% 
  mutate(obs_time = ymd_hms(obs_time))


# # convert to UTC
anem$obs_time <- with_tz(anem$obs_time, tzone = "UTC")

# convert GPS to character
anem$gps <- as.character(anem$gps)

# split out time components to compare to latlong
anem <- anem %>%
  mutate(month = month(date)) %>%
  mutate(day = day(date)) %>%
  mutate(hour = hour(obs_time)) %>%
  mutate(min = minute(obs_time)) %>%
  mutate(sec = second(obs_time)) %>%
  mutate(year = year(date))




tagged_fish <- clown %>% 
  filter(!is.na(tag_id)) %>% 
  select(tag_id, dive_num, anem_id)
tagged_fish <- left_join(tagged_fish, dive, by = "dive_num") %>% 
  select(tag_id, site)

no_recap <- anti_join(past_tags, tagged_fish) 

need_recap <- no_recap %>% 
  group_by(site) %>% 
  summarise(missing_fish = n())

# there seem to be a lot - lets map them to see 

