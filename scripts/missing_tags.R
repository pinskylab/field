# how many fish were tagged in the past that we have not captured this season
  # wanted to pull the date times from the pit scanner and determine gps locations that way, however, not all scans were done during a dive (some were done on land, like the 2015 field season)

library(ggplot2)
library(lubridate)
library(stringr)
source("scripts/field_helpers.R")

# get currently scanned fish
# # if data is accessible in google sheets:
# library(googlesheets)
# # gs_auth(new_user = TRUE) # run this if having authorization problems
# mykey <- '1symhfmpQYH8k9dAvp8yV_j_wCTpIT8gO9No4s2OIQXo' # access the file
# entry <-gs_key(mykey)
# clown <-gs_read(entry, ws='clownfish')
# dive <- gs_read(entry, ws="diveinfo")
# 
# # # save data in case network connection is lost
# clownfilename <- str_c("data/clown_", Sys.time(), ".Rdata", sep = "")
# divefilename <- str_c("data/dive_", Sys.time(), ".Rdata", sep = "")

# clown <- clown %>%  # adapt time because having trouble with time loading as numeric
#   mutate(obs_time = as.character(obs_time))
# save(clown, file = clownfilename)
# save(dive, file = divefilename)

# load data from saved if network connection is lost
# THIS HAS TO BE MANUALLY UPDATED WITH MOST CURRENT VERSION OF SAVED FILE  - COULD WRITE CODE TO FIND AND LOAD THE MOST CURRENT VERSION ####
load(file = "data/clown_2018-03-20 02:38:39.Rdata")
load(file = "data/dive_2018-03-20 02:38:39.Rdata")

# ---------------------------------------------
#   format tag ids on clownfish data sheet
# ---------------------------------------------
clown <- clown %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "9851_", "985153000")) %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "9861_", "986112100")) %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "9820_", "982000411")) %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "9821_", "982126052")) %>% # fix 6 digit entries
  mutate(tag_id = stringr::str_replace(tag_id, "^95", "98212605295"), 
    tag_id = stringr::str_replace(tag_id, "^818", "982000411818"),
    tag_id = stringr::str_replace(tag_id, "^1", "9861121001"),
    tag_id = stringr::str_replace(tag_id, "^3", "9851530003"),
    tag_id = stringr::str_replace(tag_id, "^4", "9851530004"),
    tag_id = stringr::str_replace(tag_id, "^6", "9821260526"))


# get current season tagged fish  ####
present <- clown %>% 
  filter(!is.na(tag_id)) %>% 
  select(dive_num, tag_id, obs_time, size, color) # include fields to assign a date and time
dive <- dive %>% 
  select(dive_num, gps, date) %>% 
  filter(gps == 4)

present <- left_join(present, dive, by = "dive_num") %>% 
  select(-dive_num) %>% 
  mutate(time = ymd_hms(str_c(date, obs_time, sep = " ")))

# add data from database #### 
# # on campus
# leyte <- read_db("Leyte")
# past <- leyte %>% 
#   tbl("clownfish") %>% 
# filter(!is.na(tag_id))

# in the field this is as a csv
past <- read.csv(stringsAsFactors = F, file = "data/clownfish.csv", na = "NULL") %>% 
  filter(!is.na(tag_id)) %>% 
  select(anem_table_id, tag_id, size, color)# include fields to assign a date and time, also size 
anem <- read.csv(stringsAsFactors = F, na = "NULL", file = "data/anemones.csv") %>% 
  filter(anem_table_id %in% past$anem_table_id) %>% 
  select(anem_table_id, dive_table_id, obs_time)
past <- left_join(past, anem, by = "anem_table_id")
dive <- read.csv(stringsAsFactors = F, na = "NULL", file = "data/diveinfo.csv") %>% 
  filter(dive_table_id %in% past$dive_table_id) %>% 
  select(dive_table_id, date, gps)
past <- left_join(past, dive, by = "dive_table_id") %>% 
  mutate(time = ymd_hms(str_c(date, obs_time, sep = " ")), 
    tag_id = as.character(tag_id)) %>% 
  select(-contains("table")) %>% 
  rename(old_size = size, 
    old_color = color)

# remove scans from the current field season ####
recaps <- present %>% 
  filter(tag_id %in% past$tag_id)
recaps <- left_join(recaps, past, by = "tag_id")

past <- anti_join(past, present, by = "tag_id")
rm(anem, present)

# # take a look at size structure of unscanned tags
# ggplot(past) +
#   stat_count(mapping = aes(x = old_size))
# 
# # how many fish have grown? by how much?
# ggplot(data = recaps, mapping = aes(x = as.numeric(old_size), y = size)) +
#   geom_point(mapping = aes(color = old_color))+
#   geom_smooth()
# 
# ggplot(data = recaps) + 
#   geom_point(mapping = aes(x = as.numeric(old_size), y = size)) + 
#   facet_grid(. ~ color) 
# 
# ggplot(recaps) +
#   stat_count(mapping = aes(x = size))
# 
# # how many fish have changed tail color #56, most of these that changed, changed to YP
# change_color <- recaps %>% 
#   filter(old_color != color)
# ggplot(data = change_color) +
#   geom_bar(mapping = aes(x = color))
# 
# ggplot(data = recaps) +
#   stat_summary(
#     mapping = aes(x = color, y = as.numeric(size)),
#     fun.ymin = min,
#     fun.ymax = max,
#     fun.y = median
#   )
# 
# ggplot(data = past) +
#   stat_summary(
#     mapping = aes(x = old_color, y = as.numeric(old_size)),
#     fun.ymin = min,
#     fun.ymax = max,
#     fun.y = median
#   )

# format scans to compare with gps ####
# add time zone
past <- past %>% 
  mutate(time = force_tz(time, tzone = "Asia/Manila"))
# convert to UTC
past <- past %>% 
  mutate(time = with_tz(time, tzone = "UTC")) 

# split out time components to compare to latlong
past <- past %>%
  mutate(month = month(time)) %>%
  mutate(day = day(time)) %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time)) %>%
  mutate(year = year(time))

past <- past %>% 
  select(-time, -sec)

# pull in the gpx data
gpx <- read.csv(stringsAsFactors = F, na = "NULL", file = "data/GPX.csv") %>% 
  mutate(time = ymd_hms(time))

# format for comparison with table
gpx <- gpx %>%
  mutate(year = year(time), 
      month = month(time), 
    day = day(time), 
    hour = hour(time), 
    min = minute(time), 
    sec = second(time), 
    lat = as.character(lat), 
    lon = as.character(lon), 
    time = as.character(time)) %>% 
  rename(gps = unit)

# find matches for times to assign lat long - there are more than one set of seconds (sec.y) that match
past <- left_join(past, gpx)
rm(gpx)
# # failed to assign lat lon - none failed as of 2018-03-20
# fail <- test %>% 
#   filter(is.na(lat))
past <- past %>% 
  mutate(lat = as.numeric(lat), 
    lon = as.numeric(lon)) # need to make decimal 5 digits - why? because that is all the gps can hold

past <- past %>% 
  select(-sec, -time, -elev, -month, -day, -hour, -min, -year) %>% 
  distinct()


coord <- past %>%
  group_by(tag_id) %>%
  summarise(mlat = mean(lat, na.rm = TRUE),
    mlon = mean(lon, na.rm = T))

# Examine the data
coord %>%
  select(tag_id, mlat, mlon)


# how many tags are scanned more than once? 89, most number of scans is 4, many of the 89 are 2
# test <- coord %>% 
#   group_by(tag_id) %>% 
#   summarise(num_scans = n()) %>% 
#   filter(num_scans > 1)
# # get each tag down to one observation - this is just anem_id and lat long, no extra info here
# out <- out %>%
#   group_by(anem_id) %>%
#   summarise(mlat = mean(lat, na.rm = TRUE),
#     mlon = mean(lon, na.rm = T))

readr::write_csv(coord, str_c("data/unscanned_pit_tags_for_QGIS_2018_", Sys.Date(), ".csv", sep = ""))

### MOVE THIS CSV TO THE PHILS_GIS_R DATA DIRECTORY ###







