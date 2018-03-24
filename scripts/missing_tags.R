# how many fish were tagged in the past that we have not captured this season
  # wanted to pull the date times from the pit scanner and determine gps locations that way, however, not all scans were done during a dive (some were done on land, like the 2015 field season)

library(ggplot2)
library(lubridate)
library(stringr)
source("scripts/field_helpers.R")

# get currently scanned fish ####
# get_from_google()

# load data from saved if network connection is lost
# get list of files
clown_files <- sort(list.files(path = "data/google_sheet_backups/", pattern = "clown_201*"), decreasing = T)
dive_files <- sort(list.files(path = "data/google_sheet_backups/", pattern = "dive_201*"), decreasing = T)
load(file = paste("data/google_sheet_backups/", clown_files[1], sep = ""))
load(file = paste("data/google_sheet_backups/", dive_files[1], sep = ""))

# at this point there is a clown table of newly collected field data and a dive table of newly collected field data

# ---------------------------------------------
#   format tag ids on clownfish data sheet ####
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
  select(-dive_num) 

present <- present %>% 
  mutate(time = ymd_hms(str_c(date, obs_time, sep = " ")))
rm(clown, dive)

# add data from database #### 
# # on campus
# leyte <- read_db("Leyte")
# past <- leyte %>% 
#   tbl("clownfish") %>% 
# filter(!is.na(tag_id))

# in the field this is as Rdata
load(file = "data/db_backups/clownfish_db.Rdata")
load(file = "data/db_backups/anemones_db.Rdata") 
load(file = "data/db_backups/diveinfo_db.Rdata")

past <- clown %>% 
  filter(!is.na(tag_id)) %>% 
  # include fields to assign a date and time, also size 
  select(anem_table_id, tag_id, size, color)

anem <- anem %>% 
  filter(anem_table_id %in% past$anem_table_id) %>% 
  select(anem_table_id, dive_table_id, obs_time)
past <- left_join(past, anem, by = "anem_table_id")

dive <- dive %>% 
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
# gpx <- read.csv(file = "data/db_backups/GPX.csv", na = "NULL", stringsAsFactors = F)
# # # create rdata from db_backups
# save(gpx, file = "data/db_backups/gpx_db.Rdata")
load(file = "data/db_backups/gpx_db.Rdata")

gpx <- gpx %>% 
  mutate(time = ymd_hms(time)) %>% 
  select(-elev)

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
rm(gpx, clown, dive)
# # failed to assign lat lon - none failed as of 2018-03-20

# fail <- test %>% 
#   filter(is.na(lat))
past <- past %>% 
  # the gps can only hold 5 digits after the decimal
  mutate(lat = formatC(as.numeric(lat), digits = 7), 
    lon = formatC(as.numeric(lon), digits = 8))

past <- past %>% 
  select(-sec, -time, -month, -day, -hour, -min, -year) %>% 
  distinct() %>% 
  mutate(lat = as.numeric(lat), 
    lon = as.numeric(lon))

coord <- past %>%
  group_by(tag_id) %>%
  summarise(mlat = mean(lat, na.rm = T),
    mlon = mean(lon, na.rm = T))

# Examine the data
coord %>%
  select(tag_id, mlat, mlon)


# how many tags are scanned more than once? once was 89, most number of scans is 4, many of the 89 are 2; now it is 0
# test <- coord %>%
#   group_by(tag_id) %>%
#   summarise(num_scans = n()) %>%
#   filter(num_scans > 1)
# # get each tag down to one observation - this is just anem_id and lat long, no extra info here
# out <- out %>%
#   group_by(anem_id) %>%
#   summarise(mlat = mean(lat, na.rm = TRUE),
#     mlon = mean(lon, na.rm = T))

readr::write_csv(coord, str_c("../Phils_GIS_R/data/Fish/unscanned_pit_tags_for_QGIS_2018_", Sys.Date(), ".csv", sep = ""))









