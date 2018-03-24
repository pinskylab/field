# ---------------------------------------------
#   Set up work space - load packages and data
# ---------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)
source("scripts/field_helpers.R")

# ---------------------------------------------
#   Read data and format
# ---------------------------------------------

get_from_google()

# load data from saved if network connection is lost ####
# get list of files
# clown_files <- sort(list.files(path = "data/", pattern = "clown_201*"), decreasing = T)
# dive_files <- sort(list.files(path = "data/", pattern = "dive_201*"), decreasing = T)
# load(file = paste("data/", clown_files[1], sep = ""))
# load(file = paste("data/", dive_files[1], sep = ""))

anem <- clown %>% 
  filter(!is.na(anem_spp)) %>% # we only want anem_obs, not fish_obs 
  select(dive_num, obs_time, gps, anem_id)
# if gps was not specified on the line, use Michelle's gps because she was the anem obs person on the early dives.
anem <- anem %>% 
  mutate(gps = ifelse(is.na(gps), 4, gps)) %>% 
  filter(!is.na(anem_id)) # now that we have only anem_obs, want only anems with ids

dive <- dive %>%
  filter(dive_num %in% anem$dive_num) %>%
  select(dive_num, date, gps) %>% 
  distinct()

# ---------------------------------------------
#   add dates and sites to anems
# ---------------------------------------------

anem <- left_join(anem, dive)

# create a table of the lines that do not have a time and remove them
no_gps <- anem %>% 
  filter(is.na(obs_time))
anem <- anti_join(anem, no_gps)

# combine the date_time and assign the Philippines time zone
anem <- anem %>%
  mutate(obs_time = force_tz(ymd_hms(str_c(date, obs_time, sep = " ")), tzone = "Asia/Manila"), 
    gps = as.character(gps))

# convert to UTC
anem <- mutate(anem, obs_time = with_tz(obs_time, tzone = "UTC"))

# in order to use the **assign_gpx function**, need a table that contains an id, gps unit, and a date_time that have been converted to UTC time zone - the date_time column must be called "obs_time"
# debugonce(assign_gpx_field)
coord_table <- assign_gpx_field(anem) 

coord_table <- distinct(coord_table)

anem <- left_join(anem, coord_table) %>% 
  distinct()

# how many anems have more than one obs?
multi <- anem %>% 
  group_by(anem_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)

# Sort the data
anem <- anem %>%
  arrange(obs_time)

# Examine the data
anem %>%
  select(obs_time, anem_spp, fish_spp, lat, lon, anem_id)

# Write out for QGIS (has column headers)
# add notes based on whether or not the anem has fish on it
# fish <- anem %>%
#   filter(!is.na(fish_spp) & fish_spp != "")
# fish$notes <- paste(fish$anem_spp, fish$anem_id, "w/", fish$fish_spp, sep = " ")
# fish <- select(fish, lat, lon, notes, obs_time, site, anem_id)
# # what isn't in fish?
anem <- anem %>%
  select(lat, lon, obs_time, site, anem_id) ##ALLISON NOTE: added year here b/c was needed in write_csv line below ## MRS - removed year because it doesn't exist in the table?
out <- anem
# out <- rbind(fish,anem)
out <- distinct(out)

# # get each anemone down to one observation - this is just anem_id and lat long, no extra info here
# out <- out %>%
#   group_by(anem_id) %>%
#   summarise(mlat = mean(lat, na.rm = TRUE),
#     mlon = mean(lon, na.rm = T))


write_csv(out, str_c("../Phils_GIS_R/data/Anems/GPSSurvey_anemlatlon_forQGIS_2018_", Sys.Date(), ".csv", sep = ""))

