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

# # if no network
# get_data_no_net()
# load(clown_filename)
# load(dive_filename)


anem <- clown %>% 
  filter(!is.na(anem_spp)) %>% # we only want anem_obs, not fish_obs 
  select(dive_num, obs_time, gps, anem_id, anem_spp)
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

# in order to use the **assign_gpx function**, need a table that contains an id, gps unit, and a date_time that have been converted to UTC time zone - the date_time column must be called "obs_time"
# debugonce(assign_gpx_field)
coord_table <- assign_gpx_field(anem) 

### WAIT ###

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
  select(obs_time, lat, lon, anem_id)

out <- anem
# out <- rbind(fish,anem)
out <- distinct(out)

# # get each anemone down to one observation - this is just anem_id and lat long, no extra info here
# out <- out %>%
#   group_by(anem_id) %>%
#   summarise(mlat = mean(lat, na.rm = TRUE),
#     mlon = mean(lon, na.rm = T))


# write_csv(out, str_c("../Phils_GIS_R/data/Anems/GPSSurvey_anemlatlon_forQGIS_2018_", Sys.Date(), ".csv", sep = ""))

