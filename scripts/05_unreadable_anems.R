# a script to map the unknown anem_ids to see if we can assign a value to them while in the field.
library(dplyr)
library(lubridate)
library(readr)
library(stringr)
source("scripts/field_helpers.R")

get_from_google()

# # load data from saved if network connection is lost ####
# # get list of files
# clown_files <- sort(list.files(path = "data/google_sheet_backups/", pattern = "clown_201*"), decreasing = T)
# dive_files <- sort(list.files(path = "data/google_sheet_backups/", pattern = "dive_201*"), decreasing = T)
# load(file = paste("data/google_sheet_backups/", clown_files[1], sep = ""))
# load(file = paste("data/google_sheet_backups/", dive_files[1], sep = ""))


# find all of the instances of -9999 in the clown data sheet

unreadable <- clown %>% 
  filter(anem_id == "-9999" | old_anem_id == "-9999") %>% 
  select(dive_num, obs_time, old_anem_id, anem_id, gps, notes) %>% 
  mutate(gps = as.integer(ifelse(is.na(gps), 4, gps)))
# rm(clown)

# add dive info
dive <- dive %>% 
  select(dive_num, gps, date)
unreadable <- left_join(unreadable, dive)
# rm(dive)

# add lat lons
anem <- unreadable %>%
  mutate(obs_time = force_tz(ymd_hms(str_c(date, obs_time, sep = " ")), tzone = "Asia/Manila"), 
    gps = as.character(gps))

# convert to UTC
anem <- mutate(anem, obs_time = with_tz(obs_time, tzone = "UTC")) %>% 
    distinct()
anem <- mutate(anem, id = 1:nrow(anem))

# debugonce(assign_gpx_field)
coord <- assign_gpx_field(anem)

# currently have 4 lat lon points for each observation, combine those into one obs
means <- coord %>%
  group_by(id) %>%
  summarise(mlat = mean(lat, na.rm = TRUE),
    mlon = mean(lon, na.rm = T))

# Examine the data
coord <- left_join(coord, means) %>% 
  select(-lat, -lon) %>% 
  distinct()
  

anem <- left_join(anem, coord)

write_csv(anem, str_c("data/unreadable_anems", Sys.Date(), ".csv", sep = ""))

### MOVE THIS CSV TO THE PHILS_GIS_R DATA DIRECTORY ###
