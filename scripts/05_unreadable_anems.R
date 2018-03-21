# a script to map the unknown anem_ids to see if we can assign a value to them while in the field.
library(dplyr)
library(lubridate)
library(readr)
source("scripts/field_helpers.R")

get_from_google()

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
anem <- mutate(anem, obs_time = with_tz(obs_time, tzone = "UTC"))

# split out time components to compare to latlong
anem <- anem %>%
  mutate(month = month(obs_time)) %>%
  mutate(day = day(obs_time)) %>%
  mutate(hour = hour(obs_time)) %>%
  mutate(min = minute(obs_time)) %>%
  mutate(sec = second(obs_time)) %>%
  mutate(year = year(obs_time))

# create table of lat lon data
# make an empty data frame for later
gpx <- data.frame()

# define the list of gps units
gps <- name_gps()

# determine which gps units have data & remove empties
gps <- used_gps(gps)

for (l in 1:length(gps)){
  files <- list.files(path = paste("data/",gps[l], sep = ""), pattern = "*Track*")
  for(i in 1:length(files)){ # for each file
    dat <- prep_gpx(gps, files) # parse the gpx into useable data
    gpx <- rbind(gpx, dat)
  }
}

gpx <- gpx %>%
  mutate(year = year(time)) %>% 
  mutate(month = month(time)) %>%
  mutate(day = day(time)) %>%
  mutate(hour = hour(time)) %>%
  mutate(min = minute(time)) %>%
  mutate(sec = second(time))

# fix formatting
gpx$lat <- as.character(gpx$lat) # otherwise they import as factors
gpx$lon <- as.character(gpx$lon)
gpx$time <- as.character(gpx$time)

# find matches for times to assign lat long - there are more than one set of seconds (sec.y) that match
anem <- left_join(anem, gpx, by = c("month", "day", "hour", "min", c("gps" = "unit")))
anem$lat <- as.numeric(anem$lat)
anem$lon <- as.numeric(anem$lon) # need to make decimal 5 digits - why? because that is all the gps can hold

# currently have 4 lat lon points for each observation, combine those into one obs
coord <- anem %>%
  group_by(obs_time) %>%
  summarise(mlat = mean(lat, na.rm = TRUE),
    mlon = mean(lon, na.rm = T))


# drop all of the unneccessary columns from anem and join with the coord
anem <- select(anem, anem_id, obs_time, notes)
anem <- left_join(coord, anem, by = "obs_time") %>% 
  rename(lat = mlat, lon = mlon)

anem <- distinct(anem)

# Sort the data
anem <- anem %>%
  arrange(obs_time)

# Examine the data
anem %>%
  select(obs_time, lat, lon, anem_id, notes)


write_csv(anem, str_c("data/unreadable_anems", Sys.Date(), ".csv", sep = ""))

### MOVE THIS CSV TO THE PHILS_GIS_R DATA DIRECTORY ###
