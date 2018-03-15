# add 2018 anems to the QGIS map from the field data

# SETUP ####
library(dplyr)
library(tidyr)
library(lubridate)
source("scripts/field_helpers.R")
source("scripts/readGPXGarmin.R")

# if data is accessible in google sheets:
library(googlesheets)
# gs_auth(new_user = TRUE) # run this if having authorization problems
mykey <- '1symhfmpQYH8k9dAvp8yV_j_wCTpIT8gO9No4s2OIQXo' # access the file
entry <-gs_key(mykey)

# dive data ####
dive <- gs_read(entry, ws="diveinfo") %>% 
  select(dive_num, date, contains("gps"))

# anemone data ####
anem <-gs_read(entry, ws='clownfish') %>% 
  filter(!is.na(anem_id), 
    anem_id != "NULL") %>% 
  separate(obs_time, into = c("hour", "minute", "second"), sep = ":") %>% 
  mutate(gpx_hour = as.numeric(hour) - 8) %>% 
  mutate(minute = as.numeric(minute))

anem <- left_join(anem, dive, by = "dive_num")
rm(dive)

# fix date if gpx hour is less than 0 ####
test <- anem %>% 
  filter(gpx_hour < 0)

if (nrow(test) > 0){
  anem <- anem %>%
    mutate(gpx_date = date) # create a new gpx date column
  
  other <- anem %>% 
    filter(gpx_hour < 0) 
  
  # remove from anem table
  anem <- anti_join(anem, other)
  
  # subtract date
  other <- other %>% 
    mutate(gpx_date = as.character(ymd(date) - days(1))) %>% 
    mutate(gpx_hour = gpx_hour + 24)
  
  # rejoin rows
  anem <- rbind(anem, other)
  
}else{
  anem <- anem %>% mutate(gpx_date = date)
}
rm(test)

anem <- anem %>% 
  mutate(gpx_date = date(gpx_date))

# lat lon data ####

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

# adjust formatting
lat <- gpx %>% 
  mutate(gpx_date = date(time)) %>% 
  mutate(gpx_hour = hour(time), 
    minute = minute(time),
    second = second(time)) %>% 
  select(-time, -second, -elev)

# average the 4 observations per minute into one observation ####
mean_lat <- lat %>%
  group_by(unit, gpx_date, gpx_hour, minute) %>% 
  summarise(lat = mean(as.numeric(lat)),
    lon = mean(as.numeric(lon))) %>% 
  rename(gps = unit)

# turn gps column in mean_lat into integer so can merge with anem
mean_lat$gps <- as.integer(mean_lat$gps)

# add to anem observations - RIGHT NOW, AFTER ROW 16, LAT/LON ARE NA - WHY? - b/c they have gpx hour 1,2,3,5,6 and the gps have 4-7
anem <- left_join(anem, mean_lat, by = c("gps", "gpx_date", "gpx_hour", "minute"))
rm(lat, mean_lat)

anem <- anem %>% 
  select(lat, lon, date, anem_id)

# merge multiple observations of anemones into one #### 
anem <- anem %>% 
  group_by(anem_id) %>% 
  summarize(lat = mean(lat),
    lon = mean(lon))

write.csv(anem, paste("data/GPSSurvey_anemlatlon_forQGIS", Sys.Date(), ".csv", sep = ""))
  
