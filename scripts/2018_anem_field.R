# add 2018 anems to the QGIS map from the field data

# SETUP ####
library(dplyr)
library(tidyr)
library(lubridate)
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
  mutate(dive_table_id = as.integer(dive_table_id)) %>% 
  separate(obs_time, into = c("hour", "minute", "second"), sep = ":") %>% 
  mutate(gpx_hour = as.numeric(hour) - 8) %>% 
  mutate(minute = as.numeric(minute))

anem <- left_join(anem, dive, by = "dive_table_id")
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
  mutate(gpx_date = date(gpx_date), 
    gps = as.integer(gps))

# find the lat long for this anem ####
lat <- read.csv(stringsAsFactors = F, file = "data/GPX.csv") %>% 
  filter(grepl("2017", time)) %>% 
  mutate(gpx_date = date(time)) %>% 
  filter(gpx_date %in% anem$gpx_date) %>% 
  mutate(gpx_hour = hour(time), 
    minute = minute(time),
    second = second(time)) %>% 
  select(-time, -second)

# average the 4 observations per minute into one observation ####
mean_lat <- lat %>%
  group_by(unit, gpx_date, gpx_hour, minute) %>% 
  summarise(lat = mean(as.numeric(lat)),
    lon = mean(as.numeric(lon))) %>% 
  rename(gps = unit)

anem <- left_join(anem, mean_lat, by = c("gps", "gpx_date", "gpx_hour", "minute"))
rm(lat, mean_lat)

anem <- anem %>% 
  select(lat, lon, date, anem_id)

# merge multiple observations of anemones into one ####
anem <- anem %>% 
  group_by(anem_id) %>% 
  summarize(lat = mean(lat),
    lon = mean(lon))

write.csv(anem, paste("data/GPSSurvey_anemlatlon_forQGIS_2017_", Sys.Date(), ".csv", sep = ""))
  
