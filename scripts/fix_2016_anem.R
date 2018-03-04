# need to fix the anemones for 2016 - in the field so using saved database data instead of connecting to db

# SETUP ####
library(dplyr)
library(tidyr)
library(lubridate)


# which dives were done in 2016? ####
dive <- read.csv(stringsAsFactors = F, file = "data/diveinfo.csv") %>% 
  filter(grepl("2016", date)) %>% 
  select(dive_table_id, date, gps)

# which anemones were seen on those dates? ####

anem <- read.csv(stringsAsFactors = F, file = "data/anemones.csv") %>% 
  filter(dive_table_id %in% dive$dive_table_id, 
    !is.na(anem_id), 
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
  filter(grepl("2016", time)) %>% 
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

write.csv(anem, paste("data/GPSSurvey_anemlatlon_forQGIS_2016_", Sys.Date(), ".csv", sep = ""))
  
