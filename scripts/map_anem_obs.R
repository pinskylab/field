# map_anem_obs  
#'  generating a csv of lat longs that can be put into QGIS to map anemones that have been observed more than once
library(tidyr)
library(lubridate)
# library(RMySQL)
source("scripts/con_leyte.R")

leyte <- conleyte()
# leytes <- leyte <- dbConnect(MySQL(), "Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)

# fetch anems that have an anem_obs value
anem <- leyte %>% 
  tbl("anemones") %>% 
  filter(!is.na(anem_obs)) %>% 
  select(dive_table_id, obs_time, anem_obs) %>% 
  collect()

# fetch dates & gps units
dive <- leyte %>% 
  tbl("diveinfo") %>% 
  filter(dive_table_id %in% anem$dive_table_id) %>% 
  select(dive_table_id, date, gps) %>% 
  collect()

# join anem data with dive data
anem <- left_join(anem, dive, by = "dive_table_id")

# create columns to make the table more compatible with the gpx table
anem <- anem %>% 
  separate(obs_time, into = c("hour", "minute", "second"), sep = ":") %>% 
  mutate(gpx_hour = as.numeric(hour) - 8) %>% 
  mutate(minute = as.numeric(minute))

# fix date if gpx hour is less than 0
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

anem <- rename(anem, unit = gps)

# find the lat long for this anem
lat <- leyte %>%
  tbl("GPX") %>%
  filter(date(time) %in% anem$gpx_date) %>%
  mutate(gpx_date = date(time)) %>% 
  mutate(gpx_hour = hour(time)) %>%
  mutate(minute = minute(time)) %>%
  mutate(second = second(time)) %>%
  select(-time, -second) %>%
  collect(n = Inf)

# summarize the 4 readings per minute down to one using mean  
sum_lat <- lat %>%
  group_by(unit, gpx_date, gpx_hour, minute) %>% 
  summarise(
    lat = mean(as.numeric(lat)),
    lon = mean(as.numeric(lon)))
rm(lat)

# join the lat lon to the anems
anem <- left_join(anem, sum_lat, by = c("unit", "gpx_date", "gpx_hour", "minute"))

rm(dive, other, sum_lat, test)

# group by anem_obs and get the mean lat long
final <- anem %>% 
  group_by(anem_obs) %>% 
  summarize(
    lat2 = mean(lat), 
    lon2 = mean(lon))

file_loc <- "~/Documents/Phils_GIS/Anems/_"
write.csv(final, file = paste(file_loc, Sys.Date(), "_anem_obs.csv", sep = ""))

