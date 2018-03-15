<<<<<<< HEAD
# ---------------------------------------------
#   Set up work space - load packages and data
# ---------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)
source("scripts/field_helpers.R")
source("scripts/readGPXGarmin.R")

# fix 2017 anem data
anem <- read.csv(stringsAsFactors = F, file="data/anemones.csv")




# ---------------------------------------------
#   Read data and format
# ---------------------------------------------

# # # if data is accessible in google sheets:
# # library(googlesheets)
# # # gs_auth(new_user = TRUE) # run this if having authorization problems
# # mykey <- '1symhfmpQYH8k9dAvp8yV_j_wCTpIT8gO9No4s2OIQXo' # access the file
# # entry <-gs_key(mykey)
# # clown <-gs_read(entry, ws='clownfish')
# # surv <- gs_read(entry, ws="diveinfo")
# 
# 
# anem <- clown
names(anem) <- tolower(names(anem))
# anem <- filter(anem, !is.na(dive_num))
anem <- mutate(anem, dive_table_id = as.numeric(dive_table_id)) %>% 
  filter(!is.na(dive_table_id))
# anem$id <- 1:nrow(anem) # add row numbers so that gpx can be added later (creates 4 rows for each anem) anem_table_id takes care of this


dive <- read.csv(stringsAsFactors = F, file="data/diveinfo.csv")
names(dive) <- tolower(names(dive))
dive <- dive %>%
  filter(dive_table_id %in% anem$dive_table_id) %>%
  select(dive_num, date, site, municipality, cover, gps, dive_table_id)
dive <- distinct(dive)

# ---------------------------------------------
#   add dates and sites to anems
# ---------------------------------------------

anem <- left_join(anem, dive, by = "dive_table_id")

# remove lines that are not anemones
anem <- anem %>%
  filter(!is.na(anem_spp), 
    # find only 2017 anemones
    grepl("2017", date)
  ) %>% 
  mutate(obs_time = as.POSIXct(obs_time, format = "%H:%M:%S"))

# # convert to UTC
anem$obs_time <- with_tz(anem$obs_time, tzone = "UTC")

# convert GPS to character
anem$gps <- as.character(anem$gps)

# split out time components to compare to latlong
anem <- anem %>%
  mutate(month = month(date)) %>%
  mutate(day = day(date)) %>%
  mutate(hour = hour(obs_time)) %>%
  mutate(min = minute(obs_time)) %>%
  mutate(sec = second(obs_time)) %>%
  mutate(year = year(date))

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


#### HERE CAN I USE GROUP BY ID OR obs_time AND THEN SUMMARISE TO GET THE MEAN LAT LON OR MIN LAT LON AND CREATE A NEW TABLE WITH ALL COLUMNS BUT ONLY ONE LAT LON PER OBS

coord <- anem %>%
  group_by(anem_table_id) %>%
  summarise(mlat = mean(lat, na.rm = TRUE),
    mlon = mean(lon, na.rm = T))


# drop all of the unneccessary columns from anem and join with the coord
anem <- select(anem, anem_table_id, anem_spp, anem_id, obs_time, site)

anem <- left_join(coord, anem, by = "anem_table_id")
anem <- rename(anem, lat = mlat, lon = mlon)


anem <- select(anem, -anem_table_id)
anem <- distinct(anem)


# Sort the data
anem <- anem %>%
  arrange(obs_time)

# Examine the data
anem %>%
  select(obs_time, anem_spp, fish_spp, lat, lon, anem_id)

# Write out for QGIS (has column headers)
# add notes based on whether or not the anem has fish on it
fish <- anem %>%
  filter(!is.na(fish_spp) & fish_spp != "")
fish$notes <- paste(fish$anem_spp, fish$anem_id, "w/", fish$fish_spp, sep = " ")
fish <- select(fish, lat, lon, notes, obs_time, site, anem_id)
# what isn't in fish?
anem <- anem %>%
  filter(!is.na(anem_spp) & anem_spp != "" & is.na(fish_spp)) %>%
  mutate(notes = anem_spp) %>%
  select(lat, lon, notes, obs_time, site, anem_id)

out <- rbind(fish,anem)
out <- distinct(out)

# get each anemone down to one observation - this is just anem_id and lat long, no extra info here
out <- out %>%
  group_by(anem_id) %>%
  summarise(mlat = mean(lat, na.rm = TRUE),
    mlon = mean(lon, na.rm = T))


write_csv(out, str_c("data/GPSSurvey_anemlatlon_forQGIS_2017", Sys.Date(), ".csv", sep = ""))

=======
# need to fix the anemones for 2017 - in the field so using saved database data instead of connecting to db

# SETUP ####
library(dplyr)
library(tidyr)
library(lubridate)


# which dives were done in 2017? ####
dive <- read.csv(stringsAsFactors = F, file = "data/diveinfo.csv") %>% 
  filter(grepl("2017", date)) %>% 
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
  
>>>>>>> eb297f8a0d1ba69587b77989e1949041949c833a
