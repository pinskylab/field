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

