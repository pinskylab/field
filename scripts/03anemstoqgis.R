# ---------------------------------------------
#   Set up work space - load packages and data
# ---------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)
source("scripts/field_helpers.R")
source("scripts/readGPXGarmin.R")

# ---------------------------------------------
#   Read data and format
# ---------------------------------------------

get_from_google()


# # load data from saved if network connection is lost
# # THIS HAS TO BE MANUALLY UPDATED WITH MOST CURRENT VERSION OF SAVED FILE  - COULD WRITE CODE TO FIND AND LOAD THE MOST CURRENT VERSION ####
# load(file = "data/clown_2018-03-12 21:13:51.Rdata")
# load(file = "data/dive_2018-03-12 21:13:52.Rdata")


anem <- clown %>% 
  filter(!is.na(dive_num)) %>%
  distinct() %>% 
  mutate(gps = as.integer(gps))


anem$id <- 1:nrow(anem) # add row numbers so that gpx can be added later (creates 4 rows for each anem)

names(dive) <- tolower(names(dive))
dive <- dive %>%
  filter(dive_num %in% anem$dive_num) %>%
  select(dive_num, date, site, municipality, cover, gps) %>% 
  distinct()

# if gps was not specified on the line, use Michelle's gps because she was the anem obs person on the early dives.
anem <- anem %>% 
  mutate(gps = ifelse(is.na(gps), 4, gps))


# ---------------------------------------------
#   add dates and sites to anems
# ---------------------------------------------

anem <- left_join(anem, dive, by = c("dive_num", "gps"))

# create a table of the lines that do not have a time and remove them
no_gps <- anem %>% 
  filter(is.na(obs_time))
anem <- anti_join(anem, no_gps)

# remove lines that are not anemones and remove weird dates that excel attaches and convert time zone
anem <- anem %>%
  filter(!is.na(anem_spp)) %>% 
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

coord <- anem %>%
  group_by(id) %>%
  summarise(mlat = mean(lat, na.rm = TRUE),
    mlon = mean(lon, na.rm = T))


# drop all of the unneccessary columns from anem and join with the coord
anem <- select(anem, id, anem_spp, anem_id, fish_spp, obs_time, site)

anem <- left_join(coord, anem, by = "id")
anem <- rename(anem, lat = mlat, lon = mlon)

anem <- select(anem, -id)
anem <- distinct(anem)


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
  select(lat, lon, obs_time, site, anem_id) ##ALLISON NOTE: added year hear b/c was needed in write_csv line below ## MRS - removed year because it doesn't exist in the table?
out <- anem
# out <- rbind(fish,anem)
out <- distinct(out)

# # get each anemone down to one observation - this is just anem_id and lat long, no extra info here
# out <- out %>%
#   group_by(anem_id) %>%
#   summarise(mlat = mean(lat, na.rm = TRUE),
#     mlon = mean(lon, na.rm = T))


write_csv(out, str_c("data/GPSSurvey_anemlatlon_forQGIS_2018_", Sys.Date(), ".csv", sep = ""))

### MOVE THIS CSV TO THE PHILS_GIS_R DATA DIRECTORY ###

