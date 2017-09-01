# ---------------------------------------------
#   Set up work space - load packages and data
# ---------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)
source("scripts/field_func.R")
excel_file <- ("data/GPSSurveys2017.xlsx")
source("scripts/readGPXGarmin.R")

anem_col <- c("text", "text", "text", "text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text")

dive_col <- c("text", "text", "text", "text", "date", "text", "text", "text", "text", "date", "date", "date", "text", "date", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text")


# ---------------------------------------------
#   Read data and format
# ---------------------------------------------

anem <- excl("anemones", anem_col)
names(anem) <- tolower(names(anem))
anem <- filter(anem, !is.na(divenum))
anem <- distinct(anem)
anem$id <- 1:nrow(anem) # add row numbers so that gpx can be added later (creates 4 rows for each anem)

dive <- excl("diveinfo", dive_col)
names(dive) <- tolower(names(dive))
dive <- dive %>% 
  filter(divenum %in% anem$divenum) %>% 
  select(divenum, date, site, municipality, cover)
dive <- distinct(dive)

# ---------------------------------------------
#   add dates and sites to anems
# ---------------------------------------------

anem <- left_join(anem, dive, by = "divenum")

# find samples that are lacking an anemone
(lack <- anem %>% 
    filter(is.na(anemspp) & !is.na(spp)))
# if this is zero, 
rm(lack) # if it is not zero, look into what is going on on the data sheet

# remove lines that are not anemones and remove weird dates that excel attaches
anem <- anem %>% 
  filter(!is.na(anemspp))

# remove weird excel dates
anem <- anem %>% 
  separate(obstime, into = c("baddate", "obstime"), sep = " ") %>% 
  select(-baddate)


# get the date and time info for each anemone
anem$obstime <- str_c(anem$date, anem$obstime, sep = " ")
anem$obstime <- ymd_hms(anem$obstime)
anem$obstime <- force_tz(anem$obstime, tzone = "Asia/Manila")

# convert to UTC
anem$obstime <- with_tz(anem$obstime, tzone = "UTC")

# convert GPS to character
anem$gps <- as.character(anem$gps)

# split out time components to compare to latlong
anem <- anem %>% 
  mutate(month = month(obstime)) %>% 
  mutate(day = day(obstime)) %>% 
  mutate(hour = hour(obstime)) %>% 
  mutate(min = minute(obstime)) %>% 
  mutate(sec = second(obstime)) %>% 
  mutate(year = year(obstime))

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
anem$lon <- as.numeric(anem$lon) # need to make decimal 5 digits - why?


#### HERE CAN I USE GROUP BY ID OR OBSTIME AND THEN SUMMARISE TO GET THE MEAN LAT LON OR MIN LAT LON AND CREATE A NEW TABLE WITH ALL COLUMNS BUT ONLY ONE LAT LON PER OBS

coord <- anem %>% 
  group_by(id) %>% 
  summarise(mlat = mean(lat, na.rm = TRUE),
    mlon = mean(lon, na.rm = T))


# drop all of the unneccessary columns from anem and join with the coord
anem <- select(anem, id, anemspp, anemid, numfish, spp, obstime, site, municipality)

anem <- left_join(coord, anem, by = "id")
anem <- rename(anem, lat = mlat, lon = mlon)


anem <- select(anem, -id)
anem <- distinct(anem)


# Sort the data
anem <- anem %>%
  arrange(obstime)

# Examine the data
anem %>% 
  select(obstime, anemspp, spp, lat, lon, anemid)

# Write out for QGIS (has column headers)
# add notes based on whether or not the anem has fish on it
fish <- anem %>% 
  filter(!is.na(spp) & spp != "")
fish$notes <- paste(fish$anemspp, fish$anemid, "w/", fish$numfish, fish$spp, sep = " ")
fish <- select(fish, lat, lon, notes, obstime, site, municipality)
anem <- anem %>% 
  filter(!is.na(anemspp) & anemspp != "" & is.na(spp)) %>% 
  mutate(notes = anemspp) %>% 
  select(lat, lon, notes, obstime, site, municipality)

out <- rbind(fish,anem)
out <- distinct(out)

write_csv(out, str_c("data/GPSSurvey_anemlatlon_forQGIS", anem$year[1], Sys.Date(), ".csv", sep = ""))


rm(coord, anem, surv, latlong, out, anems, anem, bad, fish, good, data, dives, gpx, files, folder, header, i, infile, l)

