# For 2017 surveys. Reads in from Excel file directly. 
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)
# library(readxl)
options(digits=8)
source("code/readGPXGarmin.R")
source("code/writeGPXGarmin.R")

#######################################
 # prep anemone data for QGIS ####
#######################################

excel_file <- ("data/GPSSurveys2017.xlsx")
anem <- readxl::read_excel(excel_file, sheet = "settlement", col_names = TRUE, na = "")
names(anem) <- str_to_lower(names(anem))
anem <- filter(anem, !is.na(obstime))
anem <- distinct(anem)

# add dates
surv <- readxl::read_excel(excel_file, sheet = "diveinfo", col_names=TRUE)
names(surv) <- stringr::str_to_lower(names(surv))

# eliminate unneccessary columns
surv <- surv %>% 
  filter(divenum %in% anem$divenum) %>% 
  select(divenum, date, site, municipality, cover)
surv <- distinct(surv)
    
anem <- left_join(anem, surv, by = "divenum")

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
  separate(obstime, into = c("baddate", "obstime"), sep = " ")  %>%
  select(-contains("bad")) 

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
# Read in each GPX file ####
folders  <-  list.files(path = "data", pattern ="gps") # move any empty folders out of the data folder
for (l in 1:length(folders)){
  files <- list.files(path = paste("data/",folders[l], sep = ""), pattern = "*Track*")
  for(i in 1:length(files)){ # for each file
    infile <- readGPXGarmin(paste("data/", folders[l], "/", files[i], sep="")) 
    header <- infile$header
    data <- infile$data
    data$time <- ymd_hms(data$time)
    data <- arrange(data, time)
    # change elevation to zero
    data$elev <- 0
    data$unit <- substr(folders[l], 4,4)
    # for loading the GPX data to the db, send to a datatable
    gpx <- rbind(gpx, data)
  }
}

gpx <- gpx %>% 
  mutate(month = month(time)) %>% 
  mutate(day = day(time)) %>% 
  mutate(hour = hour(time)) %>% 
  mutate(min = minute(time)) %>% 
  mutate(sec = second(time))




# # fix formatting
gpx$lat <- as.character(gpx$lat)
gpx$lon <- as.character(gpx$lon)
gpx$time <- as.character(gpx$time)

# find matches for times to assign lat long - there are more than one set of seconds (sec.y) that match
anem <- left_join(anem, gpx, by = c("month", "day", "hour", "min", c("gps" = "unit")))
anem$lat <- as.numeric(anem$lat)
anem$lon <- as.numeric(anem$lon) # need to make decimal 5 digits

#### HERE CAN I USE GROUP BY ID OR OBSTIME AND THEN SUMMARISE TO GET THE MEAN LAT LON OR MIN LAT LON AND CREATE A NEW TABLE WITH ALL COLUMNS BUT ONLY ONE LAT LON PER OBS

coord <- anem %>% 
  group_by(id) %>% 
  summarise(mlat = mean(lat, na.rm = TRUE),
    mlon = mean(lon, na.rm = T))


# drop all of the unneccessary columns from anem and join with the coord
anem <- select(anem, id, anemspp, anemid, numfish, spp, obstime, site, municipality)

anem <- left_join(coord, anem, by = "id")
anem <- rename(anem, lat = mlat, lon = mlon)

anem <- distinct(anem)
anem <- select(anem, -id)
anem <- distinct(anem)
anem <- mutate(anem, anem_table_id = 1:nrow(anem))

# Sort the data
anem <- anem %>%
  arrange(anem_table_id, obstime)

# Examine the data
anem %>% 
  select(anem_table_id, obstime, anemspp, spp, lat, lon, anemid)

# Write out for QGIS (has column headers)
# add notes based on whether or not the anem has fish on it
anem <- anem %>% 
  mutate(notes = paste(anemspp, anemid, sep = " ")) %>% 
  select(lat, lon, notes, obstime, site, municipality)

out <- anem
out <- distinct(out)

write_csv(out, str_c("data/GPSSurvey_settlement_forQGIS", anem$year[1], Sys.Date(), ".csv", sep = ""))


rm(coord, anem, surv, latlong, out, anems, anem, bad, fish, good, data, dives, gpx, files, folder, header, i, infile, l)

