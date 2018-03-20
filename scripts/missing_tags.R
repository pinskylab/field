# how many fish were tagged in the past that we have not captured this season


# an important feature of this is how to assign lat long to observations - we want to assign lat long to pit scans

# start with the anem_to_qgis script and modify to assign to the observations in the bioterm file instead of the anemone table

library(lubridate)
library(stringr)
source("scripts/field_helpers.R")

# get tagged fish from bioterm file ####
pit <- from_scanner("data/BioTerm.txt") %>% 
  filter(substr(scan,1,3) != "989" & substr(scan,1,3) != "999") %>% # get rid of test tags
  arrange(date, time) %>% 
  mutate(date = ymd(date), 
    date_time = force_tz(ymd_hms(str_c(date, time, sep = " ")), tzone = "Asia/Manila"))



# separate data into the past and the current field season ####
# find only this year - format of date should be 18-01-01
past <- filter(pit, substr(date, 3, 4) != "18")
present <- filter(pit, substr(date, 3, 4) == "18")

# add missing data from db ####
pit_db <- read.csv(stringsAsFactors = F, file = "data/pitscan.csv", na = "NULL") %>% 
  unite(scan, city, tag, sep = "") %>% 
  select(-notes) %>% 
  distinct()

# combine db pitscans with past ####
past <- rbind(past, pit_db) %>% 
  distinct()
rm(pit, pit_db)

# need gps, get a list of dives done on these dates ####

# for previous years
dive <- read.csv(stringsAsFactors = F, file = "data/diveinfo.csv", na = "NULL") %>% 
  mutate(date = as.Date(date)) %>% 
  filter(date %in% past$date, dive_type == "C") %>% 
  select(date, gps)
past <- left_join(past, dive, by = "date") %>%  
  distinct()
rm(dive)

# for current year # in 2018 gps4 was used for pit tagger
present <- mutate(present, gps = 4)

# format scans to compare with gps ####
# convert to UTC
past <- past %>% 
  mutate(date_time = with_tz(date_time, tzone = "UTC"))
present <- present %>% 
  mutate(date_time = with_tz(date_time, tzone = "UTC"))

# split out time components to compare to latlong
past <- past %>%
  mutate(month = month(date)) %>%
  mutate(day = day(date)) %>%
  mutate(hour = hour(date_time)) %>%
  mutate(min = minute(date_time)) %>%
  mutate(sec = second(date_time)) %>%
  mutate(year = year(date))

present <- present %>%
  mutate(month = month(date)) %>%
  mutate(day = day(date)) %>%
  mutate(hour = hour(date_time)) %>%
  mutate(min = minute(date_time)) %>%
  mutate(sec = second(date_time)) %>%
  mutate(year = year(date))



