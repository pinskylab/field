## ----setup--------------------------------------------------
library(dplyr)
library(lubridate)
library(stringr)
library(readr)
options(digits=8)


# import anemone data --------------------------------------
anem_raw <- read_csv("data/2018_clownfish.csv")
names(anem_raw) <- str_to_lower(names(anem_raw))
anem <- anem_raw %>% 
  filter(!is.na(row_id), 
         !is.na(anem_spp)) %>% 
  distinct()
rm(anem_raw)

# import dive data -----------------------------------------------
surv_raw <- read_csv("data/2018_diveinfo.csv")
names(surv_raw) <- stringr::str_to_lower(names(surv_raw))
surv <- surv_raw %>% 
  filter(dive_num %in% !!anem$dive_num) %>% 
  select(dive_num, date, site, municipality, cover) %>% 
  distinct()
rm(surv_raw)

# convert data into a format that can be matched with gps ----
anem_surv <- left_join(anem, surv, by = "dive_num") %>% 
  mutate(date_time = str_c(date, obs_time, sep = " "), 
         format_time = ymd_hms(date_time), 
         asia_time = force_tz(format_time, tzone = "Asia/Manila"),
         gpx_time = with_tz(asia_time, tzone = "UTC"),
         gps = as.character(gps), 
         month = month(gpx_time),
         day = day(gpx_time),
         hour = hour(gpx_time),
         min = minute(gpx_time), 
         sec = second(gpx_time),
         year = year(gpx_time)) %>% 
  select(-date_time, -obs_time, -date, -format_time, -asia_time)
rm(anem, surv)

# create a data frame to hold gpx data ----
gpx <- tibble()

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



## ----------------------------------------------------------------------------------------
# gpx$lat <- as.character(gpx$lat)
# gpx$lon <- as.character(gpx$lon)
# gpx$time <- as.character(gpx$time)


## ----------------------------------------------------------------------------------------
anem <- left_join(anem_surv, gpx, by = c("month", "day", "hour", "min", c("gps" = "unit")))
anem$lat <- as.numeric(anem$lat)
anem$lon <- as.numeric(anem$lon) # need to make decimal 5 digits


## ----------------------------------------------------------------------------------------
coord <- anem %>% 
  group_by(id) %>% 
  summarise(mlat = mean(lat, na.rm = TRUE),
    mlon = mean(lon, na.rm = T))


## ----------------------------------------------------------------------------------------
anem <- select(anem, row_id, anem_spp, anem_id, fish_spp, gpx_time, site, municipality)

anem <- left_join(coord, anem, by = "id")
anem <- rename(anem, lat = mlat, lon = mlon)

anem <- distinct(anem)
anem <- select(anem, -id)
anem <- distinct(anem)
anem <- mutate(anem, anem_table_id = 1:nrow(anem))


## ----------------------------------------------------------------------------------------
anem <- anem %>%
  arrange(anem_table_id, gpx_time)


## ----------------------------------------------------------------------------------------
anem %>% 
  select(anem_table_id, gpx_time, anem_spp, fish_spp, lat, lon, anem_id)


## Add fish info for maps -----
fish <- anem_raw %>% 
  filter(!is.na(fish_spp) & fish_spp != "")
fish$notes <- paste(fish$anem_spp, fish$anem_id, "w/", fish$fish_spp, sep = " ")
fish <- select(fish, lat, lon, notes, gpx_time, site, municipality)
anem <- anem %>% 
  filter(!is.na(anemspp) & anemspp != "" & is.na(spp)) %>% 
  mutate(notes = anemspp) %>% 
  select(lat, lon, notes, gpx_time, site, municipality)

out <- rbind(fish,anem)
out <- distinct(out)

write_csv(out, str_c("data/GPSSurvey_anemlatlon_forQGIS_", anem_surv$year[1], "_", Sys.Date(), ".csv", sep = ""))

