# a script to map the unknown anem_ids to see if we can assign a value to them while in the field.
library(dplyr)
library(lubridate)
library(readr)
library(stringr)
source("scripts/field_helpers.R")

get_from_google()

# # if no network
# get_data_no_net()
# load(clown_filename)
# load(dive_filename)


# find all of the instances of -9999 in the clown data sheet

unreadable <- clown %>% 
  filter(anem_id == "-9999" | old_anem_id == "-9999") %>% 
  select(dive_num, obs_time, old_anem_id, anem_id, gps, notes, anem_spp) %>% 
  mutate(gps = as.integer(ifelse(is.na(gps), 4, gps)))
# rm(clown)

if (nrow(unreadable) > 0){
  
# add dive info
dive <- dive %>% 
  select(dive_num, gps, date)
unreadable <- left_join(unreadable, dive)
# rm(dive)

# debugonce(assign_gpx_field)
coord <- assign_gpx_field(unreadable)

# currently have 4 lat lon points for each observation, combine those into one obs
means <- coord %>%
  group_by(id) %>%
  summarise(mlat = mean(lat, na.rm = TRUE),
    mlon = mean(lon, na.rm = T))

# Examine the data
coord <- left_join(coord, means) %>% 
  select(-lat, -lon) %>% 
  distinct()
  

write_csv(coord, str_c("data/unreadable_anems", Sys.Date(), ".csv", sep = ""))

### MOVE THIS CSV TO THE PHILS_GIS_R DATA DIRECTORY ###
}else{
  print("all anems are defined")
}
