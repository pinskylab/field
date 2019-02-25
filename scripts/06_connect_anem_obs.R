# connect current field anems to anem obs and map one dot for each anemone
library(stringr)
source("scripts/field_helpers.R")

# get past anems from db and assign lat lons
# past_anem <- assign_db_gpx_field()
past_anem <- assign_db_gpx()

### WAIT ### this takes quite a bit of time - is there a way to improve the code?

# get current anems from google and assign lat lons

# if network
get_from_google()

# # if no network
# get_data_no_net()
# load(clown_filename)
# load(dive_filename)

# which field observations are anems with a tag?
field_anem <- clown %>% 
  filter(!is.na(anem_spp), 
    !is.na(anem_id)) %>% 
  select(row_id, dive_num, obs_time, gps, anem_spp, old_anem_id, anem_id)

# connect date and gps
dive <- dive %>% 
  select(dive_num, gps, date)
field_anem_dive <- left_join(field_anem, dive, by = c("dive_num", "gps"))


curr_anem <- assign_gpx_field(field_anem_dive)

### WAIT ### it is ok if 4 failed to parse

curr_anem <- curr_anem %>% 
  select(lat, lon, old_anem_id, anem_id, anem_spp) %>% 
  mutate(anem_obs = NA, 
    era = "present")

all_anem <- rbind(past_anem, curr_anem)
rm(field_anem, field_anem_dive)

# which anemones already have an anem_obs number?
have_obs <- all_anem %>% 
  filter(!is.na(anem_obs)) %>% 
  select(anem_id, anem_obs) %>% 
  distinct() %>% 
  arrange(anem_id)

# attach anem_obs to any new anem observations that have been seen before, can only do this by removing and reattaching anem_obs
all_anem <- select(all_anem, -anem_obs)
all_anem <- left_join(all_anem, have_obs, by = "anem_id")


# which of these have an old_anem_id but no anem_obs?
old_obs <- all_anem %>% 
  filter(!is.na(old_anem_id) & is.na(anem_obs))

# attach existing anem_obs to any old_anem_id that has already been assigned one
old_obs <- select(old_obs, -anem_obs)
old_obs <- left_join(old_obs, have_obs, by = c("old_anem_id" = "anem_id"))

# update these rows in all_anem
all_anem <- anti_join(all_anem, old_obs, by = c("lat", "lon", "old_anem_id", "anem_id", "era"))
all_anem <- rbind(all_anem, old_obs)
rm(old_obs)

# which anemones have old_anem_id and no anem_obs (still)
have_old <- all_anem %>% 
  filter(!is.na(old_anem_id) & is.na(anem_obs))

# assign an anem obs to anems with no obs and an old_anem_id ####
x <- max(all_anem$anem_obs, na.rm = T)
y <- x + 1
z <- x+nrow(have_old)
have_old <- have_old %>% 
  mutate(anem_obs = y:z)
# split out the old anem ids with their newly assigned anem obs
olds <- have_old %>% 
  select(old_anem_id, anem_obs) %>% 
  rename(anem_id = old_anem_id)
# do the same for anem_ids
anems <- have_old %>% 
  select(anem_id, anem_obs)
# combine
new_obs <- rbind(olds, anems)
# add to have_obs
have_obs <- rbind(have_obs, new_obs)
# attach to all_anem
all_anem <- select(all_anem, -anem_obs) 
all_anem <- left_join(all_anem, have_obs, by = "anem_id")
rm(new_obs, olds, anems, have_old)

# assign anem_obs to anemones that have been seen more than once but don't have an old_id ####
multi <- all_anem %>% 
  filter(is.na(anem_obs)) %>% 
  group_by(anem_id) %>% 
  summarise(visits = n()) %>% 
  filter(visits > 1)

# assign anem_obs
x <- max(all_anem$anem_obs, na.rm = T)
y <- x + 1
z <- x+nrow(multi)
multi <- multi %>% 
  mutate(anem_obs = y:z) %>% 
  select(-visits)

# attach to the anem_obs list
have_obs <- rbind(have_obs, multi)

# attach to all_anem
all_anem <- select(all_anem, -anem_obs)
all_anem <- left_join(all_anem, have_obs, by = "anem_id")

# remove the lines containing these anem_obs from all_anem
anemObs <- all_anem %>% 
  filter(!is.na(anem_obs))
all_anem <- anti_join(all_anem, anemObs, by = c("lat", "lon", "old_anem_id", "anem_id", "era", "anem_obs"))

# create mean lat lons for all anem_obs
anemObsGpx <- anemObs %>% 
  select(anem_obs, lat, lon) %>% 
  group_by(anem_obs) %>% 
  summarise(mlat = mean(lat), 
    mlon = mean(lon))

# replace lat lons with means
anemObs <- anemObs %>% 
  select(-lat, -lon)

anemObs <- left_join(anemObs, anemObsGpx, by = "anem_obs") %>% 
  rename(lat = mlat, 
    lon = mlon)

# rejoin to all_anem
all_anem <- rbind(all_anem, anemObs)

# list out all of the EMPT anems
empt <- all_anem %>% 
  filter(anem_spp == "EMPT")

# make a list of present anems
present <- all_anem %>% 
  filter(era == "present") %>% 
  select(anem_id) %>% 
  distinct()

past <- all_anem %>% 
  filter(!anem_id %in% present$anem_id, 
    !anem_id %in% empt$anem_id) %>% 
  select(-anem_spp, -old_anem_id) %>% 
  distinct()
  
current <- all_anem %>% 
  filter(anem_id %in% present$anem_id, 
    !anem_id %in% empt$anem_id) %>% 
  select(-anem_spp, -old_anem_id, -era) %>% 
  distinct()

write.csv(past, file = "../Phils_GIS_R/data/Anems/past_anems.csv")
write.csv(current, file = "../Phils_GIS_R/data/Anems/current_anems.csv")
write.csv(empt, file = "../Phils_GIS_R/data/Anems/empty_anems.csv")
