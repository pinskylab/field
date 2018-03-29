# connect current field anems to anem obs and map one dot for each anemone
library(stringr)
source("scripts/field_helpers.R")

# get data ####

# if network
get_from_google()

# if no network
# get_data_no_net()
# load(clown_filename)
# load(dive_filename)

# which field observations are anems?
field_anem <- clown %>% 
  filter(!is.na(anem_spp)) %>% 
  select(row_id, dive_num, obs_time, gps, anem_spp, old_anem_id, anem_id)

# remove the anems that are not tagged but save them for later
untagged <- field_anem %>% 
  filter(is.na(anem_id))
field_anem <- anti_join(field_anem, untagged) %>% 
  # change columns so that the tables can bind together
  rename(anem_table_id = row_id, 
    dive_table_id = dive_num) %>% 
  mutate(anem_obs = NA)

# bring in the past anems from the database
load("data/db_backups/anemones_db.Rdata")
anem_db <- anem %>% 
  select(anem_table_id, dive_table_id, obs_time, anem_spp, old_anem_id, anem_id, anem_obs) %>% 
  filter(anem_spp != "????")

# remove untagged anems
untagged_db <- anem_db %>% 
  filter(is.na(anem_id))
anem_db <- anti_join(anem_db, untagged_db) %>% 
  mutate(gps = NA)
rm(anem)

all_anem <- rbind(anem_db, field_anem) 

# a table of all anem_ids ever used
all_ids <- select(all_anem, anem_id) %>% 
  filter(!is.na(anem_id))
all_old_ids <- select(all_anem, old_anem_id) %>% 
  filter(!is.na(old_anem_id))

# which anemones already have an anem_obs number?
have_obs <- all_anem %>% 
  filter(!is.na(anem_obs)) %>% 
  select(anem_id, anem_obs) %>% 
  distinct() %>% 
  arrange(anem_id)

lack_obs <- all_anem %>% 
  filter(is.na(anem_obs)) %>% 
  select(-anem_obs)
new_obs <- left_join(lack_obs, have_obs, by = "anem_id") %>% 
  filter(!is.na(anem_obs))

# update these rows in all_anem
all_anem <- anti_join(all_anem, new_obs, by = "anem_table_id")
all_anem <- rbind(all_anem, new_obs)

# remove these from lack_obs
lack_obs <- anti_join(lack_obs, new_obs)

# which of these lack_obs have an old_id?
have_old <- lack_obs %>% 
  filter(!is.na(old_anem_id))
# do any of those have an anem_obs?
have_old_obs <- left_join(have_old, have_obs, by = c("old_anem_id" = "anem_id")) %>% 
  filter(!is.na(anem_obs))

# update these rows in all_anem
all_anem <- anti_join(all_anem, have_old_obs, by = "anem_table_id")
all_anem <- rbind(all_anem, have_old_obs)

# remove these from lack_obs
lack_obs <- anti_join(lack_obs, have_old_obs)

# now everything in lack_obs should have never had an anem_obs associated with it, however, some of these have only been seen once.
once <- lack_obs %>% 
  group_by(anem_id) %>% 
  summarise(count = n()) %>% 
  filter(count < 2)
# only keep the ones that were not listed as old_anem_ids - went from 1117 to 1081
once <- once %>% 
  filter(!anem_id %in% all_anem$old_anem_id)

# remove the onces from lack_obs
lack_obs <- lack_obs %>% 
  filter(!anem_id %in% once$anem_id)

# find all of the anemones that have a value in the old_anem_id column but lack an anem_obs
have_old <- lack_obs %>% # for all anemones with an id
  filter(!is.na(old_anem_id)) %>% 
  select(anem_id, old_anem_id) %>% 
  mutate(anem_obs = NA)

# find anem_ids that were visited more than once
multi_visit <- lack_obs %>% # for all anemones with an id
  group_by(anem_id) %>% # pool anem_id
  summarise(count = n()) %>%  # count the number of times that anem_id occurs
  filter(count > 1)  # select only those that occur more than once
multi_visit <- lack_obs %>% 
  filter(anem_id %in% multi_visit$anem_id) %>% 
  select(anem_id, old_anem_id) %>% 
  mutate(anem_obs = NA)

# combine the have_old anems with the multi_visit anems
multi_visit <- rbind(have_old, multi_visit) %>% 
  arrange(anem_id) %>% 
  distinct()

many <- multi_visit %>% 
  group_by(anem_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)

somany <- multi_visit %>% 
  filter(anem_id %in% many$anem_id, # the rows of anemones visited more than once
    is.na(old_anem_id)) # without old_anem_id info

# because somany are repeats that contain less info (no old_anem_id), remove them from the pack
multi_visit <- anti_join(multi_visit, somany)
  
# find the next obs number
n <- max(all_anem$anem_obs, na.rm = T)
if (n == -Inf){
  n <- 0
}
# multi_visit should be a table where each row represents one anem_id, assign each row an anem_obs
multi_visit$anem_obs <- (n+1):(n+nrow(multi_visit))

# create a new table with all of the rows from lack_obs that contain anem_ids from multi_visit and add anem_obs - afterwards, should find multiple observations of the same anem have the same anem_obs
new <- lack_obs %>% 
  filter(anem_id %in% multi_visit$anem_id) %>% 
  mutate(anem_obs = NA)
for (i in 1:nrow(new)){
  x <- multi_visit %>% 
    filter(anem_id == new$anem_id[i])
  new$anem_obs[i] <- x$anem_obs[1]
}

all_anem <- anti_join(all_anem, new, by = "anem_id")
all_anem <- rbind(all_anem, new)

# go back and assign the same anem_obs to the original observation of the old_anem_id
olds <- lack_obs %>% 
  filter(anem_id %in% multi_visit$old_anem_id)
for (i in 1:nrow(olds)){
  x <- multi_visit %>% 
    filter(old_anem_id == olds$anem_id[i])
  olds$anem_obs[i] <- x$anem_obs[1]
}

all_anem <- anti_join(all_anem, olds, by = "anem_id")
all_anem <- rbind(all_anem, olds)

rm(all_ids, all_old_ids, field_anem, anem_db, clown, dive, have_obs, have_old, have_old_obs, lack_obs, many, multi_visit, new, new_obs, olds, once, somany, x)

##############################################################################
# assign gps locations to these anems to map each anem only once ####

# split out the anems into past and present field seasons, and visited and not visited - need to grab db gpx and also current gpx files

# reduce all_anem to anem_id and anem_obs
anemobs <- all_anem %>% 
  select(anem_id, anem_obs) %>% 
  distinct()

# get past anems
load("data/db_backups/anemones_db.Rdata")
load("data/db_backups/diveinfo_db.Rdata")
load("data/db_backups/gpx_db.Rdata")

past_anem <- anem %>% 
  filter(!is.na(anem_id)) %>% 
  select(anem_id, old_anem_id, anem_obs, obs_time, dive_table_id, anem_table_id)
rm(anem)
past_dive <- dive %>%
  filter(dive_table_id %in% past_anem$dive_table_id) %>% 
  select(dive_table_id, date, gps)
rm(dive)
past_anem_dive <- left_join(past_anem, past_dive, by = "dive_table_id") %>% 
  mutate(obs_time = force_tz(ymd_hms(str_c(date, obs_time, sep = " ")),  tzone = "Asia/Manila"))
rm(past_anem, past_dive)

#### NEED TO CREATE AN assign_gpx_db SCRIPT ####

# convert time zone to UTC
past_anem_dive <- past_anem_dive %>% 
  mutate(obs_time = with_tz(obs_time, tzone = "UTC"), 
    year = year(obs_time), 
    month = month(obs_time), 
    day = day(obs_time), 
    hour = hour(obs_time), 
    min = minute(obs_time)
    )

past_gpx <- gpx %>% 
  mutate(year = year(time), 
    month = month(time), 
    day = day(time), 
    hour = hour(time), 
    min = minute(time)
    ) %>% 
  group_by(year, month, day, hour, min, unit) %>% 
  summarise(lat = mean(lat), 
    lon = mean(lon)) %>% 
  rename(gps = unit)

past_anem_dive_gpx <- left_join(past_anem_dive, past_gpx, by = c("gps", "year", "month", "day", "hour", "min")) %>% 
  select(-anem_obs)

# make sure all anem_ids have updated anem_obs
past_for_qgis <- left_join(past_anem_dive_gpx, anemobs, by = "anem_id") %>% 
  select(anem_id, anem_obs, anem_table_id, lat, lon) %>% 
  mutate(note = as.character(anem_id))



# separate the anems that have been observed more than once
has_obs <- past_for_qgis %>% 
  filter(!is.na(anem_obs)) %>% 
  distinct() 

obs <- has_obs %>% 
  select(anem_obs) %>% 
  distinct()

for (i in 1:nrow(obs)) {
  x <- has_obs %>% 
    filter(anem_obs == obs$anem_obs[i]) %>% 
    select(anem_id) %>% 
    distinct()
  has_obs <- has_obs %>% 
    mutate(note = ifelse(anem_obs == obs$anem_obs[i], as.character(list(x$anem_id)), note))
}

# update past_for_qgis
past_for_qgis <- anti_join(past_for_qgis, has_obs, by = "anem_table_id")

past_for_qgis <- rbind(past_for_qgis, has_obs) %>% 
  distinct()

# find a mean lat lon for each anem
means <- past_for_qgis %>% 
  group_by(note) %>% 
  summarise(lat = mean(lat), 
    lon = mean(lon))

past_for_qgis <- past_for_qgis %>% 
  select(-anem_table_id, -lat, -lon)

past_for_qgis <- left_join(past_for_qgis, means, by = "note")

##############################################

# get current anems ####

# if network
# get_from_google()

# # if no network
get_data_no_net()
load(clown_filename)
load(dive_filename)

current_anem <- clown %>% 
  filter(!is.na(obs_time),  obs_time != "NA", !is.na(anem_spp), !is.na(anem_id)) %>% 
  select(anem_id, old_anem_id, obs_time, dive_num, gps) %>% 
  mutate(gps = ifelse(is.na(gps), 4, gps))
rm(clown)

curr_dive <- dive %>%
  filter(dive_num %in% current_anem$dive_num) %>% 
  select(dive_num, date, gps)
rm(dive)

curr_anem_dive <- left_join(current_anem, curr_dive, by = c("dive_num", "gps")) %>% 
  mutate(obs_time = force_tz(ymd_hms(str_c(date, obs_time, sep = " ")),  tzone = "Asia/Manila"))
curr_anem_dive <- curr_anem_dive %>% 
    mutate(id = 1:nrow(curr_anem_dive), 
    gps = as.character(gps))
rm(current_anem, curr_dive)

# convert time zone to UTC
curr_anem_dive <- curr_anem_dive %>% 
  mutate(obs_time = with_tz(obs_time, tzone = "UTC"))


curr_anem_dive_gpx <- assign_gpx_field(curr_anem_dive)

curr_for_qgis <- curr_anem_dive_gpx %>% 
  distinct() 

# make sure all anems have updated anem_obs
curr_for_qgis <- left_join(curr_for_qgis, anemobs, by = "anem_id") %>% 
  select(anem_id, anem_obs, id, lat, lon) %>% 
  mutate(note = as.character(anem_id))

# separate the anems that have been observed more than once
has_obs <- curr_for_qgis %>% 
  filter(!is.na(anem_obs)) %>% 
  distinct() 

obs <- has_obs %>% 
  select(anem_obs) %>% 
  distinct()

for (i in 1:nrow(obs)) {
  x <- has_obs %>% 
    filter(anem_obs == obs$anem_obs[i]) %>% 
    select(anem_id) %>% 
    distinct()
  has_obs <- has_obs %>% 
    mutate(note = ifelse(anem_obs == obs$anem_obs[i], as.character(list(x$anem_id)), note))
}

# update past_for_qgis
curr_for_qgis <- anti_join(curr_for_qgis, has_obs, by = "id")

curr_for_qgis <- rbind(curr_for_qgis, has_obs) %>% 
  distinct()

# find a mean lat lon for each anem
means <- curr_for_qgis %>% 
  group_by(note) %>% 
  summarise(lat = mean(lat), 
    lon = mean(lon))

curr_for_qgis <- curr_for_qgis %>% 
  select(-id, -lat, -lon)

curr_for_qgis <- left_join(curr_for_qgis, means, by = "note") %>% 
  mutate(era = "current")

# remove anems seen currently from the past
past_for_qgis <- anti_join(past_for_qgis, curr_for_qgis, by = "anem_id")

# repeat with anem obs separately to get all of the repeats
past_for_qgis <- anti_join (past_for_qgis, curr_for_qgis, by = "anem_obs") %>% 
  mutate(era = "past")

# make one big table
for_qgis <- rbind(curr_for_qgis, past_for_qgis)

# mark out all of the EMPT anems
empt <- all_anem %>% 
  select(anem_spp, anem_id, anem_obs) %>% 
  filter(anem_spp == "EMPT")

for_qgis <- for_qgis %>% 
  mutate(era = ifelse(anem_id %in% empt$anem_id | anem_obs %in% empt$anem_obs, "EMPT", era)) %>% 
  select(note, lat, lon, era) %>% 
  distinct()

  
write.csv(filter(for_qgis, era == "past"), file = "../Phils_GIS_R/data/Anems/past_anems.csv")
write.csv(filter(for_qgis, era == "current"), file = "../Phils_GIS_R/data/Anems/current_anems.csv")
write.csv(filter(for_qgis, era == "EMPT"), file = "../Phils_GIS_R/data/Anems/empty_anems.csv")


