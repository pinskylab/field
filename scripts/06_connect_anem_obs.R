# connect current field anems to anem obs and map one dot for each anemone
source("scripts/field_helpers.R")

# get data ####

# if network
get_from_google()

# # if no network
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
field_anem <- anti_join(field_anem, untagged)

# bring in the past anems from the database
load("data/db_backups/anemones_db.Rdata")
anem_db <- anem %>% 
  select(anem_table_id, dive_table_id, obs_time, anem_spp, old_anem_id, anem_id, anem_obs) %>% 
  filter(anem_spp != "????")

# remove untagged anems
untagged_db <- anem_db %>% 
  filter(is.na(anem_id))
anem_db <- anti_join(anem_db, untagged_db)
rm(anem)









# include untagged anems as anem_spp

