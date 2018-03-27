# connect current field anems to anem obs and map one dot for each anemone
source("scripts/field_helpers.R")

# get data ####

# if network
# get_from_google()

# if no network
get_data_no_net()
load(clown_filename)
load(dive_filename)

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

rm(all_ids, all_old_ids, field_anem, anem_db, clown, dive, have_obs, have_old, have_old_obs, lack_obs, many)

# # TEST make sure each anem_id is only at one site ####
# test <- all_anem %>% 
#   select(anem_id, dive_table_id, anem_table_id)
# 
# test <- anem_site(test)
# 
# test <- test %>%
#   distinct(anem_id, site) %>% 
#   group_by(anem_id) %>% 
#   summarize(num_sites = n()) %>% 
#   filter(anem_id != "-9999" & anem_id != "", num_sites > 1)
# 
# # repeat for anem_obs
# test <- anem %>% 
#   select(anem_id, anem_obs, dive_table_id, anem_table_id)
# 
# test <- anem_site(test)
# 
# test2 <- test %>%
#   filter(!is.na(anem_obs)) %>% 
#   distinct(anem_obs, site) %>% 
#   group_by(anem_obs) %>% 
#   summarize(num_sites = n()) %>% 
#   filter(num_sites > 1)
# 
# test3 <- test %>% # 19 obs come up with more than one site
#   filter(anem_obs %in% test2$anem_obs) %>% 
#   arrange(anem_obs)
# test4 <- left_join(test3, anem, by = c("anem_table_id", "dive_table_id", "anem_obs", "anem_id"))
# temp <- anem_date(test4)
# fix <- left_join(test4, temp, by = "dive_table_id") %>% 
#   select(anem_obs, anem_id, old_anem_id, site, date, everything())
# 
# 
# 
# # 
# leyte <- write_db("Leyte")
# dbWriteTable(leyte, "anemones", anem, row.names = F, overwrite = T)
# dbDisconnect(leyte)

#####################################################################
# I did this with the for loop above but this is also a good way.
# # find old_anem_ids that are not in the multi table as original anem_ids
# have_match <- multi %>% 
#   filter(old_anem_id %in% anem_id)
# 
# # remove all old_anem_ids that do not have a match in the id column and create an id column for them
# need_match <- anti_join(multi, have_match) %>% 
#   select(old_anem_id, anem_obs) %>% 
#   mutate(anem_id = old_anem_id)

# # rejoin to multi
# multi <- rbind(multi, need_match) %>% 
#   filter(anem_id != "-9999")

# # to incorporate this into the anem table, no longer need old_anem_id column because it is represented in the anem_id column and matched to an anem_obs
# multi <- multi %>% 
#   select(-old_anem_id) %>% 
#   distinct()
# 
# 
# # remove rows to be changed from original db
# old <- anti_join(old, anem)
# 
# # remove anem_obs from the anem table because nothing has ever been assigned an anem obs before, this won't work in the future - 
# anem <- select(anem, -anem_obs)
# 
# # join the info
# anem <- left_join(anem, multi, by = "anem_id")
# 
# # join to odb
# anem <- rbind(old, anem)
# 
# # # too many rows, which row numbers are duplicated?
# # prob <- anem %>% 
# #   group_by(anem_table_id) %>% 
# #   summarise(count = n()) %>% 
# #   filter(count > 1) 
# # 
# # test <- filter(anem, anem_table_id == 860)
# 
# 
# library(RMySQL)
# 
# # leyte <- dbConnect(MySQL(), "Leyte", default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
# # 
# # dbWriteTable(leyte, "anemones", anem, row.names = F, overwrite = T)
# # # 
# # # 
# # dbDisconnect(leyte)
# # rm(leyte)





# include untagged anems as anem_spp

