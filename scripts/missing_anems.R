# find the anems that we have not visited this year
library(dplyr)

# get a list of anems from previous years
prev_anems <- read.csv(stringsAsFactors = F, file = "data/anemones.csv", na = "NULL") %>% 
  filter(!is.na(anem_id)) %>% 
  select(anem_id, anem_obs, anem_table_id, dive_table_id, obs_time) %>% 
  arrange(anem_id)

# assign sites to the anmes
sites <- read.csv(stringsAsFactors = F, file = "data/diveinfo.csv", na = "NULL") %>% 
  select(dive_table_id, site) %>% 
  filter(dive_table_id %in% prev_anems$dive_table_id)
prev_anems <- left_join(prev_anems, sites)

# get anems visited this year

# # if data is accessible in google sheets:
# library(googlesheets)
# # gs_auth(new_user = TRUE) # run this if having authorization problems
# mykey <- '1symhfmpQYH8k9dAvp8yV_j_wCTpIT8gO9No4s2OIQXo' # access the file
# entry <-gs_key(mykey)
# clown <-gs_read(entry, ws='clownfish')
# dive <- gs_read(entry, ws="diveinfo")
# 
# # save data in case network connection is lost
# clownfilename <- str_c("data/clown_", Sys.time(), ".Rdata", sep = "")
# divefilename <- str_c("data/dive_", Sys.time(), ".Rdata", sep = "")
# save(clown, file = clownfilename)
# save(dive, file = divefilename)

# load data from saved if network connection is lost
# THIS HAS TO BE MANUALLY UPDATED WITH MOST CURRENT VERSION OF SAVED FILE  - COULD WRITE CODE TO FIND AND LOAD THE MOST CURRENT VERSION ####
load(file = "data/clown_2018-03-18 20:18:57.Rdata")
load(file = "data/dive_2018-03-18 20:18:57.Rdata")

curr_anems <- clown %>% 
  filter(!is.na(anem_id)) %>% 
  select(dive_num, anem_id, obs_time)

# join site info
curr_sites <- dive %>% 
  filter(dive_num %in% curr_anems$dive_num) %>% 
  select(dive_num, site, date)

curr_anems <- left_join(curr_anems, curr_sites, by = "dive_num")

# only check sites we haven't visited
prev_anems <- prev_anems %>% 
  filter(site %in% curr_anems$site)

# which anems haven't we visited at those sites?
missing_anems <- prev_anems %>% 
  filter(!anem_id %in% curr_anems$anem_id)


