# find info about anems from Allison's email that have anem_ids too low to be first used in 2018 but only listed in database from 2018

source("scripts/field_helpers.R")

odd_anems <- c(2467, 2892, 1219, 1225, 3008, 419, 1218, 1800, 1282, 3103, 3137, 1213, 1116, 3122, 1217, 2747)



leyte <- read_db("Leyte")

# pull in the anemone info
anem <- leyte %>% tbl("anemones") %>% 
  filter(anem_id %in% odd_anems) %>% 
  select(anem_table_id, dive_table_id, anem_id, old_anem_id, obs_time) %>% 
  collect()

# attach the dive info
dive <- leyte %>% tbl("diveinfo") %>% 
  filter(dive_table_id %in% anem$dive_table_id) %>% 
  select(date, site, dive_table_id, dive_num) %>% 
  collect()
anem <- left_join(anem, dive, by = "dive_table_id")
rm(dive)

# add additional anems to give context to when these tags were installed
anem <- anem %>% 
  mutate(plus1 = anem_id + 1, 
    min1 = anem_id - 1)

helper_anems <- anem %>% 
  select(plus1) %>% 
  rename(anem_id = plus1)

helper_anems2 <- anem %>% 
  select(min1) %>% 
  rename(anem_id = min1)

helper_anems <- rbind(helper_anems, helper_anems2)

add_anems <- leyte %>% tbl("anemones") %>% 
  filter(anem_id %in% helper_anems$anem_id) %>% 
  select(anem_table_id, dive_table_id, anem_id, old_anem_id, obs_time) %>% 
  collect()

# attach the dive info
dive <- leyte %>% tbl("diveinfo") %>% 
  filter(dive_table_id %in% add_anems$dive_table_id) %>% 
  select(date, site, dive_table_id, dive_num) %>% 
  collect()
add_anems <- left_join(add_anems, dive, by = "dive_table_id")
rm(dive)

anem <- anem %>% 
  select(-plus1, -min1)
anem <- rbind(anem, add_anems)
