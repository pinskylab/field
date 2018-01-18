# create maps to take underwater to find anemones

source("scripts/field_helpers.R")

leyte <- read_db("Leyte")

anem <- leyte %>% 
  tbl("anemones") %>% 
  select(anem_table_id, dive_table_id, anem_id, anem_obs, obs_time) %>% 
  filter(!is.na(anem_id) & anem_id != "-9999") %>% 
  collect()

date <- anem_date(anem$anem_table_id)
anem <- left_join(anem, date, by = c("anem_table_id", "dive_table_id"))
rm(date)








