# create maps to take underwater to find anemones
#
source("scripts/field_helpers.R")

leyte <- read_db("Leyte")

anem <- leyte %>% 
  tbl("anemones") %>% 
  select(anem_table_id, dive_table_id, anem_id, anem_obs, obs_time) %>% 
  filter(!is.na(anem_id) & anem_id != "-9999") %>% 
  collect()

sites <- anem_site(anem) %>% 
  select(anem_id, anem_obs, site)

date <- anem_date(anem$anem_table_id)
anem <- left_join(anem, date, by = c("anem_table_id", "dive_table_id"))
rm(date)

lats <- anem_latlong(anem)
lats <- select(lats, anem_id, site, lat, lon) 
lats <- distinct(lats)

# this is a table of each anem_id and it's lat long, write to csv to import into QGIS
sum_lat <- lats %>%
  group_by(anem_id) %>% 
  summarise(lat = mean(lat),
    lon = mean(lon))

# write.csv(sum_lat, file = "data/each_anem_once.csv", row.names = F)

