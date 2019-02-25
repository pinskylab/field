# a script to calculate the rate of fish processing by site

# collect all years with dive type C

library(tidyverse)
library(stringr)
library(lubridate)
source("scripts/field_func.R")

leyte <- read_db("Leyte")

# pull a specific set of dives
dive <- leyte %>% 
  tbl("diveinfo") %>% 
  filter(dive_type == "C") %>% 
  select(dive_table_id, date, site, gps) %>% 
  collect()

# pull in the fish observations for those dives
anem <- leyte %>% 
  tbl("anemones") %>% 
  filter(dive_table_id %in% dive$dive_table_id) %>% 
  arrange(dive_table_id, obs_time) %>% 
  select(anem_table_id, dive_table_id, obs_time) %>% 
  collect()

anem <- left_join(anem, dive, by = "dive_table_id")
anem <- anem %>% 
  mutate(year = lubridate::year(date))

# calculate the time between observations for each dive ----
anem <- anem %>% 
  # mutate(obs_time <- lubridate::hms(obs_time)) %>% 
  arrange(dive_table_id, obs_time)

anem$pause <- as.numeric(0.000)
# calculate the time between observations
for(i in 2:nrow(anem)){
  time1 <- hms(anem$obs_time[i])
  time2 <- hms(anem$obs_time[i-1])
  anem$pause[i] <- interval(time2, time1, origin = lubridate::origin)
}

anem <- anem %>% 
  mutate(as.Date(obs_time)) %>% 
  arrange(obs_time) %>% 
  select(-obs_time)

# remove the first row of each dive: the time since last dive
dive_ids <- distinct(anem, dive_table_id)
for(i in 1:nrow(dive_ids)){
  # pull out only one dive_table_id at a time
  x <- anem %>% 
    filter(dive_table_id == dive_ids$dive_table_id[i]) 
  # remove those dives from the main table
  anem <- anti_join(anem, x, by = "dive_table_id")
  # remove the first row from x (should be the gap between dives)
  x <- x[2:nrow(x), ] 
  # rejoin to the main group
  anem <- rbind(anem, x)
}

anem <- anem %>% 
  mutate(pause = pause/60) %>% 
  select(dive_table_id, pause, site, year)

anem <- anem %>%  # convert to minutes
  filter(pause > 0, 
    pause < 70) 

rate <-  anem %>% 
  group_by(site) %>% 
  summarise(avg_time = mean(pause))

# calculate the number of fish observed in 2015_01 to estimate max expected fish catch

dive <- leyte %>% 
  tbl("diveinfo") %>% 
  filter(date > "2015-01-01" & date < "2015-03-01") %>% 
  select(site, date, dive_table_id) %>% 
  collect()

anem <- leyte %>% 
  tbl("anemones") %>% 
  filter(dive_table_id %in% dive$dive_table_id) %>% 
  select(dive_table_id, anem_table_id) %>% 
  collect()

anem <- left_join(anem, dive, by = "dive_table_id")
rm(dive)

fish <- leyte %>% 
  tbl("clownfish") %>% 
  filter(anem_table_id %in% anem$anem_table_id) %>% 
  collect()

fish <- left_join(fish, anem, by = "anem_table_id")
rm(anem)

# Fish per site -----------------------------------------------------------

sitetot <- fishobs %>% 
  filter(fish_spp == "APCL") %>% 
  group_by(site) %>% 
  summarise(observed = n())

# join rate to site expectation
rate <- left_join(rate, sitetot, by = "site")

rate <- rate %>% 
  mutate(minutes = avg_time * observed, 
    dives = ceiling(minutes/120)) # if a dive is 120 minutes long

write.csv(rate, file = "data/rate of fish catch.csv", row.names = F)



