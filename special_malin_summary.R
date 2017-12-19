# Malin wants summary statistics for a talk he's giving in the Indo-Pacific
source("scripts/helpers.R")
source("scripts/date_dive.R")
library(lubridate)

# connect to the database
leyte <- read_db("Leyte")

# Number of fish and anemones seen each year ####
# Number of fish genotyped each year
# Number of SNPs used for genotyping - 809
# Number of fish PIT tagged each year
# Number of genetic recaptures each year
# Number of PIT tag recaptures each year
# Number of parents and offspring analyzed with COLONY each year - Katrina will provide
# Number of parent-offspring, sibling, and half sib matches each year - Katrina will provide

# define the time periods
# year <- c("2012", "2013", "2014", "2015", "2016", "2017")

# define fish and anemone observation dives, included A because fish that were seen at anemones
fishes <- c("A", "C", "D", "E")

# # define anem observation dives
# anems <- c("A", "C", "D", "E")

# find dives that qualify 
dive <- leyte %>% 
  tbl("diveinfo") %>%
  select(date, dive_table_id, dive_type) %>% 
  filter(dive_type %in% fishes) %>% 
  collect()

# find anemones that were observed during qualifying dives
anem <- leyte %>% 
  tbl("anemones") %>% 
  select(anem_table_id, dive_table_id, anem_spp) %>% 
  filter(dive_table_id %in% dive$dive_table_id) %>% 
  collect() 


# get the fish data for these dives
fish <- leyte %>% 
  tbl("clownfish") %>% 
  select(fish_table_id, anem_table_id, fish_spp, size, cap_id, recap, tag_id, sample_id) %>% 
  filter(anem_table_id %in% anem$anem_table_id) %>% 
  collect()

# add dates to fish observations
fish <- left_join(fish, anem, by = "anem_table_id") 
fish <- left_join(fish, dive, by = "dive_table_id")

fish <- fish %>% 
  mutate(year = year(date))


num_fish <- fish %>%
  group_by(year) %>% 
  filter(!is.na(fish_spp)) %>% 
  summarise(
    num_fish = n()
  )
num_gen <- fish %>% 
  group_by(year) %>% 
  filter(!is.na(sample_id)) %>% 
  summarize(
    num_gen = n()
  )

num_tag <- fish %>% 
  group_by(year) %>% 
  filter(!is.na(tag_id)) %>%
  filter(recap == "N" | is.na(recap)) %>% 
  summarize(
    num_tag = n()
  )

num_cap <- fish %>% 
  group_by(year) %>% 
  filter(!is.na(cap_id)) %>% 
  select(year, cap_id) %>% 
  summarize(
    num_cap = n()
  )


# the above counts the times we caught a fish, including the first time, in order to remove the first time:
# 
# # create a list of unique cap_ids
# caps <- distinct(fish, cap_id) %>% 
#   filter(!is.na(cap_id))
# 
# # create a table for each cap_id and remove the cap_id from the first year caught.
# year <- "test"
# cap_id <- "test"
# new <- data.frame(year, cap_id)
# for (i in 1:nrow(caps)) {
#   x <- caps$cap_id[i]
#   
#   temp <- num_cap %>% 
#     filter(cap_id == x)
#   
#   temp <- temp %>% 
#     filter(year != min(temp$year))
#   
#   new <- rbind(new, temp) # this is convertng new to a list for some reason.
#   
#   # x <- num_cap[which(num_cap$cap_id == caps$cap_id[i]), ] # none of this leads to removing those rows from the count!!!
#   
#   
# }
  

num_pit <- fish %>% 
  group_by(year) %>% 
  filter(recap == "Y") %>% 
  summarise(
    num_pit = n()
  )


# 
# 
# # get number of anemones 
# anem <- leyte %>% 
#   tbl("anemones") %>% 
#   select(dive_table_id, anem_spp, anem_table_id) %>% 
#   filter(!is.na(anem_spp)) %>% 
#   collect()
# 
# # get the dive data
# dive <- leyte %>% 
#   tbl("diveinfo") %>% 
#   select(dive_table_id, date) %>% 
#   filter(dive_table_id %in% anem$dive_table_id) %>% 
#   collect()
# 

# add dates to anem table
anem <- left_join(anem, dive, by = "dive_table_id")

anem <- anem %>% 
  mutate(year = year(date))

num_anem <- anem %>%
  group_by(year) %>% 
  filter(!is.na(anem_spp)) %>% 
  summarise(
    num_anem = n()
  )

summary <- left_join(num_fish, num_anem)

summary <- left_join(summary, num_gen)

summary <- left_join(summary, num_cap)

summary <- left_join(summary, num_pit)

summary <- left_join(summary, num_tag)

