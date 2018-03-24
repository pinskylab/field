# Figure out which fish have been tagged but not clipped 
# Run code, then look at need_clips_distinct to see which tags and sites need fin clips

#################### Set-up: ####################
# Load relevant libraries
library(dplyr)
library(lubridate)
library(readr)

#################### Functions: ####################
source("scripts/field_helpers.R")

#################### Running things: ####################
# # import data from database in the field (no network connection)
# clown <- read_csv(file = "data/db_backups/clownfish.csv")
# create rdata from db_backups 
# save(clown, file = "data/clownfish_db.Rdata")
load(file = "data/clownfish_db.Rdata")

# # import data from database in NJ
# leyte <- read_db("Leyte")
# clown <- leyte %>% 
#   tbl("clownfish") %>% 
#   collect()


# Consolidate database info ####
allfish_fish <- fish %>%
  select(fish_table_id, anem_table_id, fish_spp, sample_id, cap_id, anem_table_id, recap, tag_id, color, size) %>%
  collect() 

allfish_anems <- anems %>%
  select(anem_table_id, dive_table_id, anem_obs, anem_id, old_anem_id) %>%
  collect() %>%
  filter(anem_table_id %in% allfish_fish$anem_table_id)

allfish_dives <- dives %>%
  select(dive_table_id, dive_type, date, site, gps) %>%
  collect() %>%
  filter(dive_table_id %in% allfish_anems$dive_table_id)

# pull out just the year and put that in a separate column
allfish_dives$year <- as.integer(substring(allfish_dives$date,1,4))

#join together
allfish <- left_join(allfish_fish, allfish_anems, by="anem_table_id")
allfish <- left_join(allfish, allfish_dives, by="dive_table_id")

##### Different ways tagged fish could be missing a tag
# WAY 1: they have a tag_id, are a first-time capture (so NA or N in recap), but fail to get a clip
tagged <- allfish %>% filter(!is.na(tag_id)) #pull out all tagged fish
tagged_noClip <- tagged %>% filter(recap != "Y" | is.na(recap)) %>% filter(is.na(sample_id)) #filter out fish that aren't recaps and don't have a sample id

#show abbreviated version of it so can see sites - at Gabas, Magbangon, Visca, Wangag
tagged_noClip %>% select(sample_id, cap_id, recap, tag_id, size, color, anem_id, old_anem_id, dive_type, site, date)

#keep running tally of tag_id, anem_id, site
need_clips <- tagged_noClip %>% select(tag_id, anem_id, site)

# WAY 2: fish that have a Y for recapture but have only been scanned once (so were accidentally marked as a recap)
taggedY <- allfish %>% filter(recap == "Y") #pull out all recap = Y fish
recaps1Scan <- allfish %>% filter(tag_id %in% taggedY$tag_id) %>% group_by(tag_id) %>% summarize(nscans = n()) %>% filter(nscans == 1) #for fish with those tags, which ones were only scanned once?
recaps1ScanInfo <- allfish %>% filter(tag_id %in% recaps1Scan$tag_id) # pull out the info for those

#show abbreviated version of it so can see sites - one at Sitio Baybayon, one at Wangag
recaps1ScanInfo %>% select(sample_id, cap_id, recap, tag_id, size, color, anem_id, old_anem_id, dive_type, site, date)

# add to overall list
need_clips <- rbind(need_clips, (recaps1ScanInfo %>% select(tag_id, anem_id, site)))


# WAY 3: DNA sequencing failed
#list of failed samples from Michelle email 3/8/18
sampfails <- c('APCL12_271', 'APCL13_024', 'APCL13_547', 'APCL13_549', 'APCL13_551', 'APCL13_553',
               'APCL14_030', 'APCL14_157', 'APCL14_161', 'APCL14_164', 'APCL14_301', 'APCL14_304',
               'APCL14_305', 'APCL14_306', 'APCL14_492', 'APCL14_494', 'APCL15_355550', 'APCL15_404305',
               'APCL15_405807', 'APCL15_371025', 'APCL15_374500', 'APCL15_012', 'APCL15_374393', 
               'APCL15_371626', 'APCL15_402306', 'APCL15_376242', 'APCL15_015', 'APCL15_375267',
               'APCL15_406794', 'APCL15_370851')

samplefail <- allfish %>% filter(sample_id %in% sampfails & !is.na(tag_id)) #pull out the info for the samples for those that have tag_ids (not all do)

#add to list
need_clips <- rbind(need_clips, (samplefail %>% select(tag_id, anem_id, site)))

# WAY 4 - other sample fails Michelle had mentioned previously - are they already in the list?
tags_needclips <-c(985153000375914, 985153000404510, 986112100164794) #Palanas, Wangag, Magbangon
tags_SitioBaybayon <-c(985153000371791, 985153000373315, 985153000370707, 985153000373354, 985153000372065, 985153000373695, 
                       985153000373978, 985153000371016, 985153000375325, 985153000372250, 985153000373100, 985153000372731,
                       985153000375532, 985153000371164, 985153000374934) #umm, some of these don't appear to exist in allfish....only 6 do...


#others <- as.character(c(tags_needclips, tags_SitioBaybayon))
others <- data_frame(tag_id = c(tags_needclips, tags_SitioBaybayon))

need_clips %>% filter(tag_id %in% others$tag_id) # looks like only two are already covered in the existing list

tags_othersInfo <- allfish %>% filter(tag_id %in% others$tag_id) %>% distinct(tag_id, anem_id, site) #only pulls out 13 of the 18....

#add to growing list
need_clips <- rbind(need_clips, tags_othersInfo)

#just tag_ids and sites (since some fish seen at multiple anems)
need_clips_distinct <- need_clips %>% distinct(tag_id, site) %>% mutate(tag_id = as.character(tag_id)) #convert tag_id to character so shows up nicely and not in scientfic notation


# # ONES THAT MICHELLE HAD FROM LAST YEAR - DNA sequencing failed
# tags_needclips <-c(985153000375914, 985153000404510, 986112100164794) #Palanas, Wangag, Magbangon
# tags_SitioBaybayon <-c(985153000371791, 985153000373315, 985153000370707, 985153000373354, 985153000372065, 985153000373695, 
#                        985153000373978, 985153000371016, 985153000375325, 985153000372250, 985153000373100, 985153000372731,
#                        985153000375532, 985153000371164, 985153000374934)
# 
# 
# tags_needclipsInfo <- allfish %>% filter(tag_id %in% tags_needclips) %>% group_by(tag_id)
# tags_needclipsInfo %>% select(sample_id, cap_id, recap, tag_id, size, color, anem_id, old_anem_id, dive_type, site, date) %>% group_by(tag_id)
# 
# tags_SitioBaybayonInfo <- allfish %>% filter(tag_id %in% tags_SitioBaybayon) 
# tags_SitioBaybayonInfo %>% select(sample_id, cap_id, recap, tag_id, size, color, anem_id, old_anem_id, dive_type, site, date) 
