# Figure out which fish have been tagged but not clipped 
# Run code, then look at need_clips table to see which tags and sites need fin clips

#################### Set-up: ####################
# Load relevant libraries
library(dplyr)
library(lubridate)


#################### Functions: ####################
source("scripts/field_helpers.R")

#################### Running things: ####################
# import data from database in the field (no network connection) ####
# clown <- read.csv(file = "data/db_backups/clownfish.csv", na = "NULL", stringsAsFactors = F)
# anem <- read.csv(file = "data/db_backups/anemones.csv", na = "NULL", stringsAsFactors = F)
# dive <- read.csv(file = "data/db_backups/diveinfo.csv", na = "NULL", stringsAsFactors = F)
# pit <- read.csv(file = "data/db_backups/pitscan.csv", na = "NULL", stringsAsFactors = F)
# # create rdata from db_backups
# save(clown, file = "data/clownfish_db.Rdata")
# save(anem, file = "data/anemones_db.Rdata")
# save(dive, file = "data/diveinfo_db.Rdata")
# save(pit, file = "data/pitscan_db.Rdata")
load(file = "data/clownfish_db.Rdata")
load(file = "data/anemones_db.Rdata")
load(file = "data/diveinfo_db.Rdata")

# # import data from database in NJ ####
# leyte <- read_db("Leyte")

# so far have only used tag_id, fin_id, recap, fish_table_id, anem_table_id
# clown <- leyte %>% 
# #   tbl("clownfish") %>% 
# #   collect()

# anem_table_id, dive_table_id
# anem <- leyte %>% 
#   tbl("anemones") %>% 
#   collect()
# dive_table_id, site
# dive <- leyte %>% 
#   tbl("diveinfo") %>% 
#   collect()


# # Consolidate database info ####
allfish <- clown %>% 
  select(fish_table_id, anem_table_id, sample_id, tag_id, recap) %>% 
  mutate(tag_id = as.character(tag_id))
rm(clown)

anem <- anem %>% 
  select(anem_table_id, dive_table_id) %>% 
  filter(anem_table_id %in% allfish$anem_table_id)
allfish <- left_join(allfish, anem, by = "anem_table_id")
rm(anem)

dive <- dive %>% 
  select(dive_table_id, site) %>% 
  filter(dive_table_id %in% allfish$dive_table_id)
allfish <- left_join(allfish, dive, by = "dive_table_id")
rm(dive)

##### Different ways tagged fish could be missing a tag ####

# WAY 1: they have a tag_id, are a first-time capture (so NA or N in recap), but fail to get a clip
tagged <- allfish %>% filter(!is.na(tag_id)) # all fish that have a tag_id
tagged_noClip <- tagged %>% 
  #filter out fish that aren't recaps and lack fin id
  filter(is.na(sample_id), 
    is.na(recap) | # recap is NA
    recap != "Y") %>%   # or recap is Y
  # view the sites for these fish
  select(tag_id, site)

#keep running tally of tag_id, anem_id, site
need_clips <- tagged_noClip
rm(tagged_noClip, tagged)

# WAY 2: fish that have a Y for recapture but have only been scanned once (so were accidentally marked as a recap)
#pull out all recap = Y fish
taggedY <- allfish %>% 
  filter(recap == "Y") 

# look at all of the fish and determine how many were scanned only once
recaps1Scan <- allfish %>%
  filter(tag_id %in% taggedY$tag_id) %>%
  group_by(tag_id) %>%
  summarize(nscans = n()) %>%
  filter(nscans == 1) 

# pull out the info for those
recaps1ScanInfo <- allfish %>%
  filter(tag_id %in% recaps1Scan$tag_id) %>% 
  select(tag_id, site)

# double check these scans
load("data/pitscan_db.Rdata")
# 818477 is only in pit scan db once, checking data sheet: this fish had a tag when it was scanned. - Needs a finclip
# 373100 was part of Patrick's pilot study and needs a finclip
# 373978 was also part of Patrick's pilot study and needs a finclip

# add to overall list
need_clips <- rbind(need_clips, 
  (recaps1ScanInfo %>% 
      select(tag_id, site)))
rm(taggedY, recaps1Scan, recaps1ScanInfo)


# WAY 3: DNA sequencing failed
#list of failed samples from Michelle email 3/8/18
sampfails <- c('APCL12_271', 'APCL13_024', 'APCL13_547', 'APCL13_549', 'APCL13_551', 'APCL13_553', 'APCL14_030', 'APCL14_157', 'APCL14_161', 'APCL14_164', 'APCL14_301', 'APCL14_304', 'APCL14_305', 'APCL14_306', 'APCL14_492', 'APCL14_494', 'APCL15_355550', 'APCL15_404305', 'APCL15_405807', 'APCL15_371025', 'APCL15_374500', 'APCL15_012', 'APCL15_374393', 'APCL15_371626', 'APCL15_402306', 'APCL15_376242', 'APCL15_015', 'APCL15_375267', 'APCL15_406794', 'APCL15_370851')

samplefail <- allfish %>% filter(sample_id %in% sampfails & !is.na(tag_id)) #pull out the info for the samples for those that have tag_ids (not all do)

#add to list
need_clips <- rbind(need_clips, (samplefail %>% select(tag_id, site)))
rm(samplefails, samplefail)

# WAY 4 - other sample fails Michelle had mentioned previously - are they already in the list?
tags_needclips <-c(985153000375914, 985153000404510, 986112100164794) #Palanas, Wangag, Magbangon
tags_SitioBaybayon <-c(985153000371791, 985153000373315, 985153000370707, 985153000373354, 985153000372065, 985153000373695,  985153000373978, 985153000371016, 985153000375325, 985153000372250, 985153000373100, 985153000372731, 985153000375532, 985153000371164, 985153000374934) #umm, some of these don't appear to exist in allfish....only 6 do...they do exist as scans in the pitscanner, meaning a tag was scanned at some point.  I think these fish were Patrick's fish and not entered into the db because no samples were taken.


#others <- as.character(c(tags_needclips, tags_SitioBaybayon))
others <- data_frame(tag_id = c(tags_needclips, tags_SitioBaybayon))

need_clips %>% filter(tag_id %in% others$tag_id) # looks like 10 are already covered in the existing list

tags_othersInfo <- allfish %>% filter(tag_id %in% others$tag_id) %>% distinct(tag_id, site) #only pulls out 13 of the 18....

#add to growing list
need_clips <- rbind(need_clips, tags_othersInfo) %>% 
  distinct()

rm(tags_othersInfo, others)


