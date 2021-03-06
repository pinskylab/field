# ---------------------------------------------
#   Set up work space - load packages and data ####
# ---------------------------------------------
library(tidyverse)
library(stringr)
source("scripts/field_helpers.R")

# enter the number on the first anem id tag installed this year
first_tag <- 2938

# before running this script, PULL from github to make sure you have the most recent version of all of the data

# # to load from files saved on your device
clown <- read_csv("data/2018_clownfish.csv")
dive <- read_csv("data/2018_diveinfo.csv")

# include pit tag scanner output
pitfile <- "data/BioTerm.txt"
#download file from first pit tag scanner (in use through 21 March 2018)
oldpitfile <- "data/BioTerm_old.txt"
 
# download a copy of the sqlite database from https://github.com/pinskylab/leyteBuildDB/blob/master/leyte-db.sqlite and update the path to that file here:
leyte <- src_sqlite("~/Documents/leyteBuildDB/leyte-db.sqlite")

problem <- tibble()

# ---------------------------------------------
#   adjust formatting ####
# ---------------------------------------------
names(dive) <- stringr::str_to_lower(names(dive))
dive <- filter(dive, !is.na(dive_num))
names(clown) <- stringr::str_to_lower(names(clown))
clown <- filter(clown, !is.na(dive_num))

# ---------------------------------------------
#   check the diveinfo sheet for type-o's #### don't need to do this because using data validation in the spreadsheet
# ---------------------------------------------
# sites <- c("Palanas", "Wangag", "Magbangon", "Cabatoan", "Caridad Cemetery", "Caridad Proper", "Hicgop", "Hicgop South", "Sitio Tugas", "Elementary School", "Sitio Lonas", "San Agustin", "Poroc San Flower", "Poroc Rose", "Visca", "Gabas", "Tamakin Dacot", "Haina", "Sitio Baybayon")
# good <- filter(dive, site %in% sites)
# bad <- anti_join(dive, good)
# bad <- filter(bad, !is.na(divenum))
# if (nrow(bad) > 0){
#   bad$typeo <- "fix site spelling on diveinfo table"
# }
# (problem <- rbind(problem, bad))
# rm(good, bad)

# ---------------------------------------------
#   check the clownfish sheet for type-o's #### don't need to do this if using data validation
# ---------------------------------------------
# # check anem species
# anems <- c("ENQD", "STME", "HECR", "HEMG", "STHD", "HEAR", "MADO", "HEMA", "STGI", "????", "EMPT")
# good <- filter(clown, anem_spp %in% anems)
# bad <- anti_join(clown, good)
# bad <- bad %>% 
#   filter(!is.na(anem_spp), anem_spp != "")
# if (nrow(bad) > 0){
#   bad$typeo <- "fix anem spp on anem table"
# }
# (problem <- rbind(problem, bad))
# rm(good, bad)
# 
# # Check fish species #### don't need to do this because using data validation in the spreadsheet
# # check fish species
# fish <- c("APCL", "APOC", "APPE", "APSA", "APFR", "APPO", "APTH", "PRBI", "NA")
# good <- filter(clown, fish_spp %in% fish)
# bad <- anti_join(clown, good)
# bad <- filter(bad, !is.na(fish_spp), fish_spp != "")
# if (nrow(bad) > 0){
#   bad$typeo <- "fix fish spp on fish table"
# }
# (problem <- rbind(problem, bad))
# 
# # check tail colors
# colors <- c("YE", "O", "YR", "YP", "Y", "W", "WR", "WP", "BW", "B")
# good <- filter(clown, color %in% colors)
# bad <- anti_join(clown, good)
# bad <- filter(bad, !is.na(color), color != "")
# if (nrow(bad) > 0){
#   bad$typeo <- "fix tail color on fish table"
# }
# (problem <- rbind(problem, bad))

# are there anemone observations with ids that are missing data?
# incmplt_anem <- clown %>% 
#   filter(!is.na(anem_spp), anem_spp != "", # anemone spp is present
#     is.na(anem_dia) | is.na(anem_id) | is.na(depth)) # but is missing info

# make sure the info isn't somewhere else
# multi <- clown %>% 
#   filter(anem_id %in% incmplt_anem$anem_id) %>% 
#   group_by(anem_id) %>% 
#   summarise(num_obs = n()) %>% 
#   filter(num_obs > 1)



# clowndive <- clown$dive_num
# clowndive <- unique(clowndive)
# 
# bad <- compare_dives(clowndive)
# 
# if (nrow(bad) > 0){
#   bad$typeo <- "anemid in fish table doesn't match anem data table"
# }
# problem <- rbind(problem, bad)
# rm(bad, good)

# Are there repeat fin_ID numbers on the clownfish sheet? ####
bad <- clown %>% 
  select(contains("id"), -contains("anem_id"), -contains("tag_id")) %>% 
  filter(!is.na(fin_id)) %>% 
  group_by(fin_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)
if (nrow(bad) > 0){
  bad$typeo <- "fin_id is repeated on datasheet"
}
(problem <- rbind(problem, bad))

# Are there missing ID numbers on the clownfish sheet? ####
fins <- clown %>%
  select(fin_id) %>% 
  filter(!is.na(fin_id))
  # id is missing? # should be integer(0), otherwise will show you the missing id#
x <- c(1:max(fins$fin_id))  # get the highest fin_id so far
y <- data.frame(x) %>% 
  rename(fin_id = x) # create a sequence of numbers 1-highest fin_id
bad <- anti_join(y, fins) # show which numbers are missing, 
if (nrow(bad) > 0){
  bad$typeo <- "fin_id is missing"
}
(problem <- rbind(problem, bad))

# find samples that are lacking an anemone #### this needs to be added to problem - haven't tested -MRS
(lack <- clown %>%
    filter(is.na(anem_id), !is.na(fish_spp)) %>% 
    select(dive_num, obs_time, tag_id, fin_id, notes) %>% 
    filter(!is.na(fin_id) | !is.na(tag_id)))

# if this is zero rows,
rm(lack) # if it is not zero, look into what is going on on the data sheet

# are there missing anemone tag numbers on the clownfish sheet?  #### - begin with the starting anem number for this field season
  # get the used ids
anem_ids <- clown %>% 
  select(anem_id) %>% 
  filter(!is.na(anem_id), anem_id != "") %>%
  distinct()

x <- c(2938:max(anem_ids$anem_id)) #2938 is the first anem_id for 2018
y <- data.frame(x) %>% 
  rename(anem_id = x) %>% 
  filter(anem_id != 3101, anem_id != 3015, anem_id != 2947, anem_id !=3168, anem_id != 3223, anem_id != 3239) # these tags were probably dropped

bad <- anti_join(y, anem_ids)
if (nrow(bad) > 0){
  bad <- bad %>% 
    mutate(typeo = "anem_id is missing", 
      fin_id = NA)
 problem <- mutate(problem, anem_id = NA)
}
(problem <- rbind(problem, bad))

# are there anemones listed at a different site than they were in other years? Don't include new tags - in 2018 2938 was first tag ####

 # create a table of anem ids and sites
anem_db <- leyte %>% 
  tbl("anemones") %>%
  select(anem_table_id, dive_table_id, anem_id) %>%
  filter(!is.na(anem_id), anem_id != "-9999", anem_id != "NULL") %>%
  collect() 

dive_db <- leyte  %>% 
  tbl("diveinfo") %>% 
  select(dive_table_id, site) %>% 
  collect()

anem_site <- left_join(anem_db, dive_db, by = "dive_table_id") %>%
  select(anem_id, site) %>%
  distinct() %>% 
  rename(old_site = site) %>% 
  mutate(anem_id = as.integer(anem_id))

# compile a list of anemones with sites from this year
anem <- clown %>% 
    select(dive_num, anem_id) %>% 
    filter(!is.na(anem_id), anem_id != "-9999", anem_id < first_tag) %>% 
    distinct() %>% 
    mutate(anem_id = as.character(anem_id))
 
  
  new_anem_site <- left_join(anem, dive, by = "dive_num") %>% 
    select(dive_num, anem_id, site) %>% 
    mutate(anem_id = as.integer(anem_id)) %>% 
    distinct()
  
  # compare the two tables # diff 
  bad <- left_join(new_anem_site, anem_site, copy = T, by = "anem_id") %>% 
    filter(old_site != site | is.na(site)) #need to figure out how to get filter to keep nas, bad work-around at the moment
  
  # %>% 
    # filter(old_site != site)

  # should have 0 obs ####
  
  if (nrow(bad) > 0){
    bad$typeo <- "anem site does not match previous year"
  }
  (problem <- rbind(problem, bad))
  
# is an anemone observed more than once?
  obs_anem <- clown %>% 
    filter(!is.na(anem_spp)) %>% 
    group_by(anem_id) %>% 
    summarise(count = n()) %>% 
    filter(count > 1)
  
  # is data in a weird place?
  misplaced <- clown %>% 
    filter(is.na(anem_spp), 
      !is.na(depth) | !is.na(gps) |
        !is.na(anem_dia) | !is.na(egg_height)) %>% 
    filter(!is.na(anem_id) & anem_spp != "EMPT")
  
  
# is the same old_anem_id used for multiple anems?
  
double <- clown %>% 
  group_by(old_anem_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)
  
    
  # someday when we want to separate multiple fish on one untagged anem to make it not look like multiple anems, can check if the time is the same for multiple fish (or is time the same for identical anems) - this won't work well because it will be hard to tell if it is more than one anem or one anem recorded several times.
  
  
# ---------------------------------------------
#   format pit scanner data
# ---------------------------------------------
pit <- from_scanner(pitfile) # should generate 4 parsing failures #AD note - only generated 3 parsing errors... but still produces 3 columns "scan", "date", "time"
old_pit <- from_scanner(oldpitfile)

#When ran 3/25/18, old_pit has a \ added to the end of each time - remove it below
old_pit$time <- str_sub(old_pit$time, 1, str_length(old_pit$time)-1)

pit <- rbind(pit, old_pit) #join scans from current pit scanner and first one together
rm(old_pit)

# find only this year - format of date should be 18-01-01 #AD note - date is actually formatted 01/01/16
pit <- filter(pit, substr(date, 1,2) == "18")
#pit <- filter(pit, substr(date, 7,8) == "18" | substr(date, 1,2) == "18") #placement of year changes throughout! but looks like the 4 with 7,8 position are repeated with 1,2 too

# get rid of test tags
pit <- filter(pit, substr(scan,1,3) != "989" & substr(scan,1,3) != "999")
pit <- arrange(pit, date, time)

# ---------------------------------------------
#   format tag ids on clownfish data sheet
# ---------------------------------------------
clown <- clown %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "9851_", "985153000")) %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "9861_", "986112100")) %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "9820_", "982000411")) %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "9821_", "982126052")) %>% # fix 6 digit entries
  mutate(tag_id = stringr::str_replace(tag_id, "^95", "98212605295"), 
    tag_id = stringr::str_replace(tag_id, "^818", "982000411818"),
    tag_id = stringr::str_replace(tag_id, "^1", "9861121001"),
    tag_id = stringr::str_replace(tag_id, "^3", "9851530003"),
    tag_id = stringr::str_replace(tag_id, "^4", "9851530004"),
    tag_id = stringr::str_replace(tag_id, "^6", "9821260526"))
    
    

tag_ids <- clown %>% select(tag_id, recap) %>% filter(!is.na(tag_id))

# ---------------------------------------------
#   compare scans to datasheets
# ---------------------------------------------

# What tags are in the spreadsheet that were not scanned by the scanner (type-os) - should return 0 rows
spreadsheet <- anti_join(tag_ids, pit, by = c("tag_id" = "scan"))
clown %>% 
  filter(tag_id %in% spreadsheet$tag_id) %>% 
  select(dive_num, obs_time, tag_id)

# if there are rows in spreadsheet, view pit and compare


# What tags are in the scanner that are not in spreadsheet (type-os) - should return 0 rows
anti_join(pit, tag_ids, by = c("scan" = "tag_id"))  
# 818456 was scanned but the fish escaped before they could be tagged so as of 2018-03-14  it has not been used yet.

# make sure that pit tags with N only occur once on the data sheet
multi <- tag_ids %>% 
  filter(recap == "N") %>% 
  group_by(tag_id) %>% 
  summarise(tag_count = n()) %>% 
  filter(tag_count > 1)

# if multi is not zero rows, there is a type-o in one of the rows for each tag listed, double check these. (the recent anti_join(pit, tag_ids) should have also flagged this)

# view any problems that need to be taken care of
problem




# testing ####
# # an anemone came back as being listed in both Caridad and Hicgop South. This anemone is definitely in Hicgop, looking into database to see when it was ever recorded in Caridad.
# load("data/db_backups/anemones_db.Rdata")
# weird <- anem %>% 
#   filter(anem_id == 305)
# weird$corr_message
# 
# # the correction message says changed anem_id from 15 to NULL but the anem_id is not NULL, it is 305.  Very weird.  Let's get the info to go back to the data sheet - ok, changed anem_obs, not anem_id.  Fixed in db
# 
# load("data/db_backups/diveinfo_db.Rdata")
# dive <- dive %>% 
#   filter(dive_table_id %in% weird$dive_table_id)


# # do we have any tags that are scanned as Y and only appear once - compare to db? - # Doesn't seem to be working as of 3/15/18; pulls out 986112100172598 and 986112100172301, both of which were scanned in 2017 when connect directly to database and check...
# load("data/fish_db.Rdata")
# 
# # what are past tags?
# db_tags <- fish_db %>% 
#   select(tag_id) 
# 
# # which tags were marked as recaptures this field season
# current_tags <- clown %>% 
#   filter(recap == "Y") %>% 
#   select(tag_id)
# 
# # are any of these tags not in the db already?
# missing <- anti_join(current_tags, db_tags)
# 

# should be 0 obs ####

# fish anems - vs anem obs
