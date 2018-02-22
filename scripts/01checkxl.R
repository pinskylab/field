# ---------------------------------------------
#   Set up work space - load packages and data ####
# ---------------------------------------------
library(tidyverse)
library(stringr)
source("scripts/field_helpers.R")

# if data is accessible in google sheets:
library(googlesheets)
# gs_auth(new_user = TRUE) # run this if having authorization problems
mykey <- '1symhfmpQYH8k9dAvp8yV_j_wCTpIT8gO9No4s2OIQXo' # access the file
entry <-gs_key(mykey)
clown <-gs_read(entry, ws='clownfish')
dive <- gs_read(entry, ws="diveinfo")


# # if data is via csv
# dat <- ("~/Downloads/2018_clownfish_data_entry - clownfish.csv")

# include pit tag scanner output
# pitfile <- ("data/BioTerm.txt")
pitfile <- ("~/Downloads/BioTerm.txt" )

problem <- data.frame()

# if you haven't downloaded data from the db to be used in the field, do it now (Michelle did for 2018, get a copy from her if you need it)
# setup
# leyte <- read_db("Leyte")
# anem_db <- leyte %>% 
#   tbl("anemones") %>% 
#   select(anem_table_id, dive_table_id, anem_id) %>% 
#   filter(!is.na(anem_id), anem_id != "-9999") %>% 
#   collect()
# dive_db <- leyte %>% 
#   tbl("diveinfo") %>% 
#   select(dive_table_id, site) %>% 
#   collect()
# anem_db <- left_join(anem_db, dive_db, by = "dive_table_id")
# anem_site <- anem_db %>% 
#   select(anem_id, site) %>% 
#   distinct()
# save(anem_site, file="data/anem_site.Rdata")
# fish_db <- leyte %>%
#   tbl("clownfish") %>%
#   select(fish_table_id, anem_table_id, recap, tag_id) %>%
#   filter(!is.na(tag_id)) %>%
#   collect()
# save(fish_db, file = "data/fish_db.Rdata")


# ---------------------------------------------
#   adjust formatting ####
# ---------------------------------------------
names(dive) <- stringr::str_to_lower(names(dive))
dive <- filter(dive, !is.na(dive_num))
# no separate anemone sheet in 2018
# anem <- excl("anemones", anemcol)
# names(anem) <- stringr::str_to_lower(names(anem))
# anem <- filter(anem, !is.na(divenum))
names(clown) <- stringr::str_to_lower(names(clown))
clown <- filter(clown, !is.na(dive_num))

# # ---------------------------------------------
# #   check the diveinfo sheet for type-o's #### don't need to do this because using data validation in the spreadsheet
# # ---------------------------------------------
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
#   check the anemones sheet for type-o's #### don't need to do this because using data validation in the spreadsheet
# ---------------------------------------------

# # check anem species
# anems <- c("ENQD", "STME", "HECR", "HEMG", "STHD", "HEAR", "MADO", "HEMA", "STGI", "????", "EMPT")
# good <- filter(anem, anemspp %in% anems)
# bad <- anti_join(anem, good)
# bad <- filter(bad, !is.na(anemspp))
# if (nrow(bad) > 0){
#   bad$typeo <- "fix anem spp on anem table"
# }
# (problem <- rbind(problem, bad))
# rm(good, bad)
# 
# # check fish species
# fish <- c("APCL", "APOC", "APPE", "APSE", "APFR", "APPO", "APTH", "PRBI", "NA")
# good <- filter(anem, spp %in% fish)
# bad <- anti_join(anem, good) # wait for the next line before checking bad
# bad <- filter(bad, !is.na(spp))
# if (nrow(bad) > 0){
#   bad$typeo <- "fix fish spp on anem table"
# }
# (problem <- rbind(problem, bad))
# rm(good, bad)

# ---------------------------------------------
#   check the clownfish sheet for type-o's
# ---------------------------------------------

# # Check fish species #### don't need to do this because using data validation in the spreadsheet
# good <- filter(clown, spp %in% fish)
# bad <- anti_join(clown, good)
# bad <- filter(bad, !is.na(spp))
# if (nrow(bad) > 0){
#   bad$typeo <- "fix fish spp on fish table"
# }
# (problem <- rbind(problem, bad))
# 
# # check tail colors
# colors <- c("YE", "O", "YR", "YP", "Y", "W", "WR", "WP", "BW", "B")
# good <- filter(clown, color %in% colors)
# bad <- anti_join(clown, good)
# bad <- filter(bad, !is.na(color))
# if (nrow(bad) > 0){
#   bad$typeo <- "fix tail color on fish table"
# }
# (problem <- rbind(problem, bad))

# # Are there anems are on the clownfish sheet that are not on the anemone sheet? # don't need to do this because not an anem sheet in 2018
# clowndive <- clown$divenum
# clowndive <- unique(clowndive)
# 
# bad <- compare_dives(clowndive)
# 
# if (nrow(bad) > 0){
#   bad$typeo <- "anemid in fish table doesn't match anem data table"
# }
# problem <- rbind(problem, bad)
# rm(bad, good)

# Are there repeat ID numbers on the clownfish sheet? ####
dups <- clown %>% 
  select(contains("id"), -contains("anem_id"), -contains("tag_id")) %>% 
  filter(!is.na(fin_id)) %>% 
  group_by(fin_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)
if (nrow(dups) > 0){
  print("fin_id is repeated on datasheet")
}

# Are there missing ID numbers on the clownfish sheet? ####
fins <- clown %>%
  select(fin_id) %>% 
  filter(!is.na(fin_id))
  # id is missing? # should be integer(0), otherwise will show you the missing id#
x <- c(1:max(fins$fin_id))  # get the highest fin_id so far
y <- data.frame(x) %>% 
  rename(fin_id = x) # create a sequence of numbers 1-highest fin_id
z <- anti_join(y, fins) # show which numbers are missing, 

# should be 0 obs ####


  # are there missing anemone_id numbers on the clownfish sheet?  #### - begin with the starting anem number for this field season
  # gather the used ids
anem_ids <- clown %>% 
  select(anem_id) %>% 
  filter(!is.na(anem_id))
x <- c(2938:max(anem_ids$anem_id)) #2938 is the first anem_id for 2018
y <- data.frame(x) %>% 
  rename(anem_id = x)
z <- anti_join(y, anem_ids)
# are there anemones listed at a different site than they were in other years? ####

  # field use
  load("data/anem_site.Rdata")
  
  # compile a list of anemones with sites from this year
  anem <- clown %>% 
    select(dive_num, anem_id) %>% 
    filter(!is.na(anem_id), anem_id != "-9999") %>% 
    distinct()
  new_anem_site <- left_join(anem, dive, by = "dive_num") %>% 
    select(dive_num, anem_id, site)
  
  # compare the two tables # diff 
  diff <- anti_join(new_anem_site, anem_site)

  # should have 0 obs ####
  
  
# ---------------------------------------------
#   format pit scanner data
# ---------------------------------------------
pit <- from_scanner(pitfile) # should generate 4 parsing failures

# find only this year - format of date should be 18-01-01
pit <- filter(pit, substr(date, 1,2) == "17")

# get rid of test tags
pit <- filter(pit, substr(scan,1,3) != "989" & substr(scan,1,3) != "999")
pit <- arrange(pit, date, time)

# ---------------------------------------------
#   format tag ids on clownfish data sheet
# ---------------------------------------------
clown <- clown %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "985_", "985153000")) %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "986_", "986112100")) %>% 
  mutate(tag_id = stringr::str_replace(tag_id, "982_", "982000411"))

tag_ids <- clown %>% select(contains("tag")) %>% filter(!is.na(tag_id))

# ---------------------------------------------
#   compare scans to datasheets
# ---------------------------------------------

# What tags are in excel that were not scanned by the scanner (type-os) - should return 0 rows
anti_join(tag_ids, pit, by = c("tag_id" = "scan"))

# What tags are in the scanner that are not in excel (type-os) - should return 0 rows
anti_join(pit, tag_ids, by = c("scan" = "tag_id"))  

# view any problems that need to be taken care of
problem


# do we have any tags that are scanned as Y and only appear once - compare to db?
load("data/fish_db.Rdata")

# what are past tags?
db_tags <- fish_db %>% 
  select(tag_id) 

# which tags were marked as recaptures this field season
current_tags <- clown %>% 
  filter(recap == "Y") %>% 
  select(tag_id)

# are any of these tags not in the db already?
missing <- anti_join(current_tags, db_tags)

# should be 0 obs ####

