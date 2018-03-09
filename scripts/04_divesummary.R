# This is a script to determine for each dive the number of fish captured, sampled and recatpured and the same numbers at each site, also the number of dives per site.

# ---------------------------------------------
#   Set up work space - load packages and data
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

# save data in case network connection is lost
clownfilename <- str_c("data/clown_", Sys.time(), ".Rdata", sep = "")
divefilename <- str_c("data/dive_", Sys.time(), ".Rdata", sep = "")
save(clown, file = clownfilename)
save(dive, file = divefilename)

# # load data from saved if network connection is lost
# # THIS HAS TO BE MANUALLY UPDATED WITH MOST CURRENT VERSION OF SAVED FILE  - COULD WRITE CODE TO FIND AND LOAD THE MOST CURRENT VERSION ####
# load(file = "data/clown_2018-03-07 17:25:56.Rdata")
# load(file = "data/dive_2018-03-07 17:25:56.Rdata")

# fish observation info from anem survey
fish <- clown
names(fish) <- str_to_lower(names(fish))
fish <- filter(fish, !is.na(fish_spp)) %>% 
  distinct()

# get dive info
site <- dive
names(site) <- stringr::str_to_lower(names(site))
site <- site %>% 
  filter(!is.na(dive_num)) %>% 
  select(dive_num, site) %>% 
  distinct()

# fish processing info from clownfish survey
samp <- clown
names(samp) <- stringr::str_to_lower(names(samp))
samp <- filter(samp, !is.na(dive_num))

# make a table of observed fish
divetot <- fish %>%
  group_by(dive_num) %>% 
  filter(fish_spp == "APCL") %>% 
  summarise(observed = n())

# add the site to the table
divetot <- left_join(divetot, site, by = "dive_num")

# how many tissue samples were collected?  
tissue <- samp %>% 
  filter(!is.na(fin_id)) %>% 
  group_by(dive_num) %>% 
  summarize(fins=n())
divetot <- left_join(divetot, tissue, by = "dive_num")

# # how many fish were captured - doesn't work for 2018 because we guessed sizes of uncaptured fish
# cap <- samp %>% filter(!is.na(size)) %>% 
#     group_by(dive_num) %>% 
#     summarise(captured = n())
# divetot <- left_join(divetot, cap, by = "dive_num")

# how many fish were recaptures?
recap <- samp %>% 
  filter(recap == "Y") %>% 
  group_by(dive_num) %>% 
  summarise(recap = n())

divetot <- left_join(divetot, recap, by = "dive_num") 
divetot <- divetot %>% 
  select(dive_num, site, observed, captured, fins, recap) %>% 
  distinct()

# # are any captured more than observed?
# bad <- filter(divetot, captured > observed) # the one at visca is ok because it is ????
# # are there more fin clips than captured?
# bad <- filter(divetot, fins > captured)
# # are there ore recaps than captured?
# bad <- filter(divetot, recap > captured)

# Fish per site -----------------------------------------------------------

# fish observed by site
fishobs <- left_join(fish, site, by = "dive_num") %>% 
  distinct()
sitetot <- fishobs %>% 
  filter(fish_spp == "APCL") %>% 
  group_by(site) %>% 
  summarise(observed = n())

# how many tissue samples were collected?  
tissue <- left_join(samp, site, by = "dive_num") %>% 
  distinct()
fins <- tissue %>% 
  filter(!is.na(fin_id)) %>% 
  group_by(site) %>% 
  summarise(finclip = n())

sitetot <- left_join(sitetot, fins, by = "site")

# how many fish were captured
cap <- tissue %>% 
  filter(!is.na(size)) %>% 
  group_by(site) %>% 
  summarize(captured = n())

sitetot <- left_join(sitetot, cap, by = "site")

# how many fish were recaptures?
recap <- tissue %>% 
  filter(recap == "Y") %>% 
  group_by(site) %>% 
  summarize(recap = n())

sitetot <- left_join(sitetot, recap, by = "site") 
sitetot <- sitetot %>% 
  select(site, observed, captured, finclip, recap)

# are any captured more than observed?
bad <- filter(sitetot, captured > observed) # the one at visca is ok because it is ????
# are there more fin clips than captured?
bad <- filter(sitetot, finclip > captured)
# are there ore recaps than captured?
bad <- filter(sitetot, recap > captured)

View(sitetot)
# save(sitetot, file = "data/sitetot.Rdata")
View(divetot)
# save(divetot, file = "data/divetot.Rdata")
