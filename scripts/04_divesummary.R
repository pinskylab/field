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
# load(file = "data/clown_2018-03-12 21:13:51.Rdata")
# load(file = "data/dive_2018-03-12 21:13:52.Rdata")

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

# make a table of observed APCL
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

# how many fish are captured (clipped + recaptures) - this will miss fish that were captured but escaped before getting a clip...
captured <- samp %>% 
  filter(recap == "Y" | !is.na(fin_id)) %>% # select fish that either already had a tag or were given a fin clip (only counts fish that were both Y and clipped once)
  group_by(dive_num) %>%
  summarise(captured = n())

divetot <- left_join(divetot, captured, by = "dive_num")


# how many fish were recaptures?
recap <- samp %>% 
  filter(recap == "Y") %>% 
  group_by(dive_num) %>% 
  summarise(recap = n())

divetot <- left_join(divetot, recap, by = "dive_num") 

# what about the (few) fish that are recaptures but also get a fin clip?
clipped_recaps <- samp %>%
  filter(recap == "Y" & !is.na(fin_id)) %>%
  group_by(dive_num) %>%
  summarise(clipped_recaps = n())
divetot <- left_join(divetot, clipped_recaps, by = "dive_num")



divetot <- divetot %>% 
  select(dive_num, site, observed, captured, fins, recap, clipped_recaps) %>% 
  distinct()

# # are any captured more than observed?
bad <- filter(divetot, captured > observed) # the one at visca is ok because it is ????
# # are there more fin clips than captured?
bad <- filter(divetot, fins > captured)
# # are there ore recaps than captured?
bad <- filter(divetot, recap > captured)

# Fish per site -----

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

# how many fish were captured (recaps + fish with fin clips) - this does miss fish that were caught but escaped before getting a fin clip
cap <- fishobs %>% 
  filter(recap == "Y" | !is.na(fin_id)) %>% #filter out recaps or fish with fin-clips (looks like it handles recap fish with a clip correctly by only counting them once)
  group_by(site) %>%
  summarise(captured = n())

sitetot <- left_join(sitetot, cap, by = "site")

# how many fish were recaptures?
recap <- tissue %>% 
  filter(recap == "Y") %>% 
  group_by(site) %>% 
  summarize(recap = n())

sitetot <- left_join(sitetot, recap, by = "site") 

# how many recap fish got clips? (b/c missing genetic data)
clipped_Rs <- fishobs %>%
  filter(recap == "Y" & !is.na(fin_id)) %>%
  group_by(site) %>%
  summarise(clipped_recaps = n())

sitetot <- left_join(sitetot, clipped_Rs, by = "site")

# which tags have been entered into the data more than once
rep_tag <- fish %>% 
  filter(!is.na(tag_id)) %>% # get only obs with tags
  mutate(tag_id = str_sub(tag_id, -6)) %>% # make sure all tag_ids are only 6 digits
  group_by(tag_id) %>% # pull same tags together
  summarise(count = n()) %>% # count the same tags
  filter(count > 1) # choose only those that were found more than once
# how many captures were repeats of a tag already caught this season
same_seas_recap <- fish %>% 
  mutate(tag_id = str_sub(tag_id, -6)) %>% # make sure all tag_ids are only 6 digits
  filter(tag_id %in% rep_tag$tag_id) # can't do recap == Y because the first capture of this year might also have been a recap
# add counts of scans
same_seas_recap <- left_join(same_seas_recap, rep_tag, by = "tag_id")
rm(rep_tag)
# add dive info
same_seas_recap <- left_join(same_seas_recap, dive, by = "dive_num")
# reduce columns & remove repeated info
same_seas_recap <- same_seas_recap %>% 
  select(tag_id, count, site) %>% 
  distinct()
# subtract 1 from count for the initial capture this year
same_seas_recap <- same_seas_recap %>% 
  mutate(count = count - 1) %>% 
  select(-tag_id) %>% 
  rename(same_season_recap = count)

sitetot <- left_join(sitetot, same_seas_recap, by = "site")

# how many dives have been done at each site?
dive_count <- dive %>% 
  select(dive_num, site) %>% 
  distinct() %>%  # remove muliple dive records for the same dive due to multipe gps units
  group_by(site) %>% 
  summarise(number_dives = n())
sitetot <- left_join(sitetot, dive_count, by = "site")

sitetot <- sitetot %>% 
  select(site, number_dives, observed, captured, finclip, recap, clipped_recaps, same_season_recap)

# are any captured more than observed?
bad <- filter(sitetot, captured > observed) # 2017: the one at visca is ok because it is "????"
# are there more fin clips than captured?
bad <- filter(sitetot, finclip > captured)
# are there ore recaps than captured?
bad <- filter(sitetot, recap > captured)

# do the divetot numbers match up with the sitetot? 
divetotsum <- divetot %>% #calculate site totals from divetot, can compare to sitetot
  group_by(site) %>% 
  summarise(site_observed = sum(observed), 
    site_captured = sum(captured, na.rm = TRUE), 
    site_fins = sum(fins, na.rm = TRUE), 
    site_recap = sum(recap, na.rm = TRUE), 
    site_clipped_recaps = sum(clipped_recaps, na.rm = TRUE))

# #are the observed fish per site the same fish recaptured more than once in a season? (will print message if so, if no message all is good) #doesn't quite work b/c doesn't compare NAs (in sitetot) to 0s (in divetotsum) correctly
# for (i in 1:length(divetotsum$site)) {
#   site_name <- divetotsum$site[i]
#   if (divetotsum$site_observed[i] != sitetot$observed[i]) {
#     print(site_name, "fish observed mismatch", sep = " ")
#   }
#   if (divetotsum$site_fins[i] != sitetot$finclip[i]) {
#     print(site_name, "fin clips mismatch", sep = " ")
#   }
#   if (divetotsum$site_recap[i] != sitetot$recap[i]) {
#     print(site_name, "recaps mismatch", sep = " ")
#   }
#   if (divetotsum$site_clipped_recaps[i] != sitetot$clipped_recaps[i]) {
#     print(site_name, "fin clips mismatch", sep = " ")
#   }
# }

View(sitetot)
# save(sitetot, file = "data/sitetot.Rdata")
View(divetot)
# save(divetot, file = "data/divetot.Rdata")

total_cap <- sitetot %>% 
  summarise(sum_obs = sum(observed), 
    sum_cap = sum(finclip) + sum(recap, na.rm = T), 
    sum_finclip = sum(finclip), sum_recap = sum(recap, na.rm = T))
