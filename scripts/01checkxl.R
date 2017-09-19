# ---------------------------------------------
#   Set up work space - load packages and data
# ---------------------------------------------
library(tidyverse)
library(stringr)
source("scripts/field_func.R")
excel_file <- ("data/GPSSurveys2017.xlsx")
pitfile <- ("data/BioTerm.txt" )
problem <- data.frame()

anemcol <- c("text", "text", "text", "text", "date", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text")

clowncol <- c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text")

# ---------------------------------------------
#   adjust formatting
# ---------------------------------------------
dive <- excl("diveinfo", NULL)
names(dive) <- stringr::str_to_lower(names(dive))
dive <- filter(dive, !is.na(divenum))
anem <- excl("anemones", anemcol)
names(anem) <- stringr::str_to_lower(names(anem))
anem <- filter(anem, !is.na(divenum))
clown <- excl("clownfish", clowncol)
names(clown) <- stringr::str_to_lower(names(clown))
clown <- filter(clown, !is.na(divenum))

# ---------------------------------------------
#   check the diveinfo sheet for type-o's
# ---------------------------------------------
sites <- c("Palanas", "Wangag", "Magbangon", "Cabatoan", "Caridad Cemetery", "Caridad Proper", "Hicgop", "Hicgop South", "Sitio Tugas", "Elementary School", "Sitio Lonas", "San Agustin", "Poroc San Flower", "Poroc Rose", "Visca", "Gabas", "Tamakin Dacot", "Haina", "Sitio Baybayon")
good <- filter(dive, site %in% sites)
bad <- anti_join(dive, good)
bad <- filter(bad, !is.na(divenum))
if (nrow(bad) > 0){
  bad$typeo <- "fix site spelling on diveinfo table"
}
(problem <- rbind(problem, bad))
rm(good, bad)

# ---------------------------------------------
#   check the anemones sheet for type-o's
# ---------------------------------------------

# check anem species
anems <- c("ENQD", "STME", "HECR", "HEMG", "STHD", "HEAR", "MADO", "HEMA", "STGI", "????", "EMPT")
good <- filter(anem, anemspp %in% anems)
bad <- anti_join(anem, good)
bad <- filter(bad, !is.na(anemspp))
if (nrow(bad) > 0){
  bad$typeo <- "fix anem spp on anem table"
}
(problem <- rbind(problem, bad))
rm(good, bad)

# check fish species
fish <- c("APCL", "APOC", "APPE", "APSE", "APFR", "APPO", "APTH", "PRBI", "NA")
good <- filter(anem, spp %in% fish)
bad <- anti_join(anem, good) # wait for the next line before checking bad
bad <- filter(bad, !is.na(spp))
if (nrow(bad) > 0){
  bad$typeo <- "fix fish spp on anem table"
}
(problem <- rbind(problem, bad))
rm(good, bad)

# ---------------------------------------------
#   check the clownfish sheet for type-o's
# ---------------------------------------------

# Check fish species
good <- filter(clown, spp %in% fish)
bad <- anti_join(clown, good)
bad <- filter(bad, !is.na(spp))
if (nrow(bad) > 0){
  bad$typeo <- "fix fish spp on fish table"
}
(problem <- rbind(problem, bad))

# check tail colors
colors <- c("YE", "O", "YR", "YP", "Y", "W", "WR", "WP", "BW", "B")
good <- filter(clown, color %in% colors)
bad <- anti_join(clown, good)
bad <- filter(bad, !is.na(color))
if (nrow(bad) > 0){
  bad$typeo <- "fix tail color on fish table"
}
(problem <- rbind(problem, bad))

# Which anems are on the clownfish sheet that are not on the anemone sheet?
clowndive <- clown$divenum
clowndive <- unique(clowndive)

bad <- compare_dives(clowndive)

if (nrow(bad) > 0){
  bad$typeo <- "anemid in fish table doesn't match anem data table"
}
problem <- rbind(problem, bad)
rm(bad, good)

# Are there repeat ID numbers on the clownfish sheet?
dups <- clown %>% 
  select(contains("id"), -contains("anemid"), -contains("tagid")) %>% 
  filter(!is.na(finid)) %>% 
  group_by(finid) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)
if (nrow(dups) > 0){
  print("fin_id is repeated on datasheet")
}

# Are there missing ID numbers on the clownfish sheet?
missing <- clown %>%
  select(contains("id"), -contains("anemid"), -contains("tagid")) %>% 
  filter(!is.na(finid))
  # which id is missing? # should be integer(0), otherwise will show you the missing id#
  rep(1:nrow(missing))[!(rep(1:nrow(missing)) %in%  unique(missing$finid))]

# ---------------------------------------------
#   format pit scanner data
# ---------------------------------------------
pit <- from_scanner(pitfile) # should generate 4 parsing failures

# find only this year
pit <- filter(pit, substr(date, 7,8) == "17" & substr(date, 1,2) != "16")

# get rid of test tags
pit <- filter(pit, substr(scan,1,3) != "989" & substr(scan,1,3) != "999")
pit <- arrange(pit, date, time)

# ---------------------------------------------
#   format tag ids on clownfish data sheet
# ---------------------------------------------
clown <- clown %>% 
  mutate(tagid = stringr::str_replace(tagid, "985_", "985153000")) %>% 
  mutate(tagid = stringr::str_replace(tagid, "986_", "986112100")) %>% 
  mutate(tagid = stringr::str_replace(tagid, "982_", "982000411"))

tagids <- clown %>% select(contains("tag")) %>% filter(!is.na(tagid))

# ---------------------------------------------
#   compare scans to datasheets
# ---------------------------------------------

# What tags are in excel that were not scanned by the scanner (type-os) - should return 0 rows
anti_join(tagids, pit, by = c("tagid" = "scan"))

# What tags are in the scanner that are not in excel (type-os) - should return 0 rows
anti_join(pit, tagids, by = c("scan" = "tagid"))  

# view any problems that need to be taken care of
problem
