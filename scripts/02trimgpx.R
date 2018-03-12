# ---------------------------------------------
#   Set up work space - load packages and data
# ---------------------------------------------
library(tidyverse) # for pipe functions
library(lubridate) # for changing time zones

library(stringr) # for combining dates and times into dttm
source("scripts/readGPXGarmin.R")
source("scripts/writeGPXGarmin.R")
source("scripts/field_helpers.R")
# excel_file <- ("data/gpsSurveys2017.xlsx")


# ---------------------------------------------
#   import and format diveinfo
# ---------------------------------------------
# surv <- excl("diveinfo", NULL)

# if data is accessible in google sheets:
library(googlesheets)
# gs_auth(new_user = TRUE) # run this if having authorization problems
mykey <- '1symhfmpQYH8k9dAvp8yV_j_wCTpIT8gO9No4s2OIQXo' # access the file
entry <-gs_key(mykey)
clown <-gs_read(entry, ws='clownfish')
dive <- gs_read(entry, ws="diveinfo")

# # load data from saved if network connection is lost
# # THIS HAS TO BE MANUALLY UPDATED WITH MOST CURRENT VERSION OF SAVED FILE  - COULD WRITE CODE TO FIND AND LOAD THE MOST CURRENT VERSION ####
# load(file = "data/clown_2018-03-12 21:13:51.Rdata")
# load(file = "data/dive_2018-03-12 21:13:52.Rdata")

surv <- dive

# # if the data is in csv - the csvs aren't working with this code - need to convert characters to datte time??
# surv <- read.csv(file = "data/2018_clownfish_data_entry - diveinfo.csv", stringsAsFactors = F)
# surv <- surv %>% 
#   mutate(date = ymd(date), 
#     start_time = hms(start_time))
# 
# clown <- read.csv(file = "data/2018_clownfish_data_entry - clownfish-3.csv", stringsAsFactors = F)

names(surv) <- stringr::str_to_lower(names(surv))

# Combine date and time to form a dttm column and set the time zone to PHT, Asia/Manila 
surv <- surv %>% 
  select(dive_num, date, start_time, end_time, pause_start, pause_end, gps) %>% 
  mutate(start = force_tz(ymd_hms(str_c(date, start_time, sep = " ")),tzone = "Asia/Manila"), 
    end = force_tz(ymd_hms(str_c(date, end_time, sep = " ")), tzone = "Asia/Manila"), 
    paust = force_tz(ymd_hms(str_c(date, pause_start, sep = " ")), tzone = "Asia/Manila"), 
    pausend = force_tz(ymd_hms(str_c(date, pause_end, sep = " ")), tzone = "Asia/Manila")
    )

# there is a failed to parse message for the pauses that are NA but it doesn't seem to change the output in a negative way so can be ignored. ####
    
# Change time zone to UTC ####
surv <- surv %>% 
  mutate(start = with_tz(start, tzone = "UTC"), 
    end = with_tz(end, tzone = "UTC"), 
    paust = with_tz(paust, tzone = "UTC"),
    pausend = with_tz(pausend, tzone = "UTC"))

# This change to UTC should also change the date if necessary ####

# define the list of gps units
gps <- name_gps()

# determine which gps units have data & remove empties
gps <- used_gps(gps)

for (l in 1:length(gps)){
  files <- list.files(path = paste("data/",gps[l], sep = ""), pattern = "*Track*")
  for(i in 1:length(files)){ # for each file
    
    # debugonce(prep_gpx)
    dat <- prep_gpx(gps, files) # parse the gpx into useable data
    
    # which survey started after the gpx and ended before the gpx
    inds <- surv %>% 
      filter(start >= instarttime & end <= inendtime & !is.na(dive_num))
    
    # if none of the surveys fit
    if(nrow(inds) == 0){
      print(str_c("File", files[i], "does not cover a complete survey"))
      # find a survey that at least starts or ends within this GPX track. 
      # TODO: change this to warn if a survey doesn't have a GPX instead of a GPX
      # wihtout a survey. 
      inds <- surv %>% 
        filter((end <= inendtime & end >= instarttime)| (start <= inendtime & start >= instarttime))
      if(nrow(inds) == 0){
        print(str_c("EVEN WORSE:", files[i], "does not cover even PART of a survey"))
      }
    }
    
    # trim the gpx data to fit the survey and write an output file
    if (nrow(inds) > 0) {
      for(j in 1:nrow(inds)){ # step through each survey that fits within this track (one or more)
        # output all: not just if this was a dive for collecting APCL
        x <- inds[j,]
        y <- files[i]
        # debugonce(index_line)
        index_line(x,y, dat)
      }
    }
  }
}

# now import the trimmed tracks into QGIS as vector files ####




