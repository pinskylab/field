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
surv <- gs_read(entry, ws="diveinfo")

# # if the data is in csv - the csvs aren't working with this code - need to convert characters to datte time??
# surv <- read.csv(file = "data/2018_clownfish_data_entry - diveinfo.csv", stringsAsFactors = F)
# surv <- surv %>% 
#   mutate(date = ymd(date), 
#     start_time = hms(start_time))
# 
# clown <- read.csv(file = "data/2018_clownfish_data_entry - clownfish-3.csv", stringsAsFactors = F)


names(surv) <- stringr::str_to_lower(names(surv))
surv <- surv %>% 
  select(dive_num, date, start_time, end_time, pause_start, pause_end, anem_gps)

# # if using excel, there is a weird date attached to the times, don't need if you aren't reading direct from excel.
# surv <- surv %>% 
#   separate(start_time, into = c("baddate", "start_time"), sep = " ") %>% #add convert=TRUE to separate to make numeric?
#   separate(end_time, into = c("baddate2", "end_time"), sep = " ") %>%
#   separate(pause_start, into = c("baddate4", "pause_start"), sep = " ") %>%
#   separate(pause_end, into = c("baddate5", "pause_end"), sep = " ") %>% 
#   select(-contains("bad")) 

# Combine date and time to form a dttm column and set the time zone to PHT, Asia/Manila 
surv$start <- str_c(surv$date, surv$start_time, sep = " ")  
surv$start <- ymd_hms(surv$start)
surv$start <- force_tz(surv$start, tzone = "Asia/Manila")
surv$end <- str_c(surv$date, surv$end_time, sep = " ")  
surv$end <- ymd_hms(surv$end)
surv$end <- force_tz(surv$end, tzone = "Asia/Manila")
surv$paust <- str_c(surv$date, surv$pause_start, sep = " ")  
surv$paust <- ymd_hms(surv$paust)
surv$paust <- force_tz(surv$paust, tzone = "Asia/Manila")
surv$pausend <- str_c(surv$date, surv$pause_end, sep = " ")  
surv$pausend <- ymd_hms(surv$pausend)
surv$pausend <- force_tz(surv$pausend, tzone = "Asia/Manila")


# Change time zone to UTC 
surv$start <- with_tz(surv$start, tzone = "UTC")
surv$end <- with_tz(surv$end, tzone = "UTC")
surv$paust <- with_tz(surv$paust, tzone = "UTC")
surv$pausend <- with_tz(surv$pausend, tzone = "UTC")

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





