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

get_from_google()

# # if network connection is not available, find the latest save in the data folder ####
# # load data from saved if network connection is lost ####
# # get list of files
# clown_files <- sort(list.files(path = "data/google_sheet_backups/", pattern = "clown_201*"), decreasing = T)
# dive_files <- sort(list.files(path = "data/google_sheet_backups/", pattern = "dive_201*"), decreasing = T)
# load(file = paste("data/google_sheet_backups/", clown_files[1], sep = ""))
# load(file = paste("data/google_sheet_backups/", dive_files[1], sep = ""))

surv <- dive
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

gpx <- data.frame()
# define the list of gps units
gps <- name_gps()

# determine which gps units have data & remove empties
gps <- used_gps(gps)

for (l in 1:length(gps)){
  files <- list.files(path = paste("data/",gps[l], sep = ""), pattern = "*Track*")
  for(i in 1:length(files)){ # for each file
    
    # parse the gpx into useable data
    infile <- readGPXGarmin(paste("data/", gps[l], "/", files[i], sep="")) # list of 2 itmes, header and data
    header <<- infile$header
    dat <- infile$data
    dat$time <- ymd_hms(dat$time)
    instarttime <<-  dat$time[1] # start time for this GPX track
    inendtime <<- dat$time[nrow(dat)] # end time for this GPX track
    dat$elev <- 0 # change elevation to zero
    dat$unit <- substr(gps[l],4,4)
    
    gpx <- rbind(gpx, dat)
    
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




