# ---------------------------------------------
#   Set up work space - load packages and data
# ---------------------------------------------
library(tidyverse) # for pipe functions
library(lubridate) # for changing time zones
library(stringr) # for combining dates and times into dttm
source("scripts/readGPXGarmin.R")
source("scripts/writeGPXGarmin.R")
source("scripts/field_func.R")
excel_file <- ("data/GPSSurveys2017.xlsx")


# ---------------------------------------------
#   import and format diveinfo
# ---------------------------------------------
surv <- excl("diveinfo", NULL)
  
names(surv) <- stringr::str_to_lower(names(surv))
surv <- surv %>% 
  select(divenum, date, starttime, endtime, pausestart, pauseend, gps)

surv <- surv %>% 
  separate(starttime, into = c("baddate", "starttime"), sep = " ") %>%
  separate(endtime, into = c("baddate2", "endtime"), sep = " ") %>%
  separate(pausestart, into = c("baddate4", "pausestart"), sep = " ") %>%
  separate(pauseend, into = c("baddate5", "pauseend"), sep = " ") %>% 
  select(-contains("bad")) 

# Combine date and time to form a dttm column and set the time zone to PHT, Asia/Manila
surv$start <- str_c(surv$date, surv$starttime, sep = " ")  
surv$start <- ymd_hms(surv$start)
surv$start <- force_tz(surv$start, tzone = "Asia/Manila")
surv$end <- str_c(surv$date, surv$endtime, sep = " ")  
surv$end <- ymd_hms(surv$end)
surv$end <- force_tz(surv$end, tzone = "Asia/Manila")
surv$paust <- str_c(surv$date, surv$pausestart, sep = " ")  
surv$paust <- ymd_hms(surv$paust)
surv$paust <- force_tz(surv$paust, tzone = "Asia/Manila")
surv$pausend <- str_c(surv$date, surv$pauseend, sep = " ")  
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
    dat <- prep_gpx(gps, files) # parse the gpx into useable data
    
    # which survey started after the gpx and ended before the gpx
    inds <- surv %>% 
      filter(start >= instarttime & end <= inendtime & !is.na(divenum))
    
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
        index_line(x,y, dat)
      }
    }
  }
}





