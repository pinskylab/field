# helper scripts for field work
library(dplyr)
# read_db ####
#' views all of the fish recaptured at a given site
#' @export
#' @name read_db
#' @author Michelle Stuart
#' @param x = which db?
#' @examples 
#' db <- read_Db("Leyte")

read_db <- function(db_name){
  
  db <- src_mysql(dbname = db_name, default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
  return(db)
}

# anem_date ####
#' allows you to find the date based on the anem_table_id
#' @param x = anem_table_id - Where are the anem_table_id's located?
#' @keywords 
#' @export
#' @examples
#' anem_date(anem_table_id)
#' anem_date(2206)

anem_date <- function(anem_tbl_id){
  leyte <- read_db("Leyte")
  anem <- leyte %>% 
    tbl("anemones") %>% 
    filter(anem_table_id %in% anem_tbl_id) %>% 
    select(dive_table_id, anem_table_id) %>% 
    collect()
  
  day <- leyte %>% 
    tbl("diveinfo") %>%
    select(date, dive_table_id) %>%
    collect() %>% 
    filter(dive_table_id %in% anem$dive_table_id)
  
  day <- left_join(day, anem, by ="dive_table_id")
  return(day)
}

# fish_date ####
#' allows you to find the date based on the fish_table_id
#' @param x = fish_table_id - Where are the fish_table_ids's located?
#' @keywords 
#' @export
#' @examples
#' fish_date(anem_table_id)
#' fish_date(2206)

fish_date <- function(fish_tbl_id){
  leyte <- read_db("Leyte")
  fish <- leyte %>% 
    tbl("clownfish") %>% 
    filter(fish_table_id %in% fish_tbl_id) %>% 
    select(fish_table_id, anem_table_id) %>% 
    collect()
  anem <- leyte %>% 
    tbl("anemones") %>% 
    filter(anem_table_id %in% fish$anem_table_id) %>% 
    select(anem_table_id, dive_table_id) %>% 
    collect()
  anem <- left_join(fish, anem, by = "anem_table_id") 
  
  day <- leyte %>% 
    tbl("diveinfo") %>%
    select(date, dive_table_id) %>%
    collect() %>% 
    filter(dive_table_id %in% anem$dive_table_id)
  
  day <- left_join(day, anem, by ="dive_table_id")
  return(day)
}

# anem_site ####
#' return a table of anem_id and site
#' @export
#' @name read_db
#' @author Michelle Stuart
#' @param x = anem_id
#' @examples 
#' sites <- anem_site(anems)

anem_site <- function(anem_table_with_dive_ids){
  x <- leyte %>% 
    tbl("diveinfo") %>% 
    filter(dive_table_id %in% anem_table_with_dive_ids$dive_table_id) %>% 
    select(dive_table_id, site) %>% 
    collect()
  anems <- left_join(anem_table_with_dive_ids, x, by = "dive_table_id")
  return(anems)
}

# anem_latlong ####
anem_latlong <- function(anem_table_with_dive_ids){
  library(tidyr)
  library(lubridate)
  anem <- anem_table_with_dive_ids %>% 
    filter(!is.na(anem_id)) %>% 
    separate(obs_time, into = c("hour", "minute", "second"), sep = ":") %>% 
    mutate(gpx_hour = as.numeric(hour) - 8) %>% 
    mutate(minute = as.numeric(minute))
  
  dive <- leyte %>%
    tbl("diveinfo") %>%
    filter(dive_table_id %in% anem$dive_table_id) %>%
    select(dive_table_id, date, site, gps) %>%
    collect()

    anem <- left_join(anem, dive, by = c("dive_table_id", "date"))
    anem <- rename(anem, unit = gps)

  # fix date if gpx hour is less than 0
  test <- anem %>% 
    filter(gpx_hour < 0)
  
  if (nrow(test) > 0){
    anem <- anem %>%
      mutate(gpx_date = date) # create a new gpx date column
    
    other <- anem %>% 
      filter(gpx_hour < 0) 
    
    # remove from anem table
    anem <- anti_join(anem, other)
    
    # subtract date
    other <- other %>% 
      mutate(gpx_date = as.character(ymd(date) - days(1))) %>% 
      mutate(gpx_hour = gpx_hour + 24)
    
    # rejoin rows
    anem <- rbind(anem, other)
    
  }else{
    anem <- anem %>% mutate(gpx_date = date)
  }
  
  # find the lat long for this anem
  lat <- leyte %>%
    tbl("GPX")  %>% 
    mutate(gpx_date = date(time)) %>%
    filter(gpx_date %in% anem$gpx_date) %>% 
    mutate(gpx_hour = hour(time)) %>%
    mutate(minute = minute(time)) %>%
    mutate(second = second(time)) %>%
    select(-time, -second)%>%
    collect(n=Inf) 
  
  sum_lat <- lat %>%
    group_by(unit, gpx_date, gpx_hour, minute) %>% 
    summarise(lat = mean(as.numeric(lat)),
      lon = mean(as.numeric(lon)))
  
  anem <- left_join(anem, sum_lat, by = c("unit", "gpx_date", "gpx_hour", "minute"))
  return(anem)
}

# -------------------------------------------------------------------
#   Field functions - functions used while working in the Philippines
# -------------------------------------------------------------------
library(dplyr)
# excl ####
#'is a function to read any excel sheet "x", into R with the defined col_types "y", if you don't want to define you col_types, you must enter NULL. 
#' @export
#' @name excl
#' @author Michelle Stuart
#' @param x = sheet name in quotes
#' @param y = col_types in a list 
#' @examples 
#' surv <- excl("diveinfo", NULL)
#' 
#' clowncol <- c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text")
#' clown <- excl("clownfish", clowncol)

excl <- function(x, y) {
  excl <- readxl::read_excel(excel_file, sheet = x, col_names = T, col_types = y)
  return(excl)
}


# comparedives #### 
#' is a function that compares the anem ids on the clownfish sheet to the anem ids on the anemone sheet by dive number for the 2017 field season.
#' @export
#' @name compare_dives
#' @author Michelle Stuart
#' @param x = clowndive - a list of dive numbers found on the clownfish sheet
#' @examples 
#' bad <- comparedives(clowndive)

compare_dives <- function(list_of_dive_nums) {
  for (i in 1:length(list_of_dive_nums)){
    fishanem <- filter(clown, divenum == list_of_dive_nums[i]) # for one dive at a time in the fish table
    anemanem <- filter(anem, divenum == list_of_dive_nums[i]) # and the anem table
    good <- filter(fishanem, anemid %in% anemanem$anemid) # find all of the anems that are in the anem table
    bad <- anti_join(fishanem, good) # and those anems that aren't
    bad <- filter(bad, anemid != "????") # except for any with an unknown anem 
    return(bad)
  }
}


# from_scanner #### 
#' is a function that reads the pit scanner text file into R.
#' @export
#' @name from_scanner
#' @author Michelle Stuart
#' @param x = pitfile (the path to the pit scanner file)
#' @examples 
#' pit <- from_scanner(pitfile)

from_scanner <- function(pitfile) {
  pit <- readr::read_csv(pitfile, 
    col_names = c("city", "tagid", "date", "time"), 
    col_types = cols(
      city = col_character(),
      tagid = col_character(),
      date = col_character(), # have to specify as string  
      time = col_character() # have to specify as string
    )
  )
  pit <- distinct(pit)
  pit <- tibble::as_tibble(pit) %>% # merge fields into tag number for fish
    unite(scan, city, tagid, sep = "")
  return(pit)
}

# name_gps ####
#' is a function that names all of our gps units.  Sure, 5 isn't that many right now but as they fail and we buy more it will help to have a function that does this for us. 
#' @export
#' @name name_gps
#' @author Michelle Stuart
#' @param 
#' @examples 
#' gps <- name_gps()
name_gps <- function() {
  gps <- c()
  for (i in 1:5){ # because we have 5 gps units
    x <- paste("gps", i, sep = "")
    gps <- c(gps, x)
  }
  return(gps)
}


# used_gps ####
#' is a function that checks the folders of all of the gps units and determines which contain data. 
#' @export
#' @name used_gps
#' @author Michelle Stuart
#' @param 
#' @examples 
#' gps <- used_gps(gps)

used_gps <- function(gps) {
  for (i in 1:length(gps)){
    test <- list.files(path = paste("data/", gps[i], sep = ""), pattern = "*Track*")
    if (length(test) == 0){
      gps[[i]] <- NA
    }
  }
  gps <- gps[!is.na(gps)]
  return(gps)
}

# prep_gpx ####
#' is a function that reads a gpx file and parses out the useable data. 
#' @export
#' @name prep_gpx
#' @author Michelle Stuart
#' @param x = gps - list of gps units
#' @param y = files - list of track files
#' @examples 
#' dat <- parse_gpx(gps, files)

prep_gpx <- function(x, y) {
  infile <- readGPXGarmin(paste("data/", x[l], "/", y[i], sep="")) # list of 2 itmes, header and data
  header <<- infile$header
  dat <- infile$data
  dat$time <- ymd_hms(dat$time)
  instarttime <<-  dat$time[1] # start time for this GPX track
  inendtime <<- dat$time[nrow(dat)] # end time for this GPX track
  dat$elev <- 0 # change elevation to zero
  dat$unit <- substr(x[l],4,4)
  return(dat)
}



# index_line ####
#' a function that reads through each line of an index and writes an output file in gpx format
#' @export
#' @name index_line
#' @author Michelle Stuart
#' @param x = one line of an index
#' @param y = one track file
#' @param dat = a table of gpx data
#' @examples 
#' index_line(inds[j, ], files[i])

index_line <- function(x, y, dat){
  # if no pause
  if(is.na(x$paust)){
    # find the GPX points that fit within the survey
    track <- dat %>%
      filter(time >= x$start & time <= x$end) %>% 
      mutate(
        lat = as.character(lat),
        lon = as.character(lon),
        elev = as.character(elev),
        time = as.character(time)
      )
    writeGPX(filename = str_c("data/gpx_trimmed/GPS", track$unit[1], "_", x$divenum, "_", y, sep=""), outfile = track)
  }
  # account for a pause if need be
  if(!is.na(x$paust)){
    track1 <- dat %>%
      filter(time >= x$start & time <= x$paust) %>% 
      mutate(
        lat = as.character(lat),
        lon = as.character(lon),
        elev = as.character(elev),
        time = as.character(time)
      )
    track2 <- dat %>%
      filter(time >= x$pausend & time <= x$end) %>% 
      mutate(
        lat = as.character(lat),
        lon = as.character(lon),
        elev = as.character(elev),
        time = as.character(time)
      )
    
    # write as two tracks
    writeGPX(filename = paste("data/gpx_trimmed/GPS", track1$unit[1], "_",x$divenum, "_", y, '_1.gpx', sep=''), outfile = track1) 
    writeGPX(filename = paste("data/gpx_trimmed/GPS", track2$unit[1], "_",x$divenum, "_", y, '_2.gpx', sep=''), outfile = track2)
  }
  
}

# read_db ####
#' views all of the fish recaptured at a given site
#' @export
#' @name read_db
#' @author Michelle Stuart
#' @param x = which db?
#' @examples 
#' db <- read_Db("Leyte")

read_db <- function(db_name){
  
  db <- src_mysql(dbname = db_name, default.file = path.expand("~/myconfig.cnf"), port = 3306, create = F, host = NULL, user = NULL, password = NULL)
  return(db)
}


