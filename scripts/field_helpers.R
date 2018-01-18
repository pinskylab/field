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
