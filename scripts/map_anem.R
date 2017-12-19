# map_anem ####
#' create a csv of anem locations and ids that can be imported into QGIS
#' @export
#' @name map_anem
#' @author Michelle Stuart
#' @param x = year
#' @examples 
#' dat <- map_anem(2016)

map_anem <- function(x){
  library(dplyr)
  library(tidyr)
  library(lubridate)
  source("scripts/con_leyte.R")
  source("scripts/date_dive.R")

  begin <- paste(x, "01", "01", sep = "-")
  end <- paste(x, "12", "31", sep = "-")
  
  leyte <- conleyte()
  
  # pull dives for date range
  dive <- date_dive(begin, end)
  
  # reduce number of columns
  dive <- dive %>% 
    select(dive_table_id, date, site, gps) %>% 
    rename(unit = gps)
  
  # find anemones that match those dives and dates
  anem <- leyte %>%
    tbl("anemones") %>%
    filter(dive_table_id %in% dive$dive_table_id & !is.na(anem_id)) %>%
    select(dive_table_id, anem_table_id, anem_id, anem_obs, obs_time) %>%
    collect() %>% 
    separate(obs_time, into = c("hour", "minute", "second"), sep = ":") %>% 
    mutate(gpx_hour = as.numeric(hour) - 8) %>% 
    mutate(minute = as.numeric(minute))
  
  anem <- left_join(anem, dive, by = "dive_table_id")
  
  rm(dive)
  
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
    tbl("GPX") %>%
    mutate(gpx_date = date(time)) %>%
    filter(gpx_date %in% anem$gpx_date) %>% 
    mutate(gpx_hour = hour(time)) %>% 
    mutate(minute = minute(time)) %>% 
    mutate(second = second(time)) %>% 
    select(-time, -second) %>% 
    collect()
  
  sum_lat <- lat %>%
    group_by(unit, gpx_date, gpx_hour, minute) %>% 
    summarise(lat = mean(as.numeric(lat)),
      lon = mean(as.numeric(lon)))
  
  anem <- left_join(anem, sum_lat, by = c("unit", "gpx_date", "gpx_hour", "minute"))
  rm(lat, other, sum_lat, test)


# Write out for QGIS (has column headers)
  # add fish info
  # fish <- leyte %>% 
  #   tbl("clownfish") %>% 
  #   filter(anem_table_id %in% anem$anem_table_id) %>%
  #   filter(!is.na(sample_id)) %>% 
  #   collect()
  # 
  # anem <- left_join(anem, fish, by = "anem_table_id")
  # 
  # test <- anem %>% 
  #   group_by(anem_id, site, lat, lon, date) %>% 
  #   
  # 
  
  
out <- anem %>%
  select(lat, lon, date, site, anem_id, anem_obs)

name <- paste("~/Documents/Phils_GIS/Anems/",Sys.Date(), "_GPSSurvey_anemlatlong_", x, "_forQGIS.csv", sep = "")
write.table(out, file = name, col.names=T, sep=',', row.names=F, quote=T)

return(anem)
}


  
  


