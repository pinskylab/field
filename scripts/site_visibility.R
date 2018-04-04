# Assess visibility across sites and across time for Albuera managers

## Helpful to put the Baybay sites in there too, just for reference?

#################### Set-up: ####################
# Load relevant libraries
library(dplyr)
#library(tidyr)
#library(tidyverse)
library(lubridate)
#library(dbplyr)
library(ggplot2)

# Load database files
#load(file = "data/db_backups/clownfish_db.Rdata") 
#load(file = "data/db_backups/anemones_db.Rdata")
load(file = "data/db_backups/diveinfo_db.Rdata")

# Rename so not overwritten by new datasheets when they get loaded
#clown_db <- clown
#anem_db <- anem
dive_db <- dive

# Add year and month as a category to the dives 
dive_db$year <- as.integer(substring(dive_db$date,1,4)) #add year to dives
dive_db$month <- as.integer(substring(dive_db$date,6,7)) #add month to dives

# Visibility ranges used and the numbers to convert them to
vis_rec <- c("<1", "2-3", "2 to 3", "2 to 4", "2 to 5", "2 to 6", "2-8", "3 to 4", "3 to 5", "3 to 8", "4m", "4 to 1", "4 to 2", "4 to 3", "4 to 5", "4 decreasing to 2", "5 to 8", "6, lots of particles", "7 - 5 - 1")
vis_max <- c(1, 3, 3, 4, 5, 6, 8, 4, 5, 8, 4, 4, 4, 4, 5, 4, 8, 6, 7)  #maximum visibility in range recorded
vis_min <- c(0, 2, 2, 2, 2, 2, 2, 3, 3, 3, 4, 1, 2, 3, 4, 2, 5, 6, 1)  #minimum visibility in range recorded
vis_avg <- c(0.5, 2.5, 2.5, 3, 3.5, 4, 5, 3.5, 4, 5.5, 4, 2.5, 3, 3.5, 4.5, 3, 6.5, 6, 4) #average visibility in range recorded
vis_convert <- data.frame(vis_rec, vis_max, vis_min, vis_avg)

# Notes for testing: 139 is first row in "dive" with a range recorded for visibility_m ("4 to 1")

# Sets of sites
Albuera_sites <- c("Palanas", "Wangag", "Magbangon", "Cabatoan")

# Visibility (in m) for years when it was judged too poor to dive
Cab2017vis <- 1
Mag2017vis <- 1

#################### Functions: ####################
source("scripts/field_helpers.R")

# Convert visibilities entered as text (like "2 to 3") to numbers
convertVisRangeToNum <- function(df, convertList, vis_choice) { #vis_choice = maxV, minV, avgV
  df$visibility_m_clean <- rep(NA, length(df$dive_table_id), 1) #add a column for cleaned-up visibility
  
  for(i in 1:length(df$dive_table_id)){
    vis <- df$visibility_m[i] #pull out the recorded visibility 
    if(vis %in% convertList$vis_rec == FALSE) { #if recorded vis isn't one of the ranges, just use the recorded vis
      df$visibility_m_clean[i] <- df$visibility_m[i]
    } else { #if it is, convert as chosed (to max, min, or avg of range)
      vis_id <- which(convertList$vis_rec %in% vis) #find the position of the range recorded in the convert df
      if(vis_choice == 'maxV') { 
        df$visibility_m_clean[i] <- convertList$vis_max[vis_id] # if want to convert to max of range
      } else if (vis_choice == 'minV') {
        df$visibility_m_clean[i] <- convertList$vis_min[vis_id] # if want to convert to min of range
      } else if (vis_choice == 'avgV') {
        df$visibility_m_clean[i] <- convertList$vis_avg[vis_id] # if want to convert to avg of range
      }
    }
  }
  df$visibility_m_clean <- as.numeric(df$visibility_m_clean) #convert to numeric
  return(df)
}
 
 
#################### Running things! ####################
##### Get 2018 data 
get_from_google() #this pulls in the data from the 2018 datasheets but will overwrite the saved database files...

# Select relevant 2018 columns and rename so fits with convertVisRangeToNum function
dives_2018 <- dive %>% 
  select(dive_id, date, site, visibility) %>%
  rename(dive_table_id = dive_id, visibility_m = visibility) %>%
  mutate(year = as.integer(substring(date,1,4)))

##### Find average vis per year by site
# Convert ranges (like "2 to 3" entered into the database to numbers (either max, min, or average of range))
dives_visClean_db <- convertVisRangeToNum(dive_db, vis_convert, 'avgV') %>%
  select(date, site, year, visibility_m_clean)
dives_visClean_2018 <- convertVisRangeToNum(dives_2018, vis_convert, 'avgV') %>%
  select(date, site, year, visibility_m_clean)

# Combine database and 2018 data
dives_visClean <- rbind(dives_visClean_db, dives_visClean_2018)

# dives_visClean <- dives_visClean %>% 
#   filter(site %in% Albuera_sites) %>% #pull out Albuera sites
#   select(date, site, year, visibility_m_clean) #select relevant columns

# Average by day to get one vis obs per day 
dives_dailyAvg <- dives_visClean %>% 
  group_by(year, site, date) %>% 
  summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n())

# Find mean/max/min vis each year at each site, averaging across days rather than individual dives
dives_annualAvg <- dives_dailyAvg %>%
  group_by(year, site) %>%
  summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n())

# Find average vis per year by site, not averaging over day first (just averaging all dives)
dives_Avg <- dives_visClean %>%
  group_by(year, site) %>%
  summarise(avg_vis = mean(visibility_m_clean, na.rm = TRUE))

# Replace Cab and Mag vis in 2017 (currently NA b/c no full sampling dives there)
dives_Avg <- mutate(dives_Avg, avg_vis = ifelse((year == 2017 & site == "Cabatoan"), Cab2017vis, avg_vis))
dives_Avg <- mutate(dives_Avg, avg_vis = ifelse((year == 2017 & site == "Magbangon"), Mag2017vis, avg_vis))

dives_annualAvg <- dives_annualAvg %>% 
  mutate(avg_vis = ifelse((year == 2017 & site == "Cabatoan"), Cab2017vis, avg_vis)) %>%
  mutate(min_vis = ifelse((year == 2017 & site == "Cabatoan"), Cab2017vis, min_vis)) %>%
  mutate(max_vis = ifelse((year == 2017 & site == "Cabatoan"), Cab2017vis, max_vis)) %>%
  mutate(avg_vis = ifelse((year == 2017 & site == "Magbangon"), Mag2017vis, avg_vis)) %>%
  mutate(min_vis = ifelse((year == 2017 & site == "Magbangon"), Mag2017vis, min_vis)) %>%
  mutate(max_vis = ifelse((year == 2017 & site == "Magbangon"), Mag2017vis, max_vis)) 
  
#################### Plots! ####################
#mean vis for Albuera sites, all dives counted as individual obs, not averaged by date first
pdf(file = "Albuera_AvgVis.pdf")
ggplot(data = (dives_Avg %>% filter(site %in% Albuera_sites)), aes(x=year, y=avg_vis, color=site, shape=site)) +
  geom_point(size=5.0, position=position_dodge(width=0.1)) +
  ggtitle('Mean visibility through time at Albuera sites') +
  labs(x='year', y='average vis (m)') +
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017)) +
  theme_bw()
dev.off()

#mean vis for all sites, all dives counted as individual obs, not averaged by date first
pdf(file = "AllSites_AvgVis.pdf")
ggplot(data = dives_Avg, aes(x=year, y=avg_vis, color=site)) +
  geom_point(size=4.0, position=position_dodge(width=0.1)) +
  ggtitle('Mean visibility through time at all sites') +
  labs(x='year', y='average vis (m)') +
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017)) +
  theme_bw()
dev.off()

#mean and min-max range, all dives counted as individual obs (even multiple people on the same dive), averaged by day first
pdf(file ="Albuera_AvgVisMaxMin.pdf")
ggplot(data = (dives_annualAvg %>% filter(site %in% Albuera_sites)), aes(x=year, y=avg_vis, color=site, shape=site)) +
  geom_pointrange(aes(ymin=min_vis, ymax=max_vis), size=1.0, position=position_dodge(width=0.3)) +
  ggtitle('Visibility through time at Albuera sites (mean with min-max range)') +
  labs(x='year', y='average vis (m) with min-max range') +
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017)) +
  scale_y_continuous(limits = c(0,16)) +
  theme_bw()
dev.off()

# ##### OLD CODE, not useful
# 
# # #average by day to get one vis obs per day and filter for Albuera sites
# # dives_2012_Pal <- dives_visClean %>% filter(year == 2012 & site == "Palanas") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2013_Pal <- dives_visClean %>% filter(year == 2013 & site == "Palanas") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2014_Pal <- dives_visClean %>% filter(year == 2014 & site == "Palanas") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2015_Pal <- dives_visClean %>% filter(year == 2015 & site == "Palanas") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2016_Pal <- dives_visClean %>% filter(year == 2016 & site == "Palanas") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2017_Pal <- dives_visClean %>% filter(year == 2017 & site == "Palanas") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2012_Cab <- dives_visClean %>% filter(year == 2012 & site == "Cabatoan") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2013_Cab <- dives_visClean %>% filter(year == 2013 & site == "Cabatoan") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2014_Cab <- dives_visClean %>% filter(year == 2014 & site == "Cabatoan") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2015_Cab <- dives_visClean %>% filter(year == 2015 & site == "Cabatoan") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2016_Cab <- dives_visClean %>% filter(year == 2016 & site == "Cabatoan") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2017_Cab <- dives_visClean %>% filter(year == 2017 & site == "Cabatoan") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2012_Mag <- dives_visClean %>% filter(year == 2012 & site == "Magbangon") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2013_Mag <- dives_visClean %>% filter(year == 2013 & site == "Magbangon") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2014_Mag <- dives_visClean %>% filter(year == 2014 & site == "Magbangon") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2015_Mag <- dives_visClean %>% filter(year == 2015 & site == "Magbangon") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2016_Mag <- dives_visClean %>% filter(year == 2016 & site == "Magbangon") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2017_Mag <- dives_visClean %>% filter(year == 2017 & site == "Magbangon") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2012_Wan <- dives_visClean %>% filter(year == 2012 & site == "Wangag") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2013_Wan <- dives_visClean %>% filter(year == 2013 & site == "Wangag") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2014_Wan <- dives_visClean %>% filter(year == 2014 & site == "Wangag") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2015_Wan <- dives_visClean %>% filter(year == 2015 & site == "Wangag") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2016_Wan <- dives_visClean %>% filter(year == 2016 & site == "Wangag") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2017_Wan <- dives_visClean %>% filter(year == 2017 & site == "Wangag") %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # 
# # # now find max/mean/min per year
# # dives_2012_Pal_a <- dives_2012_Pal %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2012_Pal_a$year <- 2012
# # dives_2013_Pal_a <- dives_2013_Pal %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2013_Pal_a$year <- 2013
# # dives_2014_Pal_a <- dives_2014_Pal %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2014_Pal_a$year <- 2014
# # dives_2015_Pal_a <- dives_2015_Pal %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2015_Pal_a$year <- 2015
# # dives_2016_Pal_a <- dives_2016_Pal %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2016_Pal_a$year <- 2016
# # dives_2017_Pal_a <- dives_2017_Pal %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2017_Pal_a$year <- 2017
# # dives_2012_Cab_a <- dives_2012_Cab %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2012_Cab_a$year <- 2012
# # dives_2013_Cab_a <- dives_2013_Cab %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2013_Cab_a$year <- 2013
# # dives_2014_Cab_a <- dives_2014_Cab %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2014_Cab_a$year <- 2014
# # dives_2015_Cab_a <- dives_2015_Cab %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2015_Cab_a$year <- 2015
# # dives_2016_Cab_a <- dives_2016_Cab %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2016_Cab_a$year <- 2016
# # dives_2017_Cab_a <- dives_2017_Cab %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2017_Cab_a$year <- 2017
# # dives_2012_Mag_a <- dives_2012_Mag %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2012_Mag_a$year <- 2012
# # dives_2013_Mag_a <- dives_2013_Mag %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2013_Mag_a$year <- 2013
# # dives_2014_Mag_a <- dives_2014_Mag %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2014_Mag_a$year <- 2014
# # dives_2015_Mag_a <- dives_2015_Mag %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2015_Mag_a$year <- 2015
# # dives_2016_Mag_a <- dives_2016_Mag %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2016_Mag_a$year <- 2016
# # dives_2017_Mag_a <- dives_2017_Mag %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2017_Mag_a$year <- 2017
# # dives_2012_Wan_a <- dives_2012_Wan %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2012_Wan_a$year <- 2012
# # dives_2013_Wan_a <- dives_2013_Wan %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2013_Wan_a$year <- 2013
# # dives_2014_Wan_a <- dives_2014_Wan %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2014_Wan_a$year <- 2014
# # dives_2015_Wan_a <- dives_2015_Wan %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2015_Wan_a$year <- 2015
# # dives_2016_Wan_a <- dives_2016_Wan %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2016_Wan_a$year <- 2016
# # dives_2017_Wan_a <- dives_2017_Wan %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n(), site = first(site))
# # dives_2017_Wan_a$year <- 2017
# # 
# # dives_Alb <- rbind(dives_2012_Cab_a, dives_2012_Mag_a, dives_2012_Pal_a, dives_2012_Wan_a,
# #                    dives_2013_Cab_a, dives_2013_Mag_a, dives_2013_Pal_a, dives_2013_Wan_a,
# #                    dives_2014_Cab_a, dives_2014_Mag_a, dives_2014_Pal_a, dives_2014_Wan_a,
# #                    dives_2015_Cab_a, dives_2015_Mag_a, dives_2015_Pal_a, dives_2015_Wan_a,
# #                    dives_2016_Cab_a, dives_2016_Mag_a, dives_2016_Pal_a, dives_2016_Wan_a,
# #                    dives_2017_Cab_a, dives_2017_Mag_a, dives_2017_Pal_a, dives_2017_Wan_a)
# 
# # #NOT SURE WHY THE BELOW DOESN'T WORK BUT IT MISSES CABATOAN IN 2012 and 2013 even though there was a dive each year there - think the grouping by date overrides grouping by site...
# # #average by day to get one vis obs per day and filter for Albuera sites (not totally clear that/why this works, given that multiple groupings hasn't worked below...)
# # dives_2012_Alb <- dives_visClean %>% filter(year == 2012 & site %in% Albuera_sites) %>% group_by(site) %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2013_Alb <- dives_visClean %>% filter(year == 2013 & site %in% Albuera_sites) %>% group_by(site) %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2014_Alb <- dives_visClean %>% filter(year == 2014 & site %in% Albuera_sites) %>% group_by(site) %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2015_Alb <- dives_visClean %>% filter(year == 2015 & site %in% Albuera_sites) %>% group_by(site) %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2016_Alb <- dives_visClean %>% filter(year == 2016 & site %in% Albuera_sites) %>% group_by(site) %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# # dives_2017_Alb <- dives_visClean %>% filter(year == 2017 & site %in% Albuera_sites) %>% group_by(site) %>% group_by(date) %>% summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n(), site = first(site))
# 
# # #now find min/max/mean of daily
# # dives_2012_A <- dives_2012_Alb %>% group_by(site) %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n())
# # dives_2012_A$year <- 2012
# # dives_2013_A <- dives_2013_Alb %>% group_by(site) %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n())
# # dives_2013_A$year <- 2013
# # dives_2014_A <- dives_2014_Alb %>% group_by(site) %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n())
# # dives_2014_A$year <- 2014
# # dives_2015_A <- dives_2015_Alb %>% group_by(site) %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n())
# # dives_2015_A$year <- 2015
# # dives_2016_A <- dives_2016_Alb %>% group_by(site) %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n())
# # dives_2016_A$year <- 2016
# # dives_2017_A <- dives_2017_Alb %>% group_by(site) %>% summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n())
# # dives_2017_A$year <- 2017
# # 
# # dives_daily_Alb <- rbind(dives_2012_A, dives_2013_A, dives_2014_A, dives_2015_A, dives_2016_A, dives_2017_A)
# 
# #find yearly average by site, from only one dive a day
# #avg_vis_daily_Alb_2012 <- dives_daily_Alb %>% filter(year == 2012) %>% group_by(site) %>% summarize(avg_vis = mean(daily_avg_vis, rm.na = TRUE), min_vis = min(daily_avg_vis, rm.na = TRUE), max_vis = max(daily_avg_vis, rm.na = TRUE), ndays = n())
# #table((dives_visClean %>% filter(year == 2012))$site)
# 
# # #find average vis by year and site without removing multiple dives per day or observations per dive
# # avg_vis_2012 <- dives_visClean %>% filter(year == 2012) %>% group_by(site) %>% summarize(avg_vis = mean(visibility_m_clean, rm.na = TRUE), std_vis = sd(visibility_m_clean, na.rm = TRUE), min_vis = min(visibility_m_clean, na.rm = TRUE), max_vis = max(visibility_m_clean, na.rm = TRUE), ndives = n())
# # avg_vis_2012$year <- rep(2012, length(avg_vis_2012$site), 1)
# # avg_vis_2013 <- dives_visClean %>% filter(year == 2013) %>% group_by(site) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE), std_vis = sd(visibility_m_clean, na.rm = TRUE), min_vis = min(visibility_m_clean, na.rm = TRUE), max_vis = max(visibility_m_clean, na.rm = TRUE), ndives = n())
# # avg_vis_2013$year <- rep(2013, length(avg_vis_2012$site), 1)
# # avg_vis_2014 <- dives_visClean %>% filter(year == 2014) %>% group_by(site) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE), std_vis = sd(visibility_m_clean, na.rm = TRUE), min_vis = min(visibility_m_clean, na.rm = TRUE), max_vis = max(visibility_m_clean, na.rm = TRUE), ndives = n())
# # avg_vis_2014$year <- rep(2014, length(avg_vis_2012$site), 1)
# # avg_vis_2015 <- dives_visClean %>% filter(year == 2015) %>% group_by(site) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE), std_vis = sd(visibility_m_clean, na.rm = TRUE), min_vis = min(visibility_m_clean, na.rm = TRUE), max_vis = max(visibility_m_clean, na.rm = TRUE), ndives = n())
# # avg_vis_2015$year <- rep(2015, length(avg_vis_2015$site), 1)
# # avg_vis_2016 <- dives_visClean %>% filter(year == 2016) %>% group_by(site) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE), std_vis = sd(visibility_m_clean, na.rm = TRUE), min_vis = min(visibility_m_clean, na.rm = TRUE), max_vis = max(visibility_m_clean, na.rm = TRUE), ndives = n())
# # avg_vis_2016$year <- rep(2016, length(avg_vis_2016$site), 1)
# # avg_vis_2017 <- dives_visClean %>% filter(year == 2017) %>% group_by(site) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE), std_vis = sd(visibility_m_clean, na.rm = TRUE), min_vis = min(visibility_m_clean, na.rm = TRUE), max_vis = max(visibility_m_clean, na.rm = TRUE), ndives = n())
# # avg_vis_2017$year <- rep(2017, length(avg_vis_2017$site), 1)
# # 
# # avg_vis_all <- rbind(avg_vis_2012, avg_vis_2013, avg_vis_2014, avg_vis_2015, avg_vis_2016, avg_vis_2017)
# 
# # add in a 1m visibility in 2017 for Magbangon and Cabatoan (unable to dive those years b/c visibility so bad)
# Cab2017vis <- 1
# Mag2017vis <- 1
# Cab_2017 <- avg_vis_all[1,]
# Cab_2017$site <- "Cabatoan"
# Cab_2017$avg_vis <- Cab2017vis
# Cab_2017$std_vis <- NA
# Cab_2017$min_vis <- Cab2017vis
# Cab_2017$max_vis <- Cab2017vis
# Cab_2017$ndives <- 1
# Cab_2017$ndays <- 1
# Cab_2017$year <- 2017
# 
# Mag_2017 <- avg_vis_all[1,]
# Mag_2017$site <- "Magbangon"
# Mag_2017$avg_vis <- Mag2017vis
# Mag_2017$std_vis <- NA
# Mag_2017$min_vis <- Mag2017vis
# Mag_2017$max_vis <- Mag2017vis
# Mag_2017$ndives <- 2
# Mag_2017$ndays <- 1
# Mag_2017$year <- 2017
# 
# # add those to the main dataframe
# avg_vis_all <- rbind(avg_vis_all, (Cab_2017 %>% select(site, avg_vis, std_vis, min_vis, max_vis, ndives, year)))
# avg_vis_all <- rbind(avg_vis_all, (Mag_2017 %>% select(site, avg_vis, std_vis, min_vis, max_vis, ndives, year)))
# 
# #and to the daily-averaged data frame
# dives_Alb <- rbind(dives_Alb, (Cab_2017 %>% select(avg_vis, min_vis, max_vis, ndays, site, year)))
# dives_Alb <- rbind(dives_Alb, (Mag_2017 %>% select(avg_vis, min_vis, max_vis, ndays, site, year)))
# 
# 
# #trying to figure out how to do the above in fewer lines...
# # site_group <- group_by(dives_visClean, site)
# # avg_vis_site <- site_group %>% group_by(year) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE), ndives = n())
# # avg_vis_site_year <- dives_visClean %>% group_by(site) %>% summarize()
# # avg_vis_per_year <- dives %>% filter(visi)
# # avg_vis_per_year <- dives_visClean %>% group_by(site, year) %>% summarize(avg_vis = mean(visibility_m_clean, na.rm = TRUE))
# 
# #################### Plots! ####################
# #mean and min-max range, all dives counted as individual obs (even multiple people on the same dive)
# pdf(file = here("Plots/Visibility", "AlbueraVisbySiteandYear.pdf"))
# ggplot(data = (avg_vis_all %>% filter(site %in% Albuera_sites)), aes(x=year, y=avg_vis, color=site)) +
#   geom_pointrange(aes(ymin=min_vis, ymax=max_vis), size=1.0, position=position_dodge(width=0.4)) +
#   ggtitle('Visibility through time at Albuera sites (mean with min-max range)') +
#   labs(x='year', y='average vis (m) with min-max range') +
#   scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017)) +
#   theme_bw()
# dev.off()
# 
# #mean and min-max range, all dives averaged by day, so days diving are individual obs
# #trying to make the dots the size as the number of days...
# pdf(file = here("Plots/Visibility", "AlbueraVisbySiteandYear_DivesAvgByDay_NDaysSize.pdf"), height=5, width=8)
# ggplot(data = dives_Alb, aes(x=year, y=avg_vis, color=ndays, shape=site)) +
#   geom_pointrange(aes(ymin=min_vis, ymax=max_vis, color=ndays, shape=site), size=1.0, position=position_dodge(width=0.6)) +
#   ggtitle('Visibility at Albuera sites (diving days as obs)') +
#   labs(x='year', y='average vis (m) with min-max range') +
#   scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017)) +
#   theme_bw()
# dev.off()
# 
# #and without sizing by number of days
# pdf(file = here("Plots/Visibility", "AlbueraVisbySiteandYear_DivesAvgByDay.pdf"))
# ggplot(data = dives_Alb, aes(x=year, y=avg_vis, color=site)) +
#   geom_pointrange(aes(ymin=min_vis, ymax=max_vis), size=1.0, position=position_dodge(width=0.6)) +
#   ggtitle('Visibility at Albuera sites') +
#   labs(x='year', y='average vis (m) with min-max range') +
#   scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017)) +
#   theme_bw()
# dev.off()
# # #just means #### doesn't work...
# # pdf(file = here("Plots/Visibility", "AlbueraVisbySiteandYear_Means.pdf"))
# # ggplot(data = (avg_vis_all %>% filter(site %in% Albuera_sites)), aes(x=year, y=avg_vis, color=site)) +
# #   geom_point(year, avg_vis) +
# #   ggtitle('Visibility through time') +
# #   labs(x='year', y='average vis') +
# #   theme_bw()
# # dev.off()
# 
# 
