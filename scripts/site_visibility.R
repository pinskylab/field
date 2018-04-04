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
dives_Avg <- dives_Avg %>%
  mutate(avg_vis = ifelse((year == 2017 & site == "Cabatoan"), Cab2017vis, avg_vis)) %>%
  mutate(avg_vis = ifelse((year == 2017 & site == "Magbangon"), Mag2017vis, avg_vis))

dives_annualAvg <- dives_annualAvg %>% 
  mutate(avg_vis = ifelse((year == 2017 & site == "Cabatoan"), Cab2017vis, avg_vis)) %>%
  mutate(min_vis = ifelse((year == 2017 & site == "Cabatoan"), Cab2017vis, min_vis)) %>%
  mutate(max_vis = ifelse((year == 2017 & site == "Cabatoan"), Cab2017vis, max_vis)) %>%
  mutate(avg_vis = ifelse((year == 2017 & site == "Magbangon"), Mag2017vis, avg_vis)) %>%
  mutate(min_vis = ifelse((year == 2017 & site == "Magbangon"), Mag2017vis, min_vis)) %>%
  mutate(max_vis = ifelse((year == 2017 & site == "Magbangon"), Mag2017vis, max_vis)) 
  
#################### Plots! ####################
#mean vis for Albuera sites, all dives counted as individual obs, not averaged by date first
pdf(file = "plots/Albuera_AvgVis.pdf")
ggplot(data = (dives_Avg %>% filter(site %in% Albuera_sites)), aes(x=year, y=avg_vis, color=site, shape=site)) +
  geom_point(size=5.0, position=position_dodge(width=0.1)) +
  geom_line() +
  ggtitle('Mean visibility through time at Albuera sites') +
  labs(x='year', y='average vis (m)') +
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017)) +
  theme_bw()
dev.off()

#mean vis for all sites, all dives counted as individual obs, not averaged by date first
pdf(file = "plots/AllSites_AvgVis.pdf")
ggplot(data = dives_Avg, aes(x=year, y=avg_vis, color=site)) +
  geom_point(size=4.0, position=position_dodge(width=0.1)) +
  ggtitle('Mean visibility through time at all sites') +
  labs(x='year', y='average vis (m)') +
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017)) +
  theme_bw()
dev.off()

#mean and min-max range, all dives counted as individual obs (even multiple people on the same dive), averaged by day first
pdf(file = "plots/Albuera_AvgVisMaxMin.pdf")
ggplot(data = (dives_annualAvg %>% filter(site %in% Albuera_sites)), aes(x=year, y=avg_vis, color=site, shape=site)) +
  geom_pointrange(aes(ymin=min_vis, ymax=max_vis), size=1.0, position=position_dodge(width=0.3)) +
  ggtitle('Visibility through time at Albuera sites (mean with min-max range)') +
  labs(x='year', y='average vis (m) with min-max range') +
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017)) +
  scale_y_continuous(limits = c(0,16)) +
  theme_bw()
dev.off()

# ##### OLD CODE, not useful

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
