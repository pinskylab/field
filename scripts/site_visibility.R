# Assess visibility across sites and across time for Albuera managers

## Helpful to put the Baybay sites in there too, just for reference?

#################### Set-up: ####################
# Load relevant libraries
library(RMySQL) 
library(dplyr)
#library(tidyr)
#library(tidyverse)
library(lubridate)
#library(dbplyr)
library(ggplot2)

#Old code to load database files before 2018 data was added to the database
# # Load database files
# #load(file = "data/db_backups/clownfish_db.Rdata") 
# #load(file = "data/db_backups/anemones_db.Rdata")
# load(file = "data/db_backups/diveinfo_db.Rdata")
# 
# # Rename so not overwritten by new datasheets when they get loaded
# #clown_db <- clown
# #anem_db <- anem
# dive_db <- dive
# 
# # Add year and month as a category to the dives 
# dive_db$year <- as.integer(substring(dive_db$date,1,4)) #add year to dives
# dive_db$month <- as.integer(substring(dive_db$date,6,7)) #add month to dives

# Visibility ranges used and the numbers to convert them to
vis_rec <- c("<1", "2-3", "2 to 3", "2 to 4", "2 to 5", "2 to 6", "2-8", "3 to 4", "3 to 5", "3 to 8", "4m", "4 to 1", "4 to 2", "4 to 3", "4 to 5", "4 decreasing to 2", "5 to 8", "6, lots of particles", "7 - 5 - 1")
vis_max <- c(1, 3, 3, 4, 5, 6, 8, 4, 5, 8, 4, 4, 4, 4, 5, 4, 8, 6, 7)  #maximum visibility in range recorded
vis_min <- c(0, 2, 2, 2, 2, 2, 2, 3, 3, 3, 4, 1, 2, 3, 4, 2, 5, 6, 1)  #minimum visibility in range recorded
vis_avg <- c(0.5, 2.5, 2.5, 3, 3.5, 4, 5, 3.5, 4, 5.5, 4, 2.5, 3, 3.5, 4.5, 3, 6.5, 6, 4) #average visibility in range recorded
vis_convert <- data.frame(vis_rec, vis_max, vis_min, vis_avg)

# Notes for testing: 139 is first row in "dive" with a range recorded for visibility_m ("4 to 1")

# Sets of sites
Albuera_sites <- c("Palanas", "Wangag", "Magbangon N", "Magbangon S", "Cabatoan")

# Visibility (in m) for years when it was judged too poor to dive
Cab2017vis <- 1
Mag2017vis <- 1
MagN2018vis <- 1

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
#Old code for getting 2018 data before it was put into the regular database
# ##### Get 2018 data 
# get_from_google() #this pulls in the data from the 2018 datasheets but will overwrite the saved database files...
# 
# # Select relevant 2018 columns and rename so fits with convertVisRangeToNum function
# dives_2018 <- dive %>% 
#   select(dive_id, date, site, visibility) %>%
#   rename(dive_table_id = dive_id, visibility_m = visibility) %>%
#   mutate(year = as.integer(substring(date,1,4)))

##### Pull out info from database
leyte <- read_db("Leyte")

dives <- leyte %>%
  tbl("diveinfo") %>%
  select(dive_table_id, dive_num, date, site, visibility_m) %>%
  collect() %>%
  mutate(year = as.integer(substring(date,1,4))) %>%
  mutate(month = as.integer(substring(date,6,7))) 

# Filter out just Magbangon dives so can assess whether they were north or south Magbangon, so can treat those two sites separately
Mag_dives <- dives %>%
  filter(site == "Magbangon")

# Pull out anems just from Magbangon dives so can use QGIS to assign dive to N or S
Mag_anems <- leyte %>%
  tbl("anemones") %>%
  filter(dive_table_id %in% Mag_dives$dive_table_id) %>%
  select(anem_table_id, dive_table_id, anem_obs, anem_id, old_anem_id) %>%
  collect()

# Link up the Mag_dives with Mag_anems
#Mags <- left_join(Mag_anems, Mag_dives, by="dive_table_id")
Mags <- left_join(Mag_dives, Mag_anems, by="dive_table_id")

# Pull out the first non-NA anem_id seen on each dive (group by year first b/c dive_num gets reset each year)
Mag_repanem <- Mags %>% 
  #mutate(dive_num = as.integer(dive_num)) %>%
  mutate(dive_num =  as.numeric(dive_num)) %>%
  group_by(year, dive_num) %>%
  summarize(rep_anem = na.omit(anem_id)[1])

# Using QGIS map in Phils_GIS_R repository, look to see if the anem_ids representative of each dive are in the north or south part of the site
year_vec <- c(2012,2012,2012,
              2013,2013,2013,2013,
              2014,2014,2014,2014,
              2015,2015,2015,2015,2015,2015,2015,
              2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,
              2018,2018,2018) 
dive_num_vec <- c(11.1,12,13,
                  21,22,25,39,
                  20,21,33,46,
                  13,14,14.5,15,19,30,57,
                  13,34,36,37,38,39,40,41,42,48,
                  8,37,38)
rep_anem_vec <- c(NA,NA,NA,
                  211,213,NA,367,
                  471,509,681,725,
                  2089,2092,932,2114,1113,1356,1294,
                  2000,2407,2411,NA,2419,2426,2434,2441,2456,2516,
                  2956,3182,3190)
rep_anem_site <- c("Magbangon S","Magbangon S","Magbangon N",
                   "Magbangon N","Magbangon S",NA,"Magbangon N",
                   "Magbangon S","Magbangon N","Magbangon N","Magbangon S",
                   "Magbangon N","Magbangon N","Magbangon S","Magbangon N","Magbangon S","Magbangon N","Magbangon N",
                   "Magbangon N","Magbangon N","Magbangon N",NA,"Magbangon N","Magbangon N","Magbangon S","Magbangon S","Magbangon S","Magbangon N",
                   "Magbangon S","Magbangon S","Magbangon S")

# Put the Magbangon info together into a dataframe
Mag_df <- as.data.frame(cbind(year_vec, dive_num_vec, rep_anem_vec, rep_anem_site), stringsAsFactors=FALSE) %>% 
  rename(year = year_vec, dive_num = dive_num_vec, anem_id = rep_anem_vec, site = rep_anem_site)

#for 2012, dive_num 12 and 13 both on 5/10/2012 - put 12 as S and 13 as N b/c Cabatoan site was done first, then 12, then 13 so seems likely MLP moved north
#for 2012, dive_num 11.1 is right after a dive in Cabatoan on the same date (like starts a minute later), so put as S
#in 2013, dive_num 25 has no anems with numbers but also no visibility recorded
#2016, dive_num 37 has NA for anems - only a 9-min dive but has a vis recorded..., dive_table_id is 321, earlier dive in the day is 320
#Mag_info <- left_join(Mag_repanem, Mag_df, by="anem_id")

# Add this info back into the regular data frame
dives <- rename(dives, org_site = site) %>%
  mutate(site = rep(NA, length(dive_table_id))) #rename original site column so can make a new site column

# Go through dives, put in N-S Magbangon site if the year and dive_num matches, otherwise put in the original site
for(i in 1:length(dives$dive_table_id)) {
  test_year = as.character(dives$year[i])
  test_dive_num = as.character(dives$dive_num[i])
  match = 0
  
  for(j in 1:length(Mag_df$year)){
    if(Mag_df$year[j] == test_year & Mag_df$dive_num[j] == test_dive_num) {
      match = j  #check if the year/dive num combo matches one of the Magbangon dives
    }
    
    if(match !=0){
      dives$site[i] = Mag_df$site[match]
    } else {
      dives$site[i] = dives$org_site[i]
    }
  }
}
  
##### Find average vis per year by site
# # Convert ranges (like "2 to 3" entered into the database to numbers (either max, min, or average of range))
# dives_visClean_db <- convertVisRangeToNum(dive_db, vis_convert, 'avgV') %>%
#   select(date, site, year, visibility_m_clean)
# dives_visClean_2018 <- convertVisRangeToNum(dives_2018, vis_convert, 'avgV') %>%
#   select(date, site, year, visibility_m_clean)
# 
# # Combine database and 2018 data
# dives_visClean <- rbind(dives_visClean_db, dives_visClean_2018)


#is there a way to get a list of anems that are in north Magbangon and south Magbangon?

dives_visClean <- convertVisRangeToNum(dives, vis_convert, 'avgV')

# Average by day to get one vis obs per day 
dives_dailyAvg <- dives_visClean %>% 
  group_by(year, site, date) %>% 
  summarize(daily_avg_vis = mean(visibility_m_clean, na.rm = TRUE), nobs = n())

# Find mean/max/min vis each year at each site, averaging across days rather than individual dives
dives_annualAvg <- dives_dailyAvg %>%
  group_by(year, site) %>%
  summarise(avg_vis = mean(daily_avg_vis, na.rm = TRUE), min_vis = min(daily_avg_vis, na.rm = TRUE), max_vis = max(daily_avg_vis, na.rm = TRUE), ndays = n())

dives_visC <- dives_visClean %>% 
  select(visibility_m_clean, year, site)

dives_vis <- rbind(dives_visC, Mag2add)

# Find average vis per year by site, not averaging over day first (just averaging all dives)
dives_Avg <- dives_vis %>%
  group_by(year, site) %>%
  summarise(avg_vis = mean(visibility_m_clean, na.rm = TRUE))

# Replace Cab and Mag vis in 2017 (currently NA b/c no full sampling dives there)
dives_Avg <- dives_Avg %>%
  mutate(avg_vis = ifelse((year == 2017 & site == "Cabatoan"), Cab2017vis, avg_vis)) 
  #mutate(avg_vis = ifelse((year == 2017 & site == "Magbangon N"), Mag2017vis, avg_vis)) %>%
  #mutate(avg_vis = ifelse((year == 2017 & site == "Magbangon S"), Mag2017vis, avg_vis)) 
  #mutate(avg_vis = ifelse((year == 2018 & site == "Magbangon N"), MagN2018vis, avg_vis))

# Create entries for Magbangon N in 2017 and 2018 and Magbangon S in 2017 (when didn't dive) 
# Mag2017and18 <- data.frame(year = integer(3), site = character(3), avg_vis = numeric(3), stringsAsFactors = FALSE)
# Mag2017and18$year[1:3] = c(2017, 2017, 2018)
# Mag2017and18$site[1:3] = c("Magbangon N","Magbangon S","Magbangon N")
# Mag2017and18$avg_vis[1:3] = c(Mag2017vis, Mag2017vis, MagN2018vis)
# Mag2017and18 <- Mag2017and18 %>% mutate(year = as.integer(year))

Mag2add <- data.frame(visibility_m_clean = double(3), year = integer(3), site = character(3), stringsAsFactors = FALSE)
Mag2add$visibility_m_clean[1:3] = c(Mag2017vis, Mag2017vis, MagN2018vis)
Mag2add$year[1:3] = c(2017, 2017, 2018)
Mag2add$site[1:3] = c("Magbangon N","Magbangon S","Magbangon N")

# dives_Avg <- rbind(dives_Avg, Mag2017and18)
# 
# dives_test <- dives_Avg[1,]
# 
# test2 <- rbind(dives_test, Mag2017and18)
# 

dives_annualAvg <- dives_annualAvg %>% 
  mutate(avg_vis = ifelse((year == 2017 & site == "Cabatoan"), Cab2017vis, avg_vis)) %>%
  mutate(min_vis = ifelse((year == 2017 & site == "Cabatoan"), Cab2017vis, min_vis)) %>%
  mutate(max_vis = ifelse((year == 2017 & site == "Cabatoan"), Cab2017vis, max_vis)) #%>%
  # mutate(avg_vis = ifelse((year == 2017 & site == "Magbangon N"), Mag2017vis, avg_vis)) %>%
  # mutate(min_vis = ifelse((year == 2017 & site == "Magbangon N"), Mag2017vis, min_vis)) %>%
  # mutate(max_vis = ifelse((year == 2017 & site == "Magbangon N"), Mag2017vis, max_vis)) %>%
  # mutate(avg_vis = ifelse((year == 2017 & site == "Magbangon S"), Mag2017vis, avg_vis)) %>%
  # mutate(min_vis = ifelse((year == 2017 & site == "Magbangon S"), Mag2017vis, min_vis)) %>%
  # mutate(max_vis = ifelse((year == 2017 & site == "Magbangon S"), Mag2017vis, max_vis)) %>%
  # mutate(avg_vis = ifelse((year == 2018 & site == "Magbangon N"), MagN2018vis, avg_vis)) %>%
  # mutate(min_vis = ifelse((year == 2018 & site == "Magbangon N"), MagN2018vis, min_vis)) %>%
  # mutate(max_vis = ifelse((year == 2018 & site == "Magbangon N"), MagN2018vis, max_vis)) 


  
#################### Plots! ####################
#mean vis for Albuera sites, all dives counted as individual obs, not averaged by date first
pdf(file = "plots/Albuera_AvgVis.pdf")
ggplot(data = (dives_Avg %>% filter(site %in% Albuera_sites)), aes(x=year, y=avg_vis, color=site, shape=site)) +
  geom_point(size=5.0, position=position_dodge(width=0.1)) +
  geom_line() +
  ggtitle('Mean visibility through time at Albuera sites') +
  labs(x='year', y='average vis (m)') +
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017,2018)) +
  theme_bw()
dev.off()

#mean vis for all sites, all dives counted as individual obs, not averaged by date first
pdf(file = "plots/AllSites_AvgVis.pdf")
ggplot(data = dives_Avg, aes(x=year, y=avg_vis, color=site)) +
  geom_point(size=4.0, position=position_dodge(width=0.1)) +
  ggtitle('Mean visibility through time at all sites') +
  labs(x='year', y='average vis (m)') +
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017,2018)) +
  theme_bw()
dev.off()

#mean and min-max range, all dives counted as individual obs (even multiple people on the same dive), averaged by day first
pdf(file = "plots/Albuera_AvgVisMaxMin.pdf")
ggplot(data = (dives_annualAvg %>% filter(site %in% Albuera_sites)), aes(x=year, y=avg_vis, color=site, shape=site)) +
  geom_pointrange(aes(ymin=min_vis, ymax=max_vis), size=1.0, position=position_dodge(width=0.3)) +
  ggtitle('Visibility through time at Albuera sites (mean with min-max range)') +
  labs(x='year', y='average vis (m) with min-max range') +
  scale_x_continuous(breaks=c(2012,2013,2014,2015,2016,2017,2018)) +
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
