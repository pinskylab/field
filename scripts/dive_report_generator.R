#' ---
#' title: Dive Report
#' author: Michelle Stuart
#' date: 2018
#' output:
#'  pdf_document
#' ---

#'
#' ### Introduction
#' A document summarizing dives conducted during 2018 field season in the Philippines serving as a field notebook.


#' <!-- pull a list of dive numbers, sites, and dates from the diveinfo spreadsheet -->
```{r echo = F}
library(googlesheets)
library(dplyr)
library(stringr)
# if data is accessible in google sheets: 
# gs_auth(new_user = TRUE) # run this if having authorization problems
mykey <- '1symhfmpQYH8k9dAvp8yV_j_wCTpIT8gO9No4s2OIQXo' # access the file
entry <-gs_key(mykey)
clown <-gs_read(entry, ws='clownfish')
dive <- gs_read(entry, ws="diveinfo")

dive_list <- dive %>% 
  select(dive_num, site, date) %>% 
  distinct()

#' <!--add fish capture info -->
# fish observation info from anem survey
fish <- clown
names(fish) <- str_to_lower(names(fish))
fish <- filter(fish, !is.na(fish_spp)) %>% 
  distinct()

# get dive info
site <- dive
names(site) <- stringr::str_to_lower(names(site))
site <- site %>% 
  filter(!is.na(dive_num)) %>% 
  select(dive_num, site) %>% 
  distinct()

# fish processing info from clownfish survey
samp <- clown
names(samp) <- stringr::str_to_lower(names(samp))
samp <- filter(samp, !is.na(dive_num))

# make a table of observed APCL
divetot <- fish %>%
  group_by(dive_num) %>% 
  filter(fish_spp == "APCL") %>% 
  summarise(observed = n())

# add the site to the table
divetot <- left_join(divetot, site, by = "dive_num")

# how many tissue samples were collected?  
tissue <- samp %>% 
  filter(!is.na(fin_id)) %>% 
  group_by(dive_num) %>% 
  summarize(fins=n())
divetot <- left_join(divetot, tissue, by = "dive_num")

# how many fish are captured (clipped + recaptures) - this will miss fish that were captured but escaped before getting a clip...
captured <- samp %>% 
  filter(recap == "Y" | !is.na(fin_id)) %>% # select fish that either already had a tag or were given a fin clip (only counts fish that were both Y and clipped once)
  group_by(dive_num) %>%
  summarise(captured = n())

divetot <- left_join(divetot, captured, by = "dive_num")


# how many fish were recaptures?
recap <- samp %>% 
  filter(recap == "Y") %>% 
  group_by(dive_num) %>% 
  summarise(recap = n())

divetot <- left_join(divetot, recap, by = "dive_num") 

# what about the (few) fish that are recaptures but also get a fin clip?
clipped_recaps <- samp %>%
  filter(recap == "Y" & !is.na(fin_id)) %>%
  group_by(dive_num) %>%
  summarise(clipped_recaps = n())
divetot <- left_join(divetot, clipped_recaps, by = "dive_num")



divetot <- divetot %>% 
  select(dive_num, site, observed, captured, fins, recap, clipped_recaps) %>% 
  distinct()



#' <!--create dynamic text based on dive data -->
#' This is useful if you want to generate lots of text without writing it manually.  

for (i in 1:nrow(dive_list)) {
  cat("Dive", dive_list$dive_num[i], "at", dive_list$site[i], "on", as.character(dive_list$date[i]), "\n")
  cat('\n')
}

#' 




#' <!--highlight the line below within the comments starting at rmarkdown and ending at the ) and command-enter to generate the report -->
#' 
#' <!-- rmarkdown::render('/Users/macair/Philippines_Docs/field/scripts/BFAR_generator.R') -->

