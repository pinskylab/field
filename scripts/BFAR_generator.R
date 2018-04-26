source("scripts/field_helpers.R")
library(knitr)
library(rmarkdown)

#' ---
#' title: Annual Report for Gratuitous Permit No. FBP-0079-14
#' author: Malin Pinsky
#' date: 2017
#' output:
#'    word_document
#' ---

#'
#' ### Introduction
#' Under the project “Effects of low population density on reef fish
#' connectivity,” we conducted fieldwork and collected anemonefish samples in
#' Leyte from `r min(bfar$date)` to `r max(bfar$date)`. The primary objective was to collect
#' tissue samples (fin clips) from individuals of Amphiprion clarkii at each of
#' `r nrow(bfar)` locations. In this season, we sampled `r nrow(bfar)` sites in Albuera and Baybay City, Leyte.
#'
#' We are now conducting laboratory analysis of the collected specimens. We are
#' using genotyping-by-sequencing methods to genotype each specimen at a number
#' of Single Nucleotide Polymorphisms (SNPs). This will allow us to match
#' parents and offspring and to identify when we recapture the same fish in a
#' different field season. This information will allow us to determine whether
#' small populations are self-persistent or whether they rely on surrounding
#' populations for persistence (network persistence).
#'
#'### Inventory
#' Note: All samples are tissue clips from the caudal fin.
#' <!-- generate a table for the bfar report - in the field, load 2017 dive data from saved database -->
```{r echo = F}
leyte <- read_db("Leyte")
dive <- leyte %>% 
  tbl("diveinfo") %>%
  collect() %>%  
  filter(grepl("2017", date))  %>% 
  select(dive_table_id, site, municipality, date) 

anem <- leyte %>% 
  tbl("anemones") %>% 
  collect() %>% 
  filter(dive_table_id %in% dive$dive_table_id) %>% 
  select(anem_table_id, dive_table_id) %>% 
  mutate(dive_table_id = as.integer(dive_table_id), 
    anem_table_id = as.integer(anem_table_id))

anem <- left_join(anem, dive, by = "dive_table_id")
rm(dive)

fish <- leyte %>% 
  tbl("clownfish") %>% 
  collect() %>% 
  filter(anem_table_id %in% anem$anem_table_id, 
    !is.na(fin_id), fin_id != "NULL") %>% 
  select(fish_table_id, anem_table_id, fin_id)

fish <- left_join(fish, anem, by = "anem_table_id")
rm(anem)

bfar <- fish %>% 
  group_by(site, municipality, date) %>%
  summarise(samples = n()) %>% 
  mutate(Province = "Leyte") %>% 
  select(Province, municipality, site, samples, date)

# format for report
names(bfar) <- stringr::str_to_title(names(bfar))
bfar$Site_number <- 1:nrow(bfar)

# reorder columns
bfar <- bfar[, c("Site_number", "Province", "Municipality", "Site", "Samples")]

kable(bfar)

map_path <- "/Users/macair/Philippines_Docs/field/data/leyte_map.png"
include_graphics(map_path)
```


#' <!--highlight the line below within the comments starting at rmarkdown and ending at the ) and command-enter to generate the report -->
rmarkdown::render('/Users/Whitmore/Desktop/BFAR_generator.R')
