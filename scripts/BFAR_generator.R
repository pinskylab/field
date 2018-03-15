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
#' Leyte from May 1 to June 16, 2016. The primary objective was to collect
#' tissue samples (fin clips) from individuals of Amphiprion clarkii at each of
#' `r nrow(bfar)` locations. In this season, we sampled `r nrow(bfar)` sites in Albuera and Bay Bay City, Leyte.
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
dive <- read.csv("/Users/macair/Philippines_Docs/field/data/diveinfo.csv", stringsAsFactors = F) %>% 
  filter(grepl("2017", date)) %>% 
  select(dive_table_id, site, municipality)
# in the field, load the anemones that correspond to those dives
anem <- read.csv("/Users/macair/Philippines_Docs/field/data/anemones.csv", stringsAsFactors = F) %>% 
  filter(dive_table_id %in% dive$dive_table_id) %>% 
  select(anem_table_id, dive_table_id) %>% 
  mutate(dive_table_id = as.integer(dive_table_id), 
    anem_table_id = as.integer(anem_table_id))
anem <- left_join(anem, dive, by = "dive_table_id")
rm(dive)
# in the field, load the fish that correspond to those anemones
fish <- read.csv("/Users/macair/Philippines_Docs/field/data/clownfish.csv", stringsAsFactors = F) %>% 
  filter(anem_table_id %in% anem$anem_table_id, 
    !is.na(fin_id), fin_id != "NULL") %>% 
  select(fish_table_id, anem_table_id, fin_id)
fish <- left_join(fish, anem, by = "anem_table_id")
rm(anem)
bfar <- fish %>% 
  group_by(site, municipality) %>%
  summarise(samples = n()) %>% 
  mutate(Province = "Leyte") %>% 
  select(Province, municipality, site, samples)
names(bfar) <- stringr::str_to_title(names(bfar))
bfar$Site_number <- 1:nrow(bfar)
bfar <- bfar[, c(5, 1, 2, 3, 4)]

knitr::kable(bfar)

map_path <- "/Users/macair/Philippines_Docs/field/data/leyte_map.png"
include_graphics(map_path)


#' <!--highlight the line below within the comments starting at rmarkdown and ending at the ) and command-enter to generate the report -->
#' 
#' <!-- rmarkdown::render('/Users/macair/Philippines_Docs/field/scripts/BFAR_generator.R') -->

