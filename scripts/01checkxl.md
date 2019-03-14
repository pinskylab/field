# Check GPSSurveys2017.xls for type-os
Michelle Stuart  
6/7/2017  


Import the diveinfo sheet to be checked

```r
surv <- readxl::read_excel(excel_file, sheet = "diveinfo", col_names=TRUE)
```

```
## Error: 'GPSSurveys2017.xlsx' does not exist in current working directory ('/Users/macair/Documents/leyte-clownfish').
```

```r
names(surv) <- stringr::str_to_lower(names(surv))
```

```
## Error in stri_trans_tolower(string, locale = locale): object 'surv' not found
```

Make sure that the sites on the diveinfo sheet are spelled correctly.  First, define the list of sites. Then find all of the sites that were spelled correctly in a table called good. Then find all of the sites that are in the excel file, but are not in the good table (therefore not spelled correctly).  Get rid of any that are blank lines. Add a note to any type-os found in this section and add them to the final problem list.


```r
sites <- c("Palanas", "Wangag", "Magbangon", "Cabatoan", "Caridad Cemetery", "Caridad Proper", "Hicgop South", "Sitio Tugas", "Elementary School", "Sitio Lonas", "San Agustin", "Poroc San Flower", "Poroc Rose", "Visca", "Gabas", "Tamakin Dacot", "Haina", "Sitio Baybayon")
good <- dplyr::filter(surv, site %in% sites)
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'surv' not found
```

```r
bad <- dplyr::anti_join(surv, good)
```

```
## Error in dplyr::anti_join(surv, good): object 'surv' not found
```

```r
bad <- dplyr::filter(bad, !is.na(divenum))
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'bad' not found
```

```r
if (nrow(bad) > 0){
  bad$typeo <- "fix site spelling on diveinfo table"
}
```

```
## Error in nrow(bad): object 'bad' not found
```

```r
(problem <- rbind(problem, bad))
```

```
## Error in rbind(problem, bad): object 'bad' not found
```

```r
rm(good, bad)
```

```
## Warning in rm(good, bad): object 'good' not found
```

```
## Warning in rm(good, bad): object 'bad' not found
```
Import the anemone sheet to be checked, remove any blank lines and any duplicated lines


```r
anem <- readxl::read_excel(excel_file, sheet = "anemones", col_names = TRUE, na = "")
```

```
## Error: 'GPSSurveys2017.xlsx' does not exist in current working directory ('/Users/macair/Documents/leyte-clownfish').
```

```r
names(anem) <- str_to_lower(names(anem))
```

```
## Error in stri_trans_tolower(string, locale = locale): object 'anem' not found
```

```r
anem <- filter(anem, !is.na(id))
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'anem' not found
```

```r
anem <- distinct(anem)
```

```
## Error in distinct_(.data, .dots = lazyeval::lazy_dots(...), .keep_all = .keep_all): object 'anem' not found
```

Look for any anemspp field that is not on our defined list, make a note and add it to the problem list.

```r
anems <- c("ENQD", "STME", "HECR", "HEMG", "STHD", "HEAR", "MADO", "HEMA", "STGI", "????", "EMPT")
good <- filter(anem, anemspp %in% anems)
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'anem' not found
```

```r
bad <- anti_join(anem, good)
```

```
## Error in anti_join(anem, good): object 'anem' not found
```

```r
bad <- filter(bad, !is.na(anemspp))
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'bad' not found
```

```r
if (nrow(bad) > 0){
  bad$typeo <- "fix anem spp on anem table"
}
```

```
## Error in nrow(bad): object 'bad' not found
```

```r
(problem <- rbind(problem, bad))
```

```
## Error in rbind(problem, bad): object 'bad' not found
```

```r
rm(good, bad)
```

```
## Warning in rm(good, bad): object 'good' not found
```

```
## Warning in rm(good, bad): object 'bad' not found
```
Look for any spp field that is not on our defined list, make a note and add it to the problem list.

```r
fish <- c("APCL", "APOC", "APPE", "APSE", "APFR", "APPO", "APTH", "PRBI", "NA")
good <- filter(anem, spp %in% fish)
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'anem' not found
```

```r
bad <- anti_join(anem, good) # wait for the next line before checking bad
```

```
## Error in anti_join(anem, good): object 'anem' not found
```

```r
bad <- filter(bad, !is.na(spp))
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'bad' not found
```

```r
if (nrow(bad) > 0){
  bad$typeo <- "fix fish spp on anem table"
}
```

```
## Error in nrow(bad): object 'bad' not found
```

```r
(problem <- rbind(problem, bad))
```

```
## Error in rbind(problem, bad): object 'bad' not found
```

```r
rm(good, bad)
```

```
## Warning in rm(good, bad): object 'good' not found
```

```
## Warning in rm(good, bad): object 'bad' not found
```
Import the clownfish sheet to be checked, remove any blank lines and any duplicated lines


```r
clown <- readxl::read_excel(excel_file, sheet = "clownfish", col_names = TRUE, na = "")   
```

```
## Error: 'GPSSurveys2017.xlsx' does not exist in current working directory ('/Users/macair/Documents/leyte-clownfish').
```

```r
names(clown) <- stringr::str_to_lower(names(clown))
```

```
## Error in stri_trans_tolower(string, locale = locale): object 'clown' not found
```

```r
clown <- filter(clown, !is.na(divenum))
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'clown' not found
```

Look for any spp field that is not on our defined list, make a note and add it to the problem list.

```r
good <- filter(clown, spp %in% fish)
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'clown' not found
```

```r
bad <- anti_join(clown, good)
```

```
## Error in anti_join(clown, good): object 'clown' not found
```

```r
bad <- filter(bad, !is.na(spp))
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'bad' not found
```

```r
if (nrow(bad) > 0){
  bad$typeo <- "fix fish spp on fish table"
}
```

```
## Error in nrow(bad): object 'bad' not found
```

```r
(problem <- rbind(problem, bad))
```

```
## Error in rbind(problem, bad): object 'bad' not found
```
Look for any col field that is not on our defined list, make a note and add it to the problem list.

```r
colors <- c("YE", "O", "YR", "YP", "Y", "W", "WR", "WP", "BW", "B")
good <- filter(clown, color %in% colors)
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'clown' not found
```

```r
bad <- anti_join(clown, good)
```

```
## Error in anti_join(clown, good): object 'clown' not found
```

```r
bad <- filter(bad, !is.na(color))
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'bad' not found
```

```r
if (nrow(bad) > 0){
  bad$typeo <- "fix tail color on fish table"
}
```

```
## Error in nrow(bad): object 'bad' not found
```

```r
(problem <- rbind(problem, bad))
```

```
## Error in rbind(problem, bad): object 'bad' not found
```

Are there any anems on the clownfish sheet that aren't on the anem survey?


```r
dive <- clown$divenum
```

```
## Error in eval(expr, envir, enclos): object 'clown' not found
```

```r
dive <- unique(dive)
```

```
## Error in unique(dive): object 'dive' not found
```

```r
for (i in 1:length(dive)){
  fishanem <- filter(clown, divenum == dive[i]) # for one dive at a time in the fish table
  anemanem <- filter(anem, divenum == dive[i]) # and the anem table
  good <- filter(fishanem, anemid %in% anemanem$anemid) # find all of the anems that are in the anem table
  bad <- anti_join(fishanem, good) # and those anems that aren't
  bad <- filter(bad, anemid != "????") # except for any with an unknown anem 
  if (nrow(bad) > 0){
    bad$typeo <- "anemid in fish table doesn't match anem data table"
  }
  problem <- rbind(problem, bad)
}
```

```
## Error in eval(expr, envir, enclos): object 'dive' not found
```

```r
problem
```

```
## data frame with 0 columns and 0 rows
```

Import tags from scanner

```r
infile <- "BioTerm.txt" 
pit <- read_csv(infile, 
  col_names = c("city", "tagid", "date", "time"), 
  col_types = cols(
    city = col_character(),
    tagid = col_character(),
    date = col_character(), # have to specify as string  
    time = col_character() # have to specify as string
  )
)
```

```
## Error: 'BioTerm.txt' does not exist in current working directory ('/Users/macair/Documents/leyte-clownfish').
```

```r
pit <- distinct(pit)
```

```
## Error in distinct_(.data, .dots = lazyeval::lazy_dots(...), .keep_all = .keep_all): object 'pit' not found
```

```r
pit <- as_tibble(pit) %>% # merge fields into tag number for fish
  unite(scan, city, tagid, sep = "")
```

```
## Error in as_tibble(pit): object 'pit' not found
```

```r
pit <- filter(pit, substr(date, 7,8) == "17" & substr(date, 1,2) != "16") # find only this year
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'pit' not found
```

```r
# get rid of test tags
pit <- filter(pit, substr(scan,1,3) != "989" & substr(scan,1,3) != "999")
```

```
## Error in filter_(.data, .dots = lazyeval::lazy_dots(...)): object 'pit' not found
```

```r
pit <- arrange(pit, date, time)
```

```
## Error in arrange_(.data, .dots = lazyeval::lazy_dots(...)): object 'pit' not found
```

Change pit tags on clownfish data sheet to reflect the whole number and select only the tagid column

```r
clown <- clown %>% 
  mutate(tagid = stringr::str_replace(tagid, "985_", "985153000")) %>% 
  mutate(tagid = stringr::str_replace(tagid, "986_", "986112100")) %>% 
  mutate(tagid = stringr::str_replace(tagid, "982_", "982000411"))
```

```
## Error in eval(expr, envir, enclos): object 'clown' not found
```

```r
tagids <- clown %>% select(contains("tag")) %>% filter(!is.na(tagid))
```

```
## Error in eval(expr, envir, enclos): object 'clown' not found
```

```r
tagids <- distinct(tagids)
```

```
## Error in distinct_(.data, .dots = lazyeval::lazy_dots(...), .keep_all = .keep_all): object 'tagids' not found
```

What tags are in excel that were not scanned by the scanner (type-os) - should return 0 rows

```r
anti_join(tagids, pit, by = c("tagid" = "scan"))
```

```
## Error in anti_join(tagids, pit, by = c(tagid = "scan")): object 'tagids' not found
```

What tags are in the scanner that are not in excel (type-os) - should return 0 rows

```r
anti_join(pit, tagids, by = c("scan" = "tagid"))  
```

```
## Error in anti_join(pit, tagids, by = c(scan = "tagid")): object 'pit' not found
```

```r
problem
```

```
## data frame with 0 columns and 0 rows
```




