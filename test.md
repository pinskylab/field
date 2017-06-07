test
================
Michelle Stuart
6/7/2017

``` r
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(stringr)
excel_file <- ("data/GPSSurveys2017.xlsx")
problem <- data.frame()
```

Import the diveinfo sheet to be checked

``` r
surv <- readxl::read_excel(excel_file, sheet = "diveinfo", col_names=TRUE)
names(surv) <- stringr::str_to_lower(names(surv))
```

Make sure that the sites on the diveinfo sheet are spelled correctly. First, define the list of sites. Then find all of the sites that were spelled correctly in a table called good. Then find all of the sites that are in the excel file, but are not in the good table (therefore not spelled correctly). Get rid of any that are blank lines. Add a note to any type-os found in this section and add them to the final problem list.

``` r
sites <- c("Palanas", "Wangag", "Magbangon", "Cabatoan", "Caridad Cemetery", "Caridad Proper", "Hicgop South", "Sitio Tugas", "Elementary School", "Sitio Lonas", "San Agustin", "Poroc San Flower", "Poroc Rose", "Visca", "Gabas", "Tamakin Dacot", "Haina", "Sitio Baybayon")
good <- dplyr::filter(surv, site %in% sites)
bad <- dplyr::anti_join(surv, good)
bad <- dplyr::filter(bad, !is.na(divenum))
if (nrow(bad) > 0){
  bad$typeo <- "fix site spelling on diveinfo table"
}
(problem <- rbind(problem, bad))
```

    ## # A tibble: 0 × 26
    ## # ... with 26 variables: divenum <dbl>, collectingapcl <dbl>, anemone
    ## #   survey <dbl>, recapture survey <dbl>, date <dttm>, site <chr>,
    ## #   municipality <chr>, gps <dbl>, divers <chr>, starttime <dttm>,
    ## #   endtime <dttm>, duration <dttm>, discontinuous <dbl>,
    ## #   pausestart <dttm>, pauseend <dttm>, weather <chr>,
    ## #   current_knots <chr>, waveheight_cm <dbl>, visibility_m <dbl>,
    ## #   wind_mph <chr>, tide <chr>, topo_m <dbl>, depthtop_m <dbl>,
    ## #   depthbottom_m <dbl>, cover <chr>, notes <chr>

``` r
rm(good, bad)
```

Import the anemone sheet to be checked, remove any blank lines and any duplicated lines

``` r
anem <- readxl::read_excel(excel_file, sheet = "anemones", col_names = TRUE, na = "")
names(anem) <- str_to_lower(names(anem))
anem <- filter(anem, !is.na(id))
anem <- distinct(anem)
```

Look for any anemspp field that is not on our defined list, make a note and add it to the problem list.

``` r
anems <- c("ENQD", "STME", "HECR", "HEMG", "STHD", "HEAR", "MADO", "HEMA", "STGI", "????", "EMPT")
good <- filter(anem, anemspp %in% anems)
bad <- anti_join(anem, good)
bad <- filter(bad, !is.na(anemspp))
if (nrow(bad) > 0){
  bad$typeo <- "fix anem spp on anem table"
}
(problem <- rbind(problem, bad))
```

    ## # A tibble: 0 × 26
    ## # ... with 26 variables: divenum <dbl>, collectingapcl <dbl>, anemone
    ## #   survey <dbl>, recapture survey <dbl>, date <dttm>, site <chr>,
    ## #   municipality <chr>, gps <dbl>, divers <chr>, starttime <dttm>,
    ## #   endtime <dttm>, duration <dttm>, discontinuous <dbl>,
    ## #   pausestart <dttm>, pauseend <dttm>, weather <chr>,
    ## #   current_knots <chr>, waveheight_cm <dbl>, visibility_m <dbl>,
    ## #   wind_mph <chr>, tide <chr>, topo_m <dbl>, depthtop_m <dbl>,
    ## #   depthbottom_m <dbl>, cover <chr>, notes <chr>

``` r
rm(good, bad)
```

Look for any spp field that is not on our defined list, make a note and add it to the problem list.

``` r
fish <- c("APCL", "APOC", "APPE", "APSE", "APFR", "APPO", "APTH", "PRBI", "NA")
good <- filter(anem, spp %in% fish)
bad <- anti_join(anem, good) # wait for the next line before checking bad
bad <- filter(bad, !is.na(spp))
if (nrow(bad) > 0){
  bad$typeo <- "fix fish spp on anem table"
}
(problem <- rbind(problem, bad))
```

    ## # A tibble: 0 × 26
    ## # ... with 26 variables: divenum <dbl>, collectingapcl <dbl>, anemone
    ## #   survey <dbl>, recapture survey <dbl>, date <dttm>, site <chr>,
    ## #   municipality <chr>, gps <dbl>, divers <chr>, starttime <dttm>,
    ## #   endtime <dttm>, duration <dttm>, discontinuous <dbl>,
    ## #   pausestart <dttm>, pauseend <dttm>, weather <chr>,
    ## #   current_knots <chr>, waveheight_cm <dbl>, visibility_m <dbl>,
    ## #   wind_mph <chr>, tide <chr>, topo_m <dbl>, depthtop_m <dbl>,
    ## #   depthbottom_m <dbl>, cover <chr>, notes <chr>

``` r
rm(good, bad)
```

Import the clownfish sheet to be checked, remove any blank lines and any duplicated lines

``` r
clown <- readxl::read_excel(excel_file, sheet = "clownfish", col_names = TRUE, na = "")   
names(clown) <- stringr::str_to_lower(names(clown))
clown <- filter(clown, !is.na(divenum))
```

Look for any spp field that is not on our defined list, make a note and add it to the problem list.

``` r
good <- filter(clown, spp %in% fish)
bad <- anti_join(clown, good)
bad <- filter(bad, !is.na(spp))
if (nrow(bad) > 0){
  bad$typeo <- "fix fish spp on fish table"
}
(problem <- rbind(problem, bad))
```

    ## # A tibble: 0 × 26
    ## # ... with 26 variables: divenum <dbl>, collectingapcl <dbl>, anemone
    ## #   survey <dbl>, recapture survey <dbl>, date <dttm>, site <chr>,
    ## #   municipality <chr>, gps <dbl>, divers <chr>, starttime <dttm>,
    ## #   endtime <dttm>, duration <dttm>, discontinuous <dbl>,
    ## #   pausestart <dttm>, pauseend <dttm>, weather <chr>,
    ## #   current_knots <chr>, waveheight_cm <dbl>, visibility_m <dbl>,
    ## #   wind_mph <chr>, tide <chr>, topo_m <dbl>, depthtop_m <dbl>,
    ## #   depthbottom_m <dbl>, cover <chr>, notes <chr>

Look for any col field that is not on our defined list, make a note and add it to the problem list.

``` r
colors <- c("YE", "O", "YR", "YP", "Y", "W", "WR", "WP", "BW", "B")
good <- filter(clown, color %in% colors)
bad <- anti_join(clown, good)
bad <- filter(bad, !is.na(color))
if (nrow(bad) > 0){
  bad$typeo <- "fix tail color on fish table"
}
(problem <- rbind(problem, bad))
```

    ## # A tibble: 0 × 26
    ## # ... with 26 variables: divenum <dbl>, collectingapcl <dbl>, anemone
    ## #   survey <dbl>, recapture survey <dbl>, date <dttm>, site <chr>,
    ## #   municipality <chr>, gps <dbl>, divers <chr>, starttime <dttm>,
    ## #   endtime <dttm>, duration <dttm>, discontinuous <dbl>,
    ## #   pausestart <dttm>, pauseend <dttm>, weather <chr>,
    ## #   current_knots <chr>, waveheight_cm <dbl>, visibility_m <dbl>,
    ## #   wind_mph <chr>, tide <chr>, topo_m <dbl>, depthtop_m <dbl>,
    ## #   depthbottom_m <dbl>, cover <chr>, notes <chr>

Are there any anems on the clownfish sheet that aren't on the anem survey?

``` r
dive <- clown$divenum
dive <- unique(dive)

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
problem
```

    ## # A tibble: 2 × 14
    ##   divenum collector   gps   spp             obstime anemid  size color
    ## *   <dbl>     <chr> <dbl> <chr>              <dttm>  <chr> <dbl> <chr>
    ## 1      58       MRS     4  APCL 1899-12-30 12:07:00   2531   8.0     O
    ## 2      58       MRS     4  APCL 1899-12-30 12:03:00   2152   1.4    BW
    ## # ... with 6 more variables: recap <chr>, tagid <chr>, finid <dbl>,
    ## #   notes <chr>, img <chr>, typeo <chr>

Import tags from scanner

``` r
infile <- "data/BioTerm.txt" 
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

    ## Warning: 3 parsing failures.
    ##  row col  expected    actual               file
    ##    1  -- 4 columns 1 columns 'data/BioTerm.txt'
    ##  120  -- 4 columns 1 columns 'data/BioTerm.txt'
    ## 1185  -- 4 columns 1 columns 'data/BioTerm.txt'

``` r
pit <- distinct(pit)
pit <- as_tibble(pit) %>% # merge fields into tag number for fish
  unite(scan, city, tagid, sep = "")

pit <- filter(pit, substr(date, 7,8) == "17" & substr(date, 1,2) != "16") # find only this year

# get rid of test tags
pit <- filter(pit, substr(scan,1,3) != "989" & substr(scan,1,3) != "999")
pit <- arrange(pit, date, time)
```

Change pit tags on clownfish data sheet to reflect the whole number and select only the tagid column

``` r
clown <- clown %>% 
  mutate(tagid = stringr::str_replace(tagid, "985_", "985153000")) %>% 
  mutate(tagid = stringr::str_replace(tagid, "986_", "986112100")) %>% 
  mutate(tagid = stringr::str_replace(tagid, "982_", "982000411"))

tagids <- clown %>% select(contains("tag")) %>% filter(!is.na(tagid))
```

What tags are in excel that were not scanned by the scanner (type-os) - should return 0 rows

``` r
anti_join(tagids, pit, by = c("tagid" = "scan"))
```

    ## # A tibble: 0 × 1
    ## # ... with 1 variables: tagid <chr>

What tags are in the scanner that are not in excel (type-os) - should return 0 rows

``` r
anti_join(pit, tagids, by = c("scan" = "tagid"))  
```

    ## # A tibble: 62 × 3
    ##               scan     date     time
    ##              <chr>    <chr>    <chr>
    ## 1  982000411818593 06/05/17 16:09:25
    ## 2  986112100168885 06/05/17 16:05:09
    ## 3  986112100164294 06/05/17 16:01:03
    ## 4  986112100168306 06/05/17 15:49:51
    ## 5  982000411818559 06/05/17 15:45:20
    ## 6  986112100162846 06/05/17 15:41:26
    ## 7  986112100172078 06/05/17 15:29:22
    ## 8  982000411818612 06/05/17 15:24:54
    ## 9  986112100168496 06/05/17 15:18:05
    ## 10 982000411818629 06/05/17 15:13:25
    ## # ... with 52 more rows

``` r
problem
```

    ## # A tibble: 2 × 14
    ##   divenum collector   gps   spp             obstime anemid  size color
    ## *   <dbl>     <chr> <dbl> <chr>              <dttm>  <chr> <dbl> <chr>
    ## 1      58       MRS     4  APCL 1899-12-30 12:07:00   2531   8.0     O
    ## 2      58       MRS     4  APCL 1899-12-30 12:03:00   2152   1.4    BW
    ## # ... with 6 more variables: recap <chr>, tagid <chr>, finid <dbl>,
    ## #   notes <chr>, img <chr>, typeo <chr>
