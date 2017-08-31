# field functions



excl <- function(x, y) {
  excl <- readxl::read_excel(excel_file, sheet = x, col_names = T, col_types = y)
  return(excl)
}

comparedives <- function(x) {
  for (i in 1:length(x)){
    fishanem <- dplyr::filter(clown, divenum == x[i]) # for one dive at a time in the fish table
    anemanem <- dplyr::filter(anem, divenum == x[i]) # and the anem table
    good <- dplyr::filter(fishanem, anemid %in% anemanem$anemid) # find all of the anems that are in the anem table
    bad <- dplyr::anti_join(fishanem, good) # and those anems that aren't
    bad <- dplyr::filter(bad, anemid != "????") # except for any with an unknown anem 
    return(bad)
  }
}

from_scanner <- function(pitfile) {
  pit <- readr::read_csv(pitfile, 
    col_names = c("city", "tagid", "date", "time"), 
    col_types = cols(
      city = col_character(),
      tagid = col_character(),
      date = col_character(), # have to specify as string  
      time = col_character() # have to specify as string
    )
  )
  pit <- dplyr::distinct(pit)
  pit <- tibble::as_tibble(pit) %>% # merge fields into tag number for fish
    unite(scan, city, tagid, sep = "")
  return(pit)
}
