# -------------------------------------------------------------------
#   Field functions - functions used while working in the Philippines
# -------------------------------------------------------------------
library(dplyr)
# excl ####
#'is a function to read any excel sheet "x", into R with the defined col_types "y", if you don't want to define you col_types, you must enter NULL. 
#' @export
#' @name excl
#' @author Michelle Stuart
#' @param x = sheet name in quotes
#' @param y = col_types in a list 
#' @examples 
#' surv <- excl("diveinfo", NULL)
#' 
#' clowncol <- c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text")
#' clown <- excl("clownfish", clowncol)

excl <- function(x, y) {
  excl <- readxl::read_excel(excel_file, sheet = x, col_names = T, col_types = y)
  return(excl)
}


# comparedives #### 
#' is a function that compares the anem ids on the clownfish sheet to the anem ids on the anemone sheet by dive number for the 2017 field season.
#' @export
#' @name compare_dives
#' @author Michelle Stuart
#' @param x = clowndive - a list of dive numbers found on the clownfish sheet
#' @examples 
#' bad <- comparedives(clowndive)

compare_dives <- function(x) {
  for (i in 1:length(x)){
    fishanem <- filter(clown, divenum == x[i]) # for one dive at a time in the fish table
    anemanem <- filter(anem, divenum == x[i]) # and the anem table
    good <- filter(fishanem, anemid %in% anemanem$anemid) # find all of the anems that are in the anem table
    bad <- anti_join(fishanem, good) # and those anems that aren't
    bad <- filter(bad, anemid != "????") # except for any with an unknown anem 
    return(bad)
  }
}


# from_scanner #### 
#' is a function that reads the pit scanner text file into R.
#' @export
#' @name from_scanner
#' @author Michelle Stuart
#' @param x = pitfile (the path to the pit scanner file)
#' @examples 
#' pit <- from_scanner(pitfile)

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
  pit <- distinct(pit)
  pit <- tibble::as_tibble(pit) %>% # merge fields into tag number for fish
    unite(scan, city, tagid, sep = "")
  return(pit)
}

# name_gps ####
#' is a function that names all of our gps units.  Sure, 5 isn't that many right now but as they fail and we buy more it will help to have a function that does this for us. 
#' @export
#' @name name_gps
#' @author Michelle Stuart
#' @param 
#' @examples 
#' gps <- name_gps()
name_gps <- function() {
  gps <- c()
  for (i in 1:5){ # because we have 5 gps units
    x <- paste("gps", i, sep = "")
    gps <- c(gps, x)
  }
  return(gps)
}


# used_gps ####
#' is a function that checks the folders of all of the gps units and determines which contain data. 
#' @export
#' @name used_gps
#' @author Michelle Stuart
#' @param 
#' @examples 
#' gps <- used_gps(gps)

used_gps <- function(gps) {
  for (i in 1:length(gps)){
    test <- list.files(path = paste("data/", gps[i], sep = ""), pattern = "*Track*")
    if (length(test) == 0){
      gps[[i]] <- NA
    }
  }
  gps <- gps[!is.na(gps)]
  return(gps)
}

# prep_gpx ####
#' is a function that reads a gpx file and parses out the useable data. 
#' @export
#' @name prep_gpx
#' @author Michelle Stuart
#' @param x = gps - list of gps units
#' @param y = files - list of track files
#' @examples 
#' dat <- parse_gpx(gps, files)

prep_gpx <- function(x, y) {
  infile <- readGPXGarmin(paste("data/", x[l], "/", y[i], sep="")) # list of 2 itmes, header and data
  header <<- infile$header
  dat <- infile$data
  dat$time <- ymd_hms(dat$time)
  instarttime <<-  dat$time[1] # start time for this GPX track
  inendtime <<- dat$time[nrow(dat)] # end time for this GPX track
  dat$elev <- 0 # change elevation to zero
  dat$unit <- substr(x[l],4,4)
  return(dat)
}



# index_line ####
#' a function that reads through each line of an index and writes an output file in gpx format
#' @export
#' @name index_line
#' @author Michelle Stuart
#' @param x = one line of an index
#' @param y = one track file
#' @param dat = a table of gpx data
#' @examples 
#' index_line(inds[j, ], files[i])

index_line <- function(x, y, dat){
  # if no pause
  if(is.na(x$paust)){
    # find the GPX points that fit within the survey
    track <- dat %>%
      filter(time >= x$start & time <= x$end) %>% 
      mutate(
        lat = as.character(lat),
        lon = as.character(lon),
        elev = as.character(elev),
        time = as.character(time)
      )
    writeGPX(filename = str_c("data/gpx_trimmed/GPS", track$unit[1], "_", x$divenum, "_", y, sep=""), outfile = track)
  }
  # account for a pause if need be
  if(!is.na(x$paust)){
    track1 <- dat %>%
      filter(time >= x$start & time <= x$paust) %>% 
      mutate(
        lat = as.character(lat),
        lon = as.character(lon),
        elev = as.character(elev),
        time = as.character(time)
      )
    track2 <- dat %>%
      filter(time >= x$pausend & time <= x$end) %>% 
      mutate(
        lat = as.character(lat),
        lon = as.character(lon),
        elev = as.character(elev),
        time = as.character(time)
      )
    
    # write as two tracks
    writeGPX(filename = paste("data/gpx_trimmed/GPS", track1$unit[1], "_",x$divenum, "_", y, '_1.gpx', sep=''), outfile = track1) 
    writeGPX(filename = paste("data/gpx_trimmed/GPS", track2$unit[1], "_",x$divenum, "_", y, '_2.gpx', sep=''), outfile = track2)
  }
  
}

# ####
