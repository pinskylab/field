# pull the data from google and keep it loaded in the environment so you only have to pull once per data processing session


# if data is accessible in google sheets:
library(googlesheets)
library(stringr)
# gs_auth(new_user = TRUE) # run this if having authorization problems
mykey <- '1symhfmpQYH8k9dAvp8yV_j_wCTpIT8gO9No4s2OIQXo' # access the file
entry <-gs_key(mykey)
clown <-gs_read(entry, ws='clownfish')
dive <- gs_read(entry, ws="diveinfo")

# save data in case network connection is lost
clownfilename <- str_c("data/clown_", Sys.time(), ".Rdata", sep = "")
divefilename <- str_c("data/dive_", Sys.time(), ".Rdata", sep = "")
save(clown, file = clownfilename)
save(dive, file = divefilename)

# # if the network connection is lost
# load("data/clown_2018-03-22 06:00:16.Rdata")
# load("data/dive_2018-03-22 06:11:58.Rdata")
