# how many anems did we tag each year?
library(dplyr)
source("scripts/field_helpers.R")

# for 2017, what was the highest anem tag_id
# 2938?

leyte <- read_db("Leyte")

seventeen <- leyte %>% 
  tbl("anemones") %>% 
  select(anem_table_id, anem_id) %>%
  collect()

date <- anem_date(seventeen$anem_table_id) %>% 
  filter(grepl("2017", date))
seventeen <- left_join(date, seventeen, by = "anem_table_id")

seventeen <- seventeen %>% 
  filter(!is.na(anem_id)) %>% 
  mutate(anem_id = as.numeric(anem_id)) %>% 
  arrange(anem_id)

k <- max(seventeen$anem_id)

sixt <- leyte %>% 
  tbl("anemones") %>% 
  select(anem_table_id, anem_id) %>%
  collect()

date <- anem_date(sixt$anem_table_id) %>% 
  filter(grepl("2016", date))
sixt <- left_join(date, sixt, by = "anem_table_id")
sixt <- sixt %>% 
  filter(!is.na(anem_id)) %>% 
  mutate(anem_id = as.numeric(anem_id)) %>% 
  arrange(anem_id)

j <- max(sixt$anem_id)

k-j #355 2017

fift <- leyte %>% 
  tbl("anemones") %>% 
  select(anem_table_id, anem_id) %>%
  collect()

date <- anem_date(fift$anem_table_id) %>% 
  filter(grepl("2015", date))
fift <- left_join(date, fift, by = "anem_table_id")
fift <- fift %>% 
  filter(!is.na(anem_id)) %>% 
  mutate(anem_id = as.numeric(anem_id)) %>% 
  arrange(anem_id)

i <- max(fift$anem_id)
l <- 2000
j-i #410 - 2016
i - l # 172 -2015

fourt <- leyte %>% 
  tbl("anemones") %>% 
  select(anem_table_id, anem_id) %>%
  collect()

date <- anem_date(fourt$anem_table_id) %>% 
  filter(grepl("2014", date))
fourt <- left_join(date, fourt, by = "anem_table_id")
fourt <- fourt %>% 
  filter(!is.na(anem_id)) %>% 
  mutate(anem_id = as.numeric(anem_id)) %>% 
  arrange(anem_id)

h <- max(fourt$anem_id)

thirt <- leyte %>% 
  tbl("anemones") %>% 
  select(anem_table_id, anem_id) %>%
  collect()

date <- anem_date(thirt$anem_table_id) %>% 
  filter(grepl("2013", date))
thirt <- left_join(date, thirt, by = "anem_table_id")
thirt <- thirt %>% 
  filter(!is.na(anem_id)) %>% 
  mutate(anem_id = as.numeric(anem_id)) %>% 
  arrange(anem_id)

g <- max(thirt$anem_id)
h-g #360 2014

twel <- leyte %>% 
  tbl("anemones") %>% 
  select(anem_table_id, anem_id) %>%
  collect()

date <- anem_date(twel$anem_table_id) %>% 
  filter(grepl("2012", date))
twel <- left_join(date, twel, by = "anem_table_id")
twel <- twel %>% 
  filter(!is.na(anem_id)) %>% 
  mutate(anem_id = as.numeric(anem_id)) %>% 
  arrange(anem_id)

f <- max(twel$anem_id)

g-f #361

numbs <- c(361, 360, 172, 412, 355)
mean(numbs)
