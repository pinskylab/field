# test to see which anems at Wangag are ST anems (STHD, ST...)

leyte <- read_db("Leyte")

dive <- leyte %>% 
  tbl("diveinfo") %>% 
  filter(site == "Wangag") %>% 
  collect()
  
anem <- leyte %>% 
  tbl("anemones") %>% 
  collect() %>% 
  filter(dive_table_id %in% dive$dive_table_id, 
    grepl("ST", anem_spp)) 
