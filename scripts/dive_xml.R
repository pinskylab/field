# read dive watch xml into R to generate a csv - based on tutorial https://www.datacamp.com/community/tutorials/r-data-import-tutorial#xml
library(XML)

# parse the xml file
file_list <- list.files(path = "~/Desktop/", pattern = "*Dive*")

dive_log <- data.frame()
for (i in 2:length(file_list)){
  file_name <- file_list[i]
  xmlfile <- xmlTreeParse(file_name)
  # access the top node (I don't know why but the tutorial says to do it)
  topxml <- xmlRoot(xmlfile)
  
  # to put into data frame
  topxml <- xmlSApply(topxml,
    function(x) xmlSApply(x, xmlValue))
  
  xml_df <- data.frame(t(topxml), row.names = NULL)
  dive_log <- rbind(dive_log, xml_df)
}



# double check that R knows what it is (should day xml document)
class(xmlfile)

# testing pull closing

                      
