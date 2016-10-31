datasets <- c("ICC", "OHCHR", "HRW", "USSTATE", "FIDH", "CRISISGROUP", "AMNESTY")


install.packages("dplyr")
install.packages("readr")
install.packages("stringi")
install.packages("tidyr")
install.packages("doParallel")
install.packages("foreach")
install.packages("parallel")

library(stringi)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(doParallel)
library(foreach)

# create cluster for parallel package
registerDoParallel(makeCluster(detectCores()))




#define function to do 2 level splitting
splitx2 <- function(counts_split, col1, pat1, flat_names, pat2) 
{
  
  library(tidyr)  
  library(stringr)
  
  #isolate the record ids
  x <- as.character(counts_split[,1]) 
  #split the strings in counts along the semi-colon
  y <- str_split(string = counts_split[,2], pattern = pat1)
  
  #create an empty data frame to store the flattened strings
  counts_flat <- data.frame(check.rows = FALSE)
  
  #run a for loop to unlist all the string values and store them along with their record id
  for (i in 1:(length(x))) {
    t <- data.frame(x[i], unlist(y[i]))
    colnames(t) <- c("gkgrecordid", "count_value")
    counts_flat <- bind_rows(counts_flat,t)
  }
  
  #separate counts data along hashes
  counts_all_flat <- separate(data = counts_flat, col = count_value, sep = pat2, into = flat_names)
  
  return(counts_all_flat)
}


#run the split functions over the desired fields in a foreach loop, using parallel to speed it up
#run time is approximately 90 minutes

foreach (i = 1:7) %dopar% { 
  library(stringi)
  library(tidyr)
  library(dplyr)
  library(readr)
  library(stringr)
#read in data
dataname <- datasets[i]
ICC <- read.delim(str_c("HR-GKG.", dataname, ".gkgv2.csv"), stringsAsFactors=FALSE, na.strings = "NA")
write_delim(ICC, path = str_c(getwd(), "/", dataname, ".txt"), delim = "\t", na = "NA", col_names = TRUE)

#convert column names to lowercase for ease of naming
colnames(ICC) <- str_to_lower(colnames(ICC))

#Need to transform the format of the following fields: 
#	counts (semi-colon delimited, hash delimited fields)
# v1themes (semi-colon delimited, at least 275 possibilities)
# v1locations (semi-colon delimited, hash delimited fields)
# v1persons (semi-colon delimited)
#	v1organizations (semi-colon delimited)
#	tone (semi-colon delimited)
#	v2gcam (comma delimited blocks, colon delimited key-value pairs)
#	v2enhanceddates (semi-colon delimited blocks, comma delimited 

# split counts

flat_named <- c("flat_count_counttype", "flat_count_count", "flat_count_objtype", "flat_count_locationtype", "flat_count_locationfullname", "flat_count_locationcountrycode", "flat_count_adm1code", "flat_count_lat", "flat_count_lon", "flat_count_featureid")

counts_data_flat <- splitx2(counts_split = ICC[,c("gkgrecordid","counts")], col1 = "counts", pat1 = ";", flat_names = flat_named, pat2 = "#")
write_delim(counts_data_flat, path = str_c(getwd(), "/", dataname, "counts_data_flat.txt"), delim = "\t", na = "NA", col_names = TRUE)
rm(counts_data_flat)

#themes data split
themes_data_flat <- separate(data = ICC[,c("gkgrecordid", "themes")],col = "themes", into = as.character(c(1:275)), sep = ";")
write_delim(themes_data_flat, path = str_c(getwd(), "/", dataname, "themes_data_flat.txt"), delim = "\t", na = "NA", col_names = TRUE)
rm(themes_data_flat)

# split out locations data
flat_named <- c("clat_location_locationtype", "flat_location_fullname", "flat_location_countrycode", "flat_location_adm1code", "flat_location_lat", "flat_location_lon", "flat_location_featureid")
locations_data_flat <- splitx2(counts_split = ICC[,c("gkgrecordid","locations")], col1 = "locations", pat1 = ";", flat_names = flat_named, pat2 = "#")
write_delim(locations_data_flat, path = str_c(getwd(), "/", dataname, "locations_data_flat.txt"), delim = "\t", na = "NA", col_names = TRUE)
rm(locations_data_flat)

# split out persons
persons_data_flat <- separate(data = ICC[,c("gkgrecordid", "persons")],col = "persons", into = as.character(c(1:275)), sep = ";")
write_delim(persons_data_flat, path = str_c(getwd(), "/", dataname, "persons_data_flat.txt"), delim = "\t", na = "NA", col_names = TRUE)
rm(persons_data_flat)


# split out organizations
organizations_data_flat <- separate(data = ICC[,c("gkgrecordid", "organizations")],col = "organizations", into = as.character(c(1:275)), sep = ";")
write_delim(organizations_data_flat, path = str_c(getwd(), "/", dataname, "organizations_data_flat.txt"), delim = "\t", na = "NA", col_names = TRUE)
rm(organizations_data_flat)


# split out tone
tone_data_flat <- separate(data = ICC[,c("gkgrecordid", "tone")],col = "tone", into = c("flat_tone_avtone", "flat_tone_positive", "flat_tone_negative", "flat_tone_polarity", "flat_tone_activity_ref", "flat_tone_selfgroup", "flat_tone_wordcount"), sep = ",")
write_delim(tone_data_flat, path = str_c(getwd(), "/", dataname, "tone_data_flat.txt"), delim = "\t", na = "NA", col_names = TRUE)
rm(tone_data_flat)

#split out CAM



#split out enhanced dates
flat_named <- c("flat_enhanceddate_resolution", "flat_enhanceddate_month", "flat_enhanceddate_day", "flat_enhanceddate_year", "flat_enhanceddate_offset")
enhanced_dates_flat <- splitx2(counts_split = ICC[,c("gkgrecordid", "enhanceddates")], col1 = "enhanceddates", pat1 = ";", flat_names = flat_named, pat2 = "#")
write_delim(enhanced_dates_flat, path = str_c(getwd(), "/", dataname, "enhanced_dates_flat.txt"), delim = "\t", na = "NA", col_names = TRUE)
rm(enhanced_dates_flat)

}



