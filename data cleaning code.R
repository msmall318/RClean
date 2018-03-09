#Read in the data
unmerged_data <- readr::read_csv("clean_data.csv")

unmerged_data[1,] %>% print.AsIs()
unmerged_data[1,14] <- "ArmyCiv"
unmerged_data[1,15] <- "NavyCiv"
unmerged_data[1,16] <- "MarineCorpsCiv"
unmerged_data[1,17] <- "AirForceCiv"

unmerged_data[1,1] <- "Country"

#Renamed Columns
colnames(unmerged_data) <- unmerged_data[1,]
#Delete Unneeded Row
unmerged_data <- unmerged_data[-1,]

#Make Variables Numeric
cols <- 2:18
unmerged_data[,cols] %<>% lapply(function(x) as.numeric(as.character(x)))

#Sum the observations
colstomerge_army <- colnames(unmerged_data) %>% grepl("army",.,ignore.case=TRUE)
colstomerge_navy <- colnames(unmerged_data) %>% grepl("navy",.,ignore.case=TRUE)
colstomerge_marine <- colnames(unmerged_data) %>% grepl("marine",.,ignore.case=TRUE)
colstomerge_airforce <- colnames(unmerged_data) %>% grepl("air",.,ignore.case=TRUE)

#Create temporary data frame
temp <- data.frame(matrix(ncol = 0, nrow = nrow(unmerged_data)))
temp[,"Army"] <- unmerged_data[colstomerge_army] %>% rowSums()
temp[,"Navy"] <- unmerged_data[colstomerge_navy] %>% rowSums()
temp[,"MarineCorps"] <- unmerged_data[colstomerge_marine] %>% rowSums()
temp[,"AirForce"] <- unmerged_data[colstomerge_airforce] %>% rowSums()
#Delete the unneeded columns
unmerged_data <- cbind(unmerged_data[,1],temp)

#Add a variable for the data set (when data was retrieved)
unmerged_data[,"Year"] <- as.Date("1sep2017","%d%b%Y")

#Make Variables Title Case (need stringi) 
#(this may be optional and not needed, but would improve consistency for my example)
colnames(unmerged_data) <- colnames(unmerged_data) %>%
  stri_replace_all_fixed(., " ", "") %>% 
  stri_replace_all_charclass(., "\\p{WHITE_SPACE}", "")

#Make my first variable consistent (Consider comparing to iso format and create
#a specialized version of the RClean specific for DMDC/Country Cleaning -- 
#make sure country names match country name in question and attempt to compare
#also allow selection from a list or create new or ignore)
unmerged_data$Country <- unmerged_data$Country %>% 
  str_to_title() %>% str_trim()

#Time to tidy (library tidyr)
unmerged_data <- unmerged_data %>% 
  gather(., Service, Personnel, c(Navy,Army,MarineCorps,AirForce), factor_key=TRUE)

rm(list = ls(pattern = "col"),temp)


