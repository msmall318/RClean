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
colstomerge <- c(2,7,8,14)
unmerged_data[,"Army"] <- unmerged_data[colstomerge] %>% rowSums()
#Delete the unneeded columns
unmerged_data <- unmerged_data[,-colstomerge]

#Create a toy data set to practice reshaping
unmerged_data <- unmerged_data[,c(1,2,15)]

#Add a variable for the data set (when data was retrieved)
unmerged_data[,"Year"] <- as.Date("1sep2017","%d%b%Y")

#Make Variables Title Case (need stringi) 
#(this may be optional and not needed, but would improve consistency for my example)
colnames(unmerged_data) <- colnames(unmerged_data) %>%
  str_to_title() %>%
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
  gather(., Service, Personnel, c(Navy,Army), factor_key=TRUE)

