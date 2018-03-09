---
output: html_document
---



## RCleaner

This document serves as a guide to utilizing the RCleaner gadget from the RClean package.

## Instructions Tab

Instructions on the gadget itself

Every Tab can either finish and close or cancel all actions using buttons at the bottom of the screen

 - Finish and close - close session and output data to R console
 - Cancel - close session, with no action taken on the data

## Data Manipulation Tab

Data imported is visually displayed.  The following actions are available:

- Delete Rows - deletes highlighted rows from the DF
- Delete Columns - deletes highlighted columns from the DF
- Mean Center - mean center highlighted columns in the DF, where non-numeric data is ignored
- Scale - scale highlight columns in the DF, where non-numeric data is ignored

## Rename Columns Tab

Data imported is visually displayed.  The following actions are available:

 - Use the drop down bow to select what variable to rename
 - Use the text input box to type in the new variable name
 - Save - save the text input into the text box as a new variable name
 
## Encode Dummy Tab
 
Allows a user to create dummy variables from a specified variable in the dataset

  - Variable Drop Down allows users to select variable
  - Encode Dummy creates a dummy variable with levels equal to the number of levels in the selected column
  
## Download/Save Tab

This tab allows users to download (open in excel) or save the current working dataframe displayed on the screen.  

  - Download - presents browser options to open or save existing data.  
  
## Reset Tab

The sole function of this tab is to allow a user to re-initialize the gadget environment with a clean dataset.  Previous changes to the dataset ARE NOT SAVED.    

## NOTES
- NOTE: Search function will only filter data and does not apply the filter if Finish and close is clicked
- NOTE: You can encode any variable as dummy/indicator columns.  Use of this function requires understanding of indicator variables.
- NOTE: RCleaner gadget does not automatically save data to your working environment.  Assignment must be made prior to opening this gadget (i.e. my_data <- RCleaner(iris))

