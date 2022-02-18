# ------------------------------------------------------------------------------
# Setting up packages
# ------------------------------------------------------------------------------
library(readxl)
library(data.table)
library(dplyr)
library(purrr)

custom_list <- list(
  bloomberg_data = 1,
  refinitiv_data = 1,
  sustainalytics_data = 1,
  # Choose one of the following supported identifier: ISIN
  identifier = "ISIN"
)


# ------------------------------------------------------------------------------
# Main function
# ------------------------------------------------------------------------------
merge_data <- function(cl){
  
}
merge_data(custom_list)

# ------------------------------------------------------------------------------
# Prepare Bloomberg data
# ------------------------------------------------------------------------------
p_bloomberg <- function(){
# get file list with all xlsx files beginning with Bloomberg
bloomberg.list <- list.files(pattern='^(?i)bloomberg.*.xlsx', recursive = TRUE)
bloomberg.df.list <- lapply(bloomberg.list, read_excel)

# bind files of different regions into one bloomberg data table
bloomberg.df <- rbindlist(bloomberg.df.list)

# clean column names
colnames(bloomberg.df) <- gsub("^#|\\(\\)$", "", colnames(bloomberg.df))

return(bloomberg.df)
}
# ------------------------------------------------------------------------------
# Prepare Refinitiv data
# ------------------------------------------------------------------------------
p_refinitiv <- function(){
# get file list with all xlsx files beginning with Refinitiv
refinitiv.list <- list.files(pattern='^(?i)refinitiv.*.xlsx', recursive = TRUE)
refinitiv.df.list <- lapply(refinitiv.list, read_excel)

# bind files of different regions into one refinitiv data table
refinitiv.df <- rbindlist(refinitiv.df.list)
return(refinitiv.df)
}
# ------------------------------------------------------------------------------
# Prepare Sustainalytics data
# ------------------------------------------------------------------------------
p_sustainalytics <- function(){
# get file list with all csv files beginning with Sustainalytics
file.list2 <- list.files(pattern='^(?i)Sustainalytics.*.csv', recursive = TRUE)

# read files as data.table
sustain_focus <- fread(file.list2[1])
# remove from Sustainlytics focus data where EntityId row is NA
sustain_focus <- sustain_focus[!is.na(sustain_focus$EntityId), ]
reference <- fread(file.list2[2])
reference <- reference[!duplicated(reference[, "EntityId"], fromLast=T), ]
descriptions <- fread(file.list2[3])
descriptions$Description <- gsub("(.*)\\s\\(fieldid_[0-9]+\\)", "\\1", descriptions$Description)
descriptions <- descriptions[!duplicated(descriptions)]

# replace variable id with variable name/description
for (i in 1:ncol(sustain_focus)) { 
  if (colnames(sustain_focus)[i] %in% c("EntityId", "FieldDate", "file_date")){next}
  colnames(sustain_focus)[i] = descriptions$Description[which(descriptions$`Variable Name`==colnames(sustain_focus)[i])]
} 
sustain_focus[, 4 := NULL]

#colnames(sustain_focus) <- lapply(colnames(sustain_focus), function(x) {
#  x <- descriptions$Description[which(descriptions$`Variable Name`==x)]
#  return(x)
#  })


# keeping only most recent data for the same company
sustain_focus <- sustain_focus[!duplicated(sustain_focus[, "EntityId"], fromLast=F), ]

# match company information from reference
sustain_focus <- left_join(sustain_focus, 
                           reference[, c("EntityId", "ISIN", "EntityName", "Subindustry", "Country")], 
                           by="EntityId")
}
# ------------------------------------------------------------------------------
# Merging
# ------------------------------------------------------------------------------
merge <- function(){
# put data sets to be merged into a list
merge_list <- list(bloomberg, refinitiv, sustain_focus)

# convert all data into data.frames
merge_list <- lapply(merge_list, as.data.frame)
# drop some unwanted columns as specified
drop_columns <- c("ID", "Identifier (RIC)", "FieldDate", "file_date", "EntityId")
merge_list <- lapply(merge_list, function(x) x %>% select(-any_of(drop_columns)))

# merge data by ISIN using purrr::reduce
merged_data_raw <- merge_list %>% reduce(full_join, by="ISIN")

# clean up columns of merged data
merged_data <- merged_data_raw %>% 
  mutate(company_name = coalesce(`EntityName`, `Company Name`, Name)) %>%
  mutate(sub_industry = coalesce(Sub_Industry, Subindustry)) %>%
  mutate(country = coalesce(cntry_of_incorporation, `Country of Headquarters`, 
                            Country, `HQ Address Country ISO`)) %>%
  select(-c(`EntityName`, `Company Name`, Name, Sub_Industry, Subindustry,
            cntry_of_incorporation, `Country of Headquarters`, Country, `HQ Address Country ISO`)) %>%
  select(company_name, ISIN, sub_industry, country, everything())

# save merged data to source file location
existing_name <- last(list.files(pattern='^merged.*.csv', recursive = FALSE))
file_name <- paste0(gsub("(.*).csv", "\\1", existing_name), "_copy.csv")
write.csv(merged_data, file_name, na = "", row.names = FALSE)
}