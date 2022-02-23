# Feb 23, 2022
# to-do
# support KLD social ratings
# update sustainalytics cleaning function

# ------------------------------------------------------------------------------
# Setting up packages
# ------------------------------------------------------------------------------
install.packages("pacman")
pacman::p_load("readxl","data.table","purrr","stringr","dplyr")

# ------------------------------------------------------------------------------
# Check data validity
# ------------------------------------------------------------------------------
# works for bloomberg and refinitiv data
check_data <- function(cl, data.list){
  if (is_empty(data.list)) {stop("Files no found. Check naming of files.")}
  
  lead_col_names <- colnames(data.list[[1]])
  
  # check if all dataframes in list have the same column names
  lapply(data.list, function(x) if (!identical(lead_col_names, colnames(x))) 
    {stop("Trying to rbind data with different columns names. Check data columns!")} )
  
  # check if identifier exists
  if (!(TRUE %in% str_detect(lead_col_names, paste0("(?i)", cl$identifier)))){
    stop(paste("Identifier", cl$identifier, "not found"))
  }
  
  data.df <- rbindlist(data.list)
  
  return(data.df)
}

# ------------------------------------------------------------------------------
# Prepare Bloomberg data
# ------------------------------------------------------------------------------
p_bloomberg <- function(cl){
  # get file list with all xlsx files beginning with Bloomberg
  bloomberg.list <- list.files(pattern='^(?i)bloomberg.*.xlsx', recursive = TRUE)
  bloomberg.df.list <- lapply(bloomberg.list, read_excel)
  
  # check validity of files, and if valid, 
  # bind files of different regions into one bloomberg data table
  bloomberg.df <- check_data(cl, bloomberg.df.list)
  
  # clean column names
  colnames(bloomberg.df) <- gsub("^#|\\(\\)$", "", colnames(bloomberg.df))
  
  return(bloomberg.df)
}

# ------------------------------------------------------------------------------
# Prepare Refinitiv data
# ------------------------------------------------------------------------------
p_refinitiv <- function(cl){
  # get file list with all xlsx files beginning with Refinitiv
  refinitiv.list <- list.files(pattern='^(?i)refinitiv.*.xlsx', recursive = TRUE)
  refinitiv.df.list <- lapply(refinitiv.list, read_excel)
  
  # check validity of files, and if valid, 
  # bind files of different regions into one refinitiv data table
  refinitiv.df <- check_data(cl, refinitiv.df.list)

  return(refinitiv.df)
}

# ------------------------------------------------------------------------------
# Prepare Sustainalytics data
# ------------------------------------------------------------------------------
p_sustainalytics <- function(cl){
  # get file list with all csv files beginning with Sustainalytics
  sustain.list <- list.files(pattern='^(?i)sustainalytics.*.csv', recursive = TRUE)
  
  if (is_empty(sustain.list)) {stop("Files no found. Check naming of files.")}
    
  # read files as data.table
  sustain_focus <- fread(sustain.list[1])
  # remove from Sustainlytics focus data where EntityId row is NA
  sustain_focus <- sustain_focus[!is.na(sustain_focus$EntityId), ]
  reference <- fread(sustain.list[2])
  reference <- reference[!duplicated(reference[, "EntityId"], fromLast=T), ]
  descriptions <- fread(sustain.list[3])
  descriptions$Description <- gsub("(.*)\\s\\(fieldid_[0-9]+\\)", "\\1", descriptions$Description)
  descriptions <- descriptions[!duplicated(descriptions)]
  
  # replace variable id with variable name/description
  for (i in 1:ncol(sustain_focus)) { 
    if (colnames(sustain_focus)[i] %in% c("EntityId", "FieldDate", "file_date")){next}
    colnames(sustain_focus)[i] = descriptions$Description[which(descriptions$`Variable Name`==colnames(sustain_focus)[i])]
  } 
  sustain_focus[, 4 := NULL]
  
  # work this out
  #colnames(sustain_focus) <- lapply(colnames(sustain_focus), function(x) {
  #  x <- descriptions$Description[which(descriptions$`Variable Name`==x)]
  #  return(x)
  #  })
  
  
  # keeping only most recent data for the same company
  sustain_focus <- sustain_focus[!duplicated(sustain_focus[, "EntityId"], fromLast=F), ]
  
  # match company information from reference
  cols <- c("EntityId", cl$identifier, "EntityName", "Subindustry", "Country")
  sustain_focus <- left_join(sustain_focus, reference[, ..cols], by="EntityId")
  return(sustain_focus)
}

# ------------------------------------------------------------------------------
# Main merge function
# ------------------------------------------------------------------------------
merge_data <- function(cl, co){
  cl$identifier <- toupper(cl$identifier)
  
  # put all data required (flagged as 1) together in a list of data.tables
  merge_list <- list()
  # note that seq_along(cl[1:3]) produces 1 2 3, so definitions of y and n are needed
  # if data flags do not appear first in custom list; cl[1:3] hard coded
  merge_list <- lapply(seq_along(cl[1:3]), function(y, n, i){
    if (y[i]==1){
      print(paste("Cleaning", n[i], "data..."))
      append(merge_list, get(paste0("p_", n[i]))(cl))
      }
  }, y=cl[1:3], n=names(cl[1:3]))
  
  # remove any NULL values in list
  merge_list <- merge_list[lengths(merge_list) != 0]
  
  # convert all data into data.frames
  merge_list <- lapply(merge_list, as.data.frame, check.names=FALSE)
  # drop some unwanted columns as specified
  drop_columns <- c("ID", "Identifier (RIC)", "FieldDate", "file_date", "EntityId")
  merge_list <- lapply(merge_list, function(x) x %>% select(-any_of(drop_columns)))
  
  # merge data by identifier using purrr::reduce
  merged_data <- merge_list %>% reduce(full_join, by=cl$identifier)
 
  # coalesce specified items
  for (i in colnames(co)){
    merged_data <- merged_data %>%
      mutate(!!i := coalesce(!!!select(., any_of(as.vector(na.omit(co[, i]))))))
  }
  
  # clean up columns
  merged_data <- merged_data %>% 
    select(-any_of(as.vector(na.omit(as.matrix(co))))) %>%
    select(cl$identifier, colnames(co), everything())
  
  # save merged data to source file location
  # naming by counting number of copies
  #existing_count <- length(list.files(pattern='^merged_ESG_data.*.csv', recursive = FALSE))
  #if (existing_count == 0){file_name <- "merged_ESG_data.csv"
  #} else if (existing_count == 1) {file_name <- "merged_ESG_data copy.csv"
  #} else {file_name <- paste0("merged_ESG_data copy ", existing_count, ".csv")}
  
  # naming by pasting time
  file_name <- paste0("merged_ESG_data ", format(Sys.time(), "%Y-%m-%d %I.%M%p"), ".csv")
  write.csv(merged_data, file_name, na = "", row.names = FALSE)
  print(paste("Check source file location for merged data:", file_name))
  
}

# -------------------------- development --------------------------

cl <- list(
  bloomberg = 1,
  refinitiv = 1,
  sustainalytics = 1,
  # ISIN recommended
  identifier = "ISIN"
)

co <- data.frame(
  row.names = c("bloomberg","refinitiv","sustainalytics"),
  company_name = c("Name","Company Name","EntityName"),
  country = c("cntry_of_incorporation","Country of Headquarters","Country"),
  industry = c("Sub_Industry",NA,"Subindustry")
)
