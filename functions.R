# May 17, 2022
# to-do
# update sustainalytics cleaning function
# options to take newest available observation or attach date and record all
# implement S&P function

# ------------------------------------------------------------------------------
# Setting up packages
# ------------------------------------------------------------------------------
pacman::p_load("readxl","data.table","purrr","stringr","dplyr")
# increasing storage capacity to 7GB
memory.limit(size=56000)

# ------------------------------------------------------------------------------
# Check data validity
# ------------------------------------------------------------------------------
check_empty_list <- function(list){
  if (is_empty(list)) {stop("No files found. Check naming of files.")}
}

# works for bloomberg and refinitiv data
check_data <- function(cl, data.list){
  
  check_empty_list(data.list)
  
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
# Rename columns
# ------------------------------------------------------------------------------
rename_column <- function(data_names, name_list, text){
  data_names <- ifelse(!(data_names %in% name_list), paste0(text, data_names), data_names)
  return(data_names)
}

# ------------------------------------------------------------------------------
# Prepare Bloomberg data
# ------------------------------------------------------------------------------
p_Bloomberg <- function(cl, rename){
  # get file list with all xlsx files beginning with Bloomberg
  bloomberg.list <- list.files(pattern='^(?i)bloomberg.*.xlsx', recursive = TRUE)
  bloomberg.df.list <- lapply(bloomberg.list, read_excel)

  # check validity of files, and if valid, 
  # bind files of different regions into one bloomberg data table
  bloomberg.df <- check_data(cl, bloomberg.df.list)
  
  # clean column names
  colnames(bloomberg.df) <- gsub("^#|\\(\\)$", "", colnames(bloomberg.df))
  colnames(bloomberg.df) <- rename_column(colnames(bloomberg.df), rename, "Bloomberg_")
  
  return(bloomberg.df)
}

# ------------------------------------------------------------------------------
# Prepare Refinitiv data
# ------------------------------------------------------------------------------
p_Refinitiv <- function(cl, rename){
  # get file list with all xlsx files beginning with Refinitiv
  refinitiv.list <- list.files(pattern='^(?i)refinitiv.*.xlsx', recursive = TRUE)
  refinitiv.df.list <- lapply(refinitiv.list, read_excel)
  
  # check validity of files, and if valid, 
  # bind files of different regions into one refinitiv data table
  refinitiv.df <- check_data(cl, refinitiv.df.list)
  colnames(refinitiv.df) <- rename_column(colnames(refinitiv.df), rename, "Refinitiv_")

  return(refinitiv.df)
}

# ------------------------------------------------------------------------------
# Prepare Sustainalytics data
# ------------------------------------------------------------------------------
p_Sustainalytics <- function(cl, rename){
  # get file list with all csv files beginning with Sustainalytics
  sustain.list <- list.files(pattern='^(?i)sustainalytics.*.csv', recursive = TRUE)
  
  check_empty_list(sustain.list)

  # read reference and variable description files as data.table
  reference <- fread(sustain.list[grepl("(?i)reference", sustain.list)])
  reference <- reference[!duplicated(reference[, "EntityId"], fromLast=T), ]
  
  descriptions <- fread(sustain.list[grepl("(?i)description", sustain.list)])
  descriptions$Description <- gsub("(.*)\\s\\(fieldid_[0-9]+\\)", "\\1", descriptions$Description)
  descriptions <- descriptions[!duplicated(descriptions)]
  
  # read remaining files
  sustain.list.other <- sustain.list[!(grepl("(?i)reference | (?)description", sustain.list))]
  check_empty_list(sustain.list.other)
  # column bind # can't really do this, need to decide how to deal with data date first!
  
  sustain <- data.table()
  for (i in 1:length(sustain.list.other)){
    temp <- fread(sustain.list.other[i])
    # remove from data where EntityId row is NA
    temp <- temp[!is.na(temp$EntityId), ]
    # replace variable id with variable name/description
    for (i in 1:ncol(temp)) { 
      if (colnames(temp)[i] %in% c("EntityId", "FieldDate", "file_date", "GroupId")){next}
      if (length(descriptions$Description[which(descriptions$`Variable Name`==colnames(temp)[i])])==0){
        print(paste0("Unable to locate variable name for ", colnames(temp)[i]))
        next}
      colnames(temp)[i] = descriptions$Description[which(descriptions$`Variable Name`==colnames(temp)[i])]
    }
    
    # remove columns "FieldDate", "file_date", "GroupId" and duplicated EntityId
    temp <- as.data.frame(temp)
    temp <- temp[!duplicated(as.list(temp))]
    drop_columns <- c("FieldDate", "file_date", "GroupId", "EntityId.1")
    temp <- temp %>% select(-any_of(drop_columns))
    
    
    # keeping only most recent data for the same company
    temp <- temp[!duplicated(temp[, "EntityId"], fromLast=F), ]
    
    # merge
    if (nrow(sustain) == 0){
      sustain <- temp
      }else{sustain <- full_join(sustain, temp, by = "EntityId")}
  }
  
  # work this out
  #colnames(sustain_focus) <- lapply(colnames(sustain_focus), function(x) {
  #  x <- descriptions$Description[which(descriptions$`Variable Name`==x)]
  #  return(x)
  #  })
  
  
  # match company information from reference
  cols <- c("EntityId", cl$identifier, "EntityName", "Subindustry", "Country")
  sustain <- left_join(sustain, reference[, ..cols], by="EntityId")
  
  # append column name prefix
  colnames(sustain) <- rename_column(colnames(sustain), rename, "Sustain_")
  
  return(sustain)
}

# ------------------------------------------------------------------------------
# Prepare S&P data
# ------------------------------------------------------------------------------
p_SandP <- function(cl, rename){
  print("In development")
}
# ------------------------------------------------------------------------------
# Main merge function
# ------------------------------------------------------------------------------
merge_data <- function(cl, co){
  cl$identifier <- toupper(cl$identifier)
  rename_list <- c(as.vector(unlist(co)), names(co), cl$identifier, c("ID", "Identifier (RIC)", "FieldDate", "file_date", "EntityId"))
  rename_list <- rename_list[!is.na(rename_list)]
  
  # put all data required (flagged as 1) together in a list of data.tables
  merge_list <- list()
  # note that seq_along(cl[1:4]) produces 1 2 3 4, so definitions of y and n are needed
  # when data flags do not appear first in custom list; cl[1:4] hard coded
  merge_list <- lapply(seq_along(cl[1:4]), function(y, n, i){
    if (y[i]==1){
      print(paste("Cleaning", n[i], "data..."))
      append(merge_list, get(paste0("p_", n[i]))(cl, rename_list))
      }
  }, y=cl[1:4], n=names(cl[1:4]))
  
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
    co_temp <- as.vector(co[, i])[!is.na(as.vector(co[, i]))]
    merged_data <- merged_data %>%
      mutate(!!i := coalesce(!!!select(., any_of(co_temp))))
  }
  
  ncol_before <- ncol(merged_data)
  
  # clean up columns
  co_vector <- as.vector(as.matrix(co))[!is.na(as.vector(as.matrix(co)))]
  merged_data <- merged_data %>% 
    select(-any_of(co_vector)) %>%
    select(cl$identifier, colnames(co), everything())
  
  ncol_after <- ncol(merged_data)
  print(paste("Removed", ncol_before - ncol_after, "columns used in coalesce"))
  
  # save merged data to source file location
  # naming by counting number of copies
  #existing_count <- length(list.files(pattern='^merged_ESG_data.*.csv', recursive = FALSE))
  #if (existing_count == 0){file_name <- "merged_ESG_data.csv"
  #} else if (existing_count == 1) {file_name <- "merged_ESG_data copy.csv"
  #} else {file_name <- paste0("merged_ESG_data copy ", existing_count, ".csv")}
  
  # naming by pasting time
  file_name <- paste0("merged_ESG_data ", format(Sys.time(), "%Y-%m-%d %I.%M%p"), ".csv")
  write.csv(merged_data, file_name, na = "", row.names = FALSE)
  print(paste0("Exported: ", file_name, ". Check source file location"))
  
}
