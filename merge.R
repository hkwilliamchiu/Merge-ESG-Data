# Author: William Chiu
# Date: Feb 23, 2022
# This is a preliminary draft; more updates to come

# set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
#source("https://raw.githubusercontent.com/hkwilliamchiu/Merge-ESG-Data/main/functions.R")
source("functions.R")

custom_list <- list(
  bloomberg = 1,
  refinitiv = 1,
  sustainalytics = 1,
  # ISIN recommended
  identifier = "ISIN"
)

coalesce_names <- data.frame(
  row.names = c("bloomberg","refinitiv","sustainalytics"),
  company_name = c("Name","Company Name","EntityName"),
  country = c("cntry_of_incorporation","Country of Headquarters","Country"),
  industry = c("Sub_Industry",NA,"Subindustry")
)

merge_data(custom_list, coalesce_names)
