# Author: William Chiu
# Date: Feb 23, 2022
# This is a preliminary draft; more updates to come

# set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
install.packages("pacman")

#source("https://raw.githubusercontent.com/hkwilliamchiu/Merge-ESG-Data/main/functions.R")
source("functions.R")

custom_list <- list(
  Bloomberg = 1,
  Refinitiv = 1,
  Sustainalytics = 1,
  identifier = "isin" # ISIN recommended
)

coalesce_names <- data.frame(
  company_name = c("Name","Company Name","EntityName",NA),
  country = c("Country of Headquarters","Country","cntry_of_incorporation","HQ Address Country ISO"),
  industry = c("Sub_Industry","Subindustry",NA,NA)
)

merge_data(custom_list, coalesce_names)
