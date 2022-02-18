# Author: William Chiu
# Date: Feb 18, 2022
# This is a very preliminary draft with limited capability/flexibility; more updates to come
# to-do
# decide on what company parameters to report (and how to call them)
# Name, ISIN, Industry, Country
# Bloomberg: Name, ISIN, Sub_Industry, cntry_of_incorporation
# Refinitiv: Company Name, ISIN, ..., Country of Headquarters
# Sustainalytics: EntityName, ISIN, Subindustry, Country

# create global reference file?

# possible primary merger: ISIN

# support KLD social ratings

# before merging, need to ascertain: what files are fed, what company parameters to use

# set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
source("functions.R")

custom_list <- list(
  bloomberg_data = 1,
  refinitiv_data = 1,
  sustainalytics_data = 1,
  # Choose one of the following supported identifier: ISIN
  identifier = "ISIN"
)

merge_data(custom_list)