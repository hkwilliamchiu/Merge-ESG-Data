# Date: May 4, 2022
# More updates to come

# set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
# install.packages("pacman")

#source("https://raw.githubusercontent.com/hkwilliamchiu/Merge-ESG-Data/main/functions.R")
source("functions.R")

custom_list_company <- list(
  Bloomberg = 0,
  Refinitiv = 0,
  Sustainalytics = 0,
  SandP = 0,
  identifier = "isin" # ISIN recommended
)

custom_list_fund <- list(
  ISS = 0,
  Refinitiv_fund = 0,
  identifier = "isin"
)

coalesce_company <- data.frame(
  company_name = c("Name","Company Name","EntityName","companyname",NA),
  country = c("Country of Headquarters","Country","cntry_of_incorporation","HQ Address Country ISO","incorporation_country"),
  industry = c("Sub_Industry","Subindustry","simpleindustry",NA,NA)
)

merge_data(custom_list_company, coalesce_company)
