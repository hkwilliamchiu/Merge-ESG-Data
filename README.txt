====== Description ======
- This folder contains ESG data download from Bloomberg, Refinitiv and Sustainalytics, and an R script, merge.R, for cleaning and merging them, eventually producing merged_ESG.csv (or copies of it...)
- There is minimum modification to the raw ESG data downloaded (only one modification to Bloomberg data as discussed below), almost all of the cleaning is performed by the code
- The primary (and only) key for merge is ISIN which is provided by all 3 data vendors
- Company name, sub-industry, and country of incorporation data provided by the 3 sources are also combined (via dplyr::coalesce)

====== Data ======
The files that are used for merging are:

- Bloomberg
    * Bloomberg data.xlsx : a sample ESG data downloaded via Bloomberg Query Language. Sheet "BQL" cell A1 displays the fuction used to download the data. Note that the function can only run in a Bloomberg terminal. Shee1 contains a duplicate, as values, of the data downloaded, and the cell containing the BQL function is removed.

- Refinitiv
    * Refinitiv_ESG_2020_AfricaAndOceania.xlsx : a sample ESG data downloaded via Refinitiv workspace. It contains some selected ESG metrics for all companies located in the region Africa and Oceania
    * Refinitiv_ESG_2020_~ : same sample ESG data as above for region ~
    
- Sustainalytics
    * Sustainalytics ESG Risk Rating Focus data.csv : ESG Risk Rating Focus data downloaded from WRDS. Note that the columns of variables are named as fiid ids, which will be replaced by actual variable descriptions (See Sustainalytics Variable Descriptions.csv)
    * Sustainalytics Variable Descriptions.csv : a list containing corresponding variable descriptions of all field ids
    * Sustainalytics Reference data : company reference data downloaded from WRDS. It contains company information that is matched by Sustainalytics' internal id (EntityId)