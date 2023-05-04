====== Description ======
- This folder contains ESG data download from Bloomberg, Refinitiv and Sustainalytics, and an R script, merge.R, for cleaning and merging them, eventually producing merged_ESG.csv
- There is minimum modification to the raw ESG data downloaded (only one modification to Bloomberg data as discussed below), almost all of the cleaning is performed by the code
- The primary key for merging is ISIN which is provided by all 3 data vendors
- Company name, sub-industry, and country of incorporation data provided by the 3 sources are also combined (via dplyr::coalesce)

====== How to use ======
- If you do not need to edit the source code, you should be able to have your data merged only using merge.R if you:
   * Set the variables of the list custom_list to decide which data to use (1 to include and 0 to not include)
   * Provide the unique identifier that you want to use (ISIN is recommended)
   * Provide a data frame of columns that you want to coalesce from the data (note: given a set of vectors, dplyr::coalesce finds the first non-missing value at each position).
      - Column names you supplied (such as company_name, country, and industry) will be the names of the coalesced columns, and the vector on the right should contain columns that you want to use in coalesce. Hence make sure the column names do not duplicate any of the columns used for coalesce. All the columns used in coalesce will be removed from the merged data. Remember to fill in NAs to the shorter vectors so that each vector is of the same length.
   * After the required data are filled in, run the entire script.
   
====== Data used in demo ======
The files that are used for merging are:

- Bloomberg
    * Bloomberg data.xlsx : a sample ESG data downloaded via Bloomberg Query Language. Sheet "BQL" cell A1 displays the function used to download the data. Note that the function can only run in a Bloomberg terminal. Sheet1 contains a duplicate, as values, of the data downloaded, and the cell containing the BQL function is removed.

- Refinitiv
    * Refinitiv_ESG_2020_AfricaAndOceania.xlsx : a sample ESG data downloaded via Refinitiv workspace. It contains some selected ESG metrics for all companies located in the region Africa and Oceania
    * Refinitiv_ESG_2020_~ : same sample ESG data as above for region ~
    
- Sustainalytics
    * Sustainalytics ESG Risk Rating Focus data.csv : ESG Risk Rating Focus data downloaded from WRDS. Note that the columns of variables are named as fiid ids, which will be replaced by actual variable descriptions (See Sustainalytics Variable Descriptions.csv)
    * Sustainalytics Variable Descriptions.csv : a list containing corresponding variable descriptions of all field ids
    * Sustainalytics Reference data : company reference data downloaded from WRDS. It contains company information that is matched by Sustainalytics' internal id (EntityId)
