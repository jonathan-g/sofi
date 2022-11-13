# The script automatically downloads the data from ACS 5-year source
# The function takes the ACS data table (acs_indicator_info) and its 
# variable to download from the ACS

library(dplyr)
library(tidyverse)
library(tidycensus)
library(sf)
library(rprojroot)
proj_root <- find_root(is_rstudio_project)

#vars2018 <- load_variables(2018, "acs5", cache = TRUE)

acs_data_process <- function(scale, table, var, state, county, year){
  tab <- get_acs(geo = scale, table = table, state = state, county = county, year = year, cache_table = TRUE, geometry = TRUE) %>%
    filter(variable %in% var) %>%
    group_by(NAME)
}

name <- c('pop', 'male_pop', 'female_pop', 'asian', 'black', 'hispanic', 'white', 'limt_edu_no_schol', 'limt_edu_nursery', 'limt_edu_kindergarten', 'limt_edu_1st', 'limt_edu_2nd',
          "limt_edu_3rd", "limt_edu_4th", "limt_edu_5th", "limt_edu_6th", "limt_edu_7th", "limt_edu_8th", "limt_edu_9th", "limt_edu_10th", "limt_edu_11th", "limt_edu_12th", 
          "limt_edu_regular_high", "limt_edu_ged", "limt_edu_less_1_college", "limt_edu_1_more_college", 'limt_edu_assiciate', 'limt_english', 'median_income', 'unemployment', 'poverty',
          'median_house_value', 'median_gross_rent', 'fam_own_child', "single_parent_family", "single_person_hshd", 'total_hshd', 'senior_hshd', 'multi_fam_hous_owner', 
          'multi_fam_hous_renter')

table <- c('B01003', "B01001", "B01001", "B02001", "B02001", "B03001", "B02001", "B15003", "B15003", "B15003", "B15003", "B15003", "B15003", "B15003", "B15003", "B15003", "B15003", 
           "B15003", "B15003", "B15003", "B15003", "B15003", "B15003", "B15003", "B15003", "B15003", "B15003", "B16003", "B19013", "B23025", "B17001", "B25077", "B25064", "B09002",
           "B09002", "B11001", "B11001", "B11007", "B25032", "B25032")

var <- c('B01003_001', 'B01001_002', "B01001_026", "B02001_005", "B02001_003", "B03001_003", "B02001_002", "B15003_002", "B15003_003", "B15003_004", "B15003_005", "B15003_006",
         "B15003_007", "B15003_008", "B15003_009", "B15003_010", "B15003_011", "B15003_012", "B15003_013", "B15003_014", "B15003_015", "B15003_016", "B15003_017", "B15003_018",
         "B15003_019", "B15003_020", "B15003_021", "B16003_001", "B19013_001", "B23025_007", 'B17001_002', "B25077_001", "B25064_001", "B09002_001", "B09002_008", "B11001_008",
         "B11001_001", "B11007_002", "B25032_010", "B25032_021")

acs_indicator_info <- data.frame(name = name, table = table, var = var)

# function to Download data from ACS 5-year
acs_download <- function(info_df, year) {
  df <- list()
for(i in 1:nrow(info_df)) {
 name  <- acs_indicator_info[i, 1]
 table <- acs_indicator_info[i, 2]
 var <- acs_indicator_info[i, 3]
 df[[name]] <- acs_data_process(scale = 'tract', table = table, var = var, state = 'TN', county = 'Davidson', year = year)
}
  return(df)
}

acs_df <- acs_download(acs_indicator_info, year = 2018)
#saveRDS(acs_df, file = (paste0(proj_root, "/data/src/acs_df.rds")))
