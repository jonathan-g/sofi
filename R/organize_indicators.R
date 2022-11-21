# The script reads in all indicators (ACS, facility, crime) and organizes them to 
# form a data frame.

library(dplyr)
library(rprojroot)
library(sf)

proj_root <- find_root(is_rstudio_project)

# read in the ACS indicators data frame list
acs_list <- readRDS(paste0(proj_root, '/data/src/acs_df.rds'))

# read in the facility indicators data frame list
facility_list <- readRDS(paste0(proj_root, '/data/gen/facility.rds'))

# read in the crime data frame list
crime_df <- readRDS(paste0(proj_root, '/data/gen/crime_census_tract_df.rds'))

# append columns function
append_column <- function(list, df, variable = 'estimate'){
  for (i in (1:length(list))) {
    name <- names(list)[[i]]
    df <- list[[i]] %>% 
      st_drop_geometry() %>%
      ungroup() %>%
      dplyr::select('GEOID',  !!name := !!variable) %>%
      left_join(df, by = 'GEOID')
  }
  return(df)
}

# append acs columns
sofi_indicators_df <- append_column(acs_list, crime_df, 'estimate')

# add facility columns
sofi_indicators_df <- append_column(facility_list, sofi_indicators_df, 'count')

# organize the columns (sum and recategorize, regroup and reorder)
sofi_indicators_df <- sofi_indicators_df %>%
  mutate(healthcare = across(starts_with("healthcare")) %>% rowSums) %>% 
  mutate(grocery = across(starts_with("grocery")) %>% rowSums) %>%
  mutate(limt_edu = across(starts_with("limt_edu")) %>% rowSums) %>%
  mutate(multi_fam = across(starts_with("multi_fam")) %>% rowSums) %>%
  dplyr::select(-contains(c("healthcare_", "limt_edu_", "grocery_", "multi_fam_"))) %>%
  st_as_sf() %>%
  mutate(area = st_area(.)) %>%
  relocate('NAME', .after = 'GEOID') %>%
  relocate('geometry', .after = 'multi_fam')

# save the final sofi indicators data frame
#saveRDS(sofi_indicators_df, file = (paste0(proj_root, "/data/gen/sofi_indicators_df.rds")))

