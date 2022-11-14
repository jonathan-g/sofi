# The script reads in the facility objects data and manipulates the data
library(sf)
library(dplyr)
library(rprojroot)

proj_root <- find_root(is_rstudio_project)

facility_df <- readRDS(paste0(proj_root, '/data/src/facility_df.rds'))

facility_count <- function(df, census_tract, type = 'point'){
  if(nrow(df) == 0||is.null(df)){
    df <- census_tract %>%
      dplyr::select(-'NAME') %>%
      mutate(!!paste0(type, '_count') := 0)
  }
  
  else{  
    df <- st_join(df %>% st_transform(st_crs(census_tract)), 
                  census_tract, join = st_intersects) %>%
      group_by(GEOID) %>%
      summarise(!!paste0(type, '_count') := n()) %>%
      filter(!is.na(GEOID))
  }
}

facility <- list()

for(i in (1:length(facility_df))){
  name <- names(facility_df)[[i]]
  
  df_points <- facility_count(facility_df[[i]]$osm_points, census_tract, 'point')
  df_polygon <- facility_count(facility_df[[i]]$osm_polygons, census_tract, 'polygon')
  df_multipolygon <- facility_count(facility_df[[i]]$osm_multipolygons, census_tract, 'multipolygon')
  
  df <- df_points %>% st_drop_geometry() %>%
    left_join(df_polygon %>% st_drop_geometry(), by = 'GEOID') %>%
    left_join(df_multipolygon %>% st_drop_geometry(), by = 'GEOID') %>%
    right_join(census_tract, by = 'GEOID') %>%
    replace(is.na(.), 0) %>%
    mutate(count = point_count + polygon_count + multipolygon_count) %>%
    dplyr::select(-'point_count', -'polygon_count', -'multipolygon_count')
  
  facility[[name]] <- df
}
#saveRDS(facility, file = (paste0(proj_root, "/data/gen/facility.rds")))

