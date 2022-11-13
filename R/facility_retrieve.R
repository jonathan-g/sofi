# The script automatically downloads the data from open street map (OSM) source
# The function takes the OSM data frame (facility_indicator_info) that contains 
# the key and its value of the facility objects of interest to download from the OSM

library(osmdata)
library(ggplot2)
library(ggmap)
library('rprojroot')
proj_root <- find_root(is_rstudio_project)

# Davidson County, Nashville boundary
nash_bb <- getbb("Nashville")

facility_retrieve <- function(nash_bb, key, value){
  df <- nash_bb %>%
    opq() %>%
    add_osm_feature(key = key, value = value) %>%
    osmdata_sf()
}

name <- c('cathedral', 'chapel', 'monstery', 'mosque', 'religion', 'temple', 'church', 'charity', 'recreation',
          'pub', 'monument', 'stadium', 'cafe', 'restaurant', 'fire_station', 'grocery_alcohol', 'grocery_bakery', 
          'grocery_beverages', 'grocery_butcher', 'grocery_cheese', 'grocery_chocolate', 'grocery_coffee', 
          'grocery_confectionery', 'grocery_convenience', 'grocery_deli', 'grocery_dairy', 'grocery_farm',
          'grocery_frozen_food', 'grocery_greengrocer', 'grocery_health_food', 'grocery_ice_cream', 'grocery_pasta',
          'grocery_pastry', 'grocery_seafood', 'grocery_spices', 'grocery_tea', 'grocery_wine', 'grocery_water', 'supermarket',
          'emergency', 'healthcare_clinic', 'healthcare_dentist', 'healthcare_doctors', 'healthcare_pharmacy', 'healthcare_social_facility',
          'healthcare_nursing_home','healthcare_veterinary', 'hospital', 'college', 'kindergarten', 'library',
          'school', 'university', 'bank', 'bus_stop', 'green', 'barber')

key <- c('building', "building", "building", 'building', 'building', 'building', 'building', 'shop', 'leisure',
         'amenity', 'historic', 'building', 'amenity', 'amenity', 'building', 'shop', 'shop', 'shop', 'shop',
         'shop', 'shop', 'shop', 'shop', 'shop', 'shop', 'shop', 'shop', 'shop', 
         'shop', 'shop', 'shop', 'shop', 'shop', 'shop', 'shop', 'shop', 'shop', 'shop', 
         'building', 'emergency', 'amenity', 'amenity',
         'amenity', 'amenity', 'amenity', 'amenity', 'amenity', 'amenity', 'amenity', 'amenity', 'amenity', 'amenity',
         'amenity', 'amenity', 'highway', 'landuse', 'shop')

value <- c('cathedral', 'chapel', "monastery", "mosque", 'religious', 'temple', 'church', 'charity', 'fitness_centre',
           'bar', 'monument', 'stadium', 'cafe', 'restaurant', 'fire_station', 'alcohol', 'bakery', 'beverages', 'butcher',
           'cheese', 'chocolate', 'coffee', 'confectionery', 'convenience', 'deli', 'dairy', 'farm', 'frozen_food',
           'greengrocer', 'health_food', 'ice_cream', 'pasta', 'pastry', 'seafood', 'spices', 'tea', 'wine', 'water',
           'supermarket', 'ambulance_station', 'clinic', 'dentist', 'doctors', 'nursing_home', 'pharmacy', 'social_facility', 'veterinary',
           'hospital', 'college', 'kindergarten', 'library', 'school', 'university', 'bank', 'bus_stop', 'recreation_ground',
           'hairdresser')

facility_indicator_info <- data.frame(name = name, key = key, value = value)

# function to download data from OSM 
facility_download <- function(info_df){
  df <- list()
for(i in 1:nrow(info_df)) {
  name  <- info_df[i, 1]
  key <- info_df[i, 2]
  value <- info_df[i, 3]
  df[[name]] <- facility_retrieve(nash_bb, key, value)
}
  return(df)
}

facility_df <- facility_download(facility_indicator_info)
#saveRDS(facility_df, file = (paste0(proj_root, "/data/src/facility_df.rds")))
# retrieving map of lagos
# nash_map <- get_map(nash_bb, maptype = "roadmap")
# 
# ggmap(nash_map) +
#   geom_sf(
#     data = nash_bars$osm_polygons,
#     inherit.aes = FALSE,
#     colour = "#08519c",
#     fill = "#08306b",
#     alpha = .5,
#     size = 1
#   ) +
#   geom_sf(
#     data = nash_bars$osm_points,
#     inherit.aes = FALSE,
#     colour = "#08519c",
#     fill = "#08306b",
#     alpha = .5,
#     size = 1
#   ) +
#   labs(
#     title = "Bars in Nashville",
#     x = "Latitude",
#     y = "Longitude"
#   )
