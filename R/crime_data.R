# The script maps the crime data from council district to census tract boundary.
library(sf)
library(fasterize)
library(raster)
library(exactextractr)
library(dplyr)
library(rprojroot)

proj_root <- find_root(is_rstudio_project)

# read in the council districts shapefile.
council_district_shp <- read_sf(paste0(proj_root, "/data/src/crime_data/geo.shp"))

# The crime data is hard coded from the UCR2021ByCouncilDistrict.pdf
# The sequence is from council district 01-35
crime_total_count <- c(908, 1644, 1218, 236, 1649, 1224, 976, 358, 1029,
                       832, 539, 416, 1039, 860, 1902, 1278, 2076, 410, 
                       4335, 1076, 1700, 533, 250, 573, 585, 959, 682, 
                       1145, 550, 1026, 362, 733, 501, 364, 355)

# crime total count for each of the council district 
crime <- cbind(council_district_shp, crime_total_count) %>% 
  st_transform(crs = st_crs(census_tract)) %>%
  mutate(area = st_area(.), units = area/900, crime_units = crime_total_count/units) %>%
  st_as_sf()

# save raw crime total count for each council district
#saveRDS(crime, file = (paste0(proj_root, "/data/src/crime_data/crime.rds")))

# Map the crime data from council district to census tract using NLCD 30m raster
nlcd_raster <- raster::raster(file.path(proj_root, "data/src/nlcd-2019.img"))
crime_r <- fasterize(crime %>% st_transform(st_crs(nlcd_raster)), nlcd_raster, field = 'crime_units')
crime_census_tract <- exact_extract(crime_r, census_tract %>% st_transform(st_crs(nlcd_raster)), 'sum')
crime_census_tract_df <- cbind(census_tract, crime = crime_census_tract)

# save mapped crime data for each census tract 
#saveRDS(crime_census_tract_df, file = (paste0(proj_root, "/data/gen/crime_census_tract_df.rds")))