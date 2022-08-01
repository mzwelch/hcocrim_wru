#Prepare shapefiles
#Pull and clean shapefiles
library(tidyverse)
library(janitor)
library(tidycensus)
library(jaspatial)

jaspatial::load_geo_packages()
jaspatial::set_geo_options()
options(scipen = 999)
sf_use_s2(FALSE)

filter_shape <- function(.data, .dataId, .boundary, .boundaryId) {
  .boundaryId <- enquo(.boundaryId)
  .dataId <- enquo(.dataId)
  
  data <- st_join(.data, .boundary) %>% 
    filter(!is.na(!!.boundaryId)) %>% 
    distinct(., !!.dataId, .keep_all = T)
  
  return(data)
}

#Harris
counties <- tigris::counties(48, cb=T) %>%
  clean_shape() %>% 
  filter(geoid %in% c("48201")) %>% 
  dplyr::select(geoid = geoid, geoname = name) %>% 
  mutate(geotype = "Harris County")

# Tracts 2019 #######
tracts <- tigris::tracts(48, county=c(201, 157, 167, 339, 473), cb=T) %>% 
  clean_shape() %>% 
  mutate(geoname = geoid) %>% 
  dplyr::select(geoid, geoname) %>% 
  mutate(geotype = "Census tracts")


# Zip codes ###########
zip_codes <- tigris::zctas(cb=T) %>% 
  clean_shape() %>%
  filter_shape(., geoid10, counties, geoid) %>% 
  mutate(geoname = geoid10) %>% 
  dplyr::select(geoid = geoid10, geoname ) %>% 
  mutate(geotype = "Zip codes")

# Super Neighborhoods ##########
supern <- read_sf("https://opendata.arcgis.com/datasets/38f8418ea69d450ca49366260f83dcf6_3.geojson") %>% 
  clean_shape() %>% 
  mutate(geoid = paste0("SN", polyid)) %>% 
  dplyr::select(geoid, geoname = snbname, geometry) %>% 
  mutate(geotype = "Super neighborhoods")

#Tract to super neighborhood crosswalk #######
supern_tract_int <- st_intersection(tracts, supern) %>%
  mutate(intersect_area = st_area(.)) %>% 
  dplyr::select(geoid, supern = geoid.1, intersect_area) %>% 
  st_drop_geometry()

supern_tract_crosswalk <- tracts %>% 
  mutate(tract_area = st_area(.)) %>% 
  left_join(., supern_tract_int) %>% 
  mutate(coverage = as.numeric(intersect_area/tract_area)) %>% 
  filter(!is.na(supern)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(geoid, supern, coverage)



#Zip code to Harris County crosswalk ====================
harris_zip_int <- st_intersection(zip_codes, counties) %>%
  mutate(intersect_area = st_area(.)) %>% 
  dplyr::select(geoid, harris = geoid.1, intersect_area) %>% 
  st_drop_geometry()

harris_zip_crosswalk <- zip_codes %>% 
  mutate(zip_area = st_area(.)) %>% 
  left_join(., harris_zip_int) %>% 
  mutate(coverage = as.numeric(intersect_area/zip_area)) %>% 
  filter(!is.na(harris)) %>% 
  st_drop_geometry() %>% 
  dplyr::select(geoid, harris, coverage)

# #COH shapefile based on supern boundaries
coh = read_sf("https://opendata.arcgis.com/datasets/f67fe01244ce4d60add8e99e8e2f3a8c_0.geojson") %>% 
  count() %>%
  mutate(
    geotype = "Houston",
    geoname = "Houston",
    geoid = "4835000"
  ) %>%
  dplyr::select(geotype, geoname, geoid, geometry)

#COMBINE ###################################################
layers <- rbind(tracts, zip_codes, supern) %>% dplyr::select(geotype, everything())

# SAVE FILES ################################################
save(layers, harris = counties, supern_tract_crosswalk, harris_zip_crosswalk, file = here::here("data-clean", "shapefiles.RData"))

  
  