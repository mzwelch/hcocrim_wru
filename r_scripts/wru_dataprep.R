####WRU Exploratory this script imports the HCo Data and prepares it for use with library(wru)

#### environment setup, census key, libraries, etc.####

library(tidyverse)
library(censusapi)
library(lubridate)
library(janitor)
library(config)
library(googlesheets4)
library(here)
library(wru)
library(tidycensus)
library(jaspatial)
library(tigris)
library(sf)

#####harris co court data read-in and prep####

load(here::here("data_raw","dashboard.RData"))
load(here::here("data","tracts_TX.RData"))

#notes about what to do next:
#separate names (last, first, middle?)
full_filtered <- full %>% filter(fda > "2014-12-31" & fda < "2020-01-01") #subset data 2015-2019, avoids pandemic, eliminates option of census names list 2010 or 2020

##should look into selecting only certain variables, still large file, but might help with processing time (400k entries (though something seems very slow locally))
data <- full_filtered %>% #created another row because full is such large file
  separate(def_nam, into = c("surname","first"),sep = ", ") %>% separate(first, into = c("first", "middle"),sep = " ") %>% #def nam to three fields, header compliannt for predict_race()
  mutate(def_sex = case_when(
    def_sex =="M" ~ 0,
    def_sex =="F" ~ 1)) %>%    #convert sex for predict_race()
  mutate(def_rac = case_when(
    def_rac == "W" ~ 1,
    def_rac == "B" ~ 2,
    def_rac == "A" ~ 4,
    def_rac == "I" ~ 5,
    def_rac == "U" ~ 5)) %>%  #convert race for predict_race()
  as_tibble() %>% 
  rename(sex = def_sex, race = def_rac, age = ageatfiling) %>% #rename variable names for predict_race()
  st_as_sf(coords = c(43,42),na.fail = FALSE) %>%  #long, lat!
  st_set_crs("wgs84") #create point geometry

####Geolocation####
# using tract level 2019 (init_environ.R) and impute geoid for each def
# separate geo id to county and tract level

#join with census
geoloc <- st_join(data,tracts) %>% # impute tract level info for each def
  filter(!is.na(geoid)) %>% # if geoid is empty remove case. removes empty geometry and def outside of texas
  mutate(
    state = "TX", #no def remaining from outside of TX at this point
    county = substr(geoid, 3,5),
    tract = substr(geoid, 6,11)) %>%  #create cols for county and tract ids
  select(cas, surname,first, middle, race, sex, age, def_dob, def_stnum, def_stnam, def_cty, def_zip, #def cats
        fda, comp_agency, offcat, # offense cats
        geometry, geoid, geoname, geotype, state, county, tract) #geo cats

#number of def removed
missing_geoid <- length(data[[1]]) - length(geoloc[[1]])



#### run predict_race() and store ####
## first predictive method using wru W/O geo data (names only) ##
name_wru <- geoloc %>% st_drop_geometry() %>% predict_race(surname.only = T)

## second predictive method using wru W/ geo data ##
#names
geoname_wru <- geoloc %>% st_drop_geometry( ) %>%
  predict_race(
    voter.file = .,
    census.geo = "tract",
    census.key = "CENSUS_API_KEY",
    census.data = wru_tract)
## third method W/O wru package, application of racial distribution by tract
geo_tractdist <- geoloc %>% st_drop_geometry %>% 
  left_join(wru_tract[["TX"]][["tract"]] %>% select(tract,r_whi,r_bla,r_his,r_asi,r_oth), 
            by = "tract",
            keep = FALSE)
#name,sex
# geoname_sex_wru <- geoloc %>% st_drop_geometry() %>%
# #filter(is.na(sex) == FALSE ) %>% 
#  predict_race(
#    voter.file = .,
#    sex = T,
#    census.geo = "tract",
# census.key = "CENSUS_API_KEY",
#    census.data = wru_tract_sex)
#name, sex, age
#geoname_sex_age_wru <- geoloc %>% st_drop_geometry() %>%
#  predict_race(
#    voter.file = .,
#    sex = T,
#    age = T,
#    census.geo = "tract",
#    census.key = "CENSUS_API_KEY",
#    census.data = wru_tract_sex_age)
#name, initial race
#geoname_race.init <- geoloc %>% st_drop_geometry() %>%
#  predict_race(
#    voter.file = .,
#    race.init = T,
#    census.geo = "tract",
#    census.key = "CENSUS_API_KEY",
#    census.data = wru_tract_sex_age)

save(name_wru,geoname_wru,
     file = here::here("data","predictions.RData"))

#### convert predictions to usable form (long) and hispanic/non-hisp####
#load(here::here("data","predictions.RData")))
#hisp/nonhisp, sum all less pred hisp to pred_nonhisp
predict_list <- list(name_wru,geoname_wru)
predict_list_nonhisp <- list()
for (df in predict_list){
  df <- df %>% mutate(pred.nonhisp = pred.whi + pred.bla + pred.asi + pred.oth)
  predict_list_nonhisp <- list(c(predict_list_nonhisp,df))
}

predict_list_weights <- list()
for(df in predict_list){
  df <- df %>% 
    pivot_longer(
      cols = starts_with("pred."),
      names_to = "pred_race",
      names_prefix = "pred.",
      values_to = "predict_value"
    )
  predict_list_weights[[df]] <- df
}

name_wru_weight <-name_wru %>%
  mutate(pred.nonhisp = pred.whi + pred.bla + pred.asi + pred.oth) %>% #add ethnicity
  pivot_longer(
    cols = starts_with("pred."),
    names_to = "pred_race",
    names_prefix = "pred.",
    values_to = "predict_value")

geoname_wru_weight <-geoname_wru %>% 
  mutate(pred.nonhisp = pred.whi + pred.bla + pred.asi + pred.oth) %>% #add ethnicity
  pivot_longer( 
    cols = starts_with("pred."), 
    names_to = "pred_race",
    names_prefix = "pred.",
    values_to = "predict_value") # pivot to create weights

