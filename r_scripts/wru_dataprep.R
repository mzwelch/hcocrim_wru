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

