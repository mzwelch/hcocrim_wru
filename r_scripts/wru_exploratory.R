#### WRU Exploratory Research ####
#This script explores the utility of the WRU pacakge in determinig Hisp/Non-Hisp in the HCo Crim. Ct Dataset
#Of interest are the the layers of added precision, confounding features, work with census data and geographic data.
#Not addressed in WRU package article "(insert link here)" is effect on M vs F prediction, increased or decreased accuracy (would only apply in case using first name)
#Other questions that have poppped up are signing officer race, is this useful?
#Methods to confirm "accuracy" rate
#Finally, the package will give a likelihood across 5+ racial categories, methods to determine race:
#1) take highest and use that
#2) weight the entries according to top two, three to all five categories

#if new, run init_environ.R shapefile_prep.R, wru_dataprep.R
#geotagged data with applicable wru fields and exploratory fields stored as geoloc

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
#name,sex
geoname_sex_wru <- geoloc %>% st_drop_geometry( ) %>%
  predict_race(
    voter.file = .,
    sex = T,
    census.geo = "tract",
    census.key = "CENSUS_API_KEY",
    census.data = wru_tract)
#name, sex, age
geoname_sex_wru <- geoloc %>% st_drop_geometry( ) %>%
  predict_race(
    voter.file = .,
    sex = T,
    age = T,
    census.geo = "tract",
    census.key = "CENSUS_API_KEY",
    census.data = wru_tract)

#### convert predictions to usable form (long) and hispanic/non-hisp####
## create list of predictions, create loop, convert all to long in order to weight

## create list of predictions, create loop, 
## collapse predictions into hispanic v. nonhispanic, convert all to long to weight

nameset <- data %>% slice(1:10) %>% select(surname,first,middle, def_rac, def_sex, def_dob, def_st)
data(voters)
predict_race(nameset)
predict_race(nameset,surname.only = TRUE)