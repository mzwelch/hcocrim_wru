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
## third method W/O wru package, application of racial distribution by tract
geo_tractdist <- geoloc %>% st_drop_geometry %>% 
  left_join(wru_tract[["TX"]][["tract"]] %>% select(tract,r_whi,r_bla,r_his,r_asi,r_oth), 
            by = "tract",
            keep = FALSE)
#name,sex
#geoname_sex_wru <- geoloc %>% st_drop_geometry( ) %>%
#  predict_race(
#    voter.file = .,
#    sex = T,
#    census.geo = "tract",
    #census.key = "CENSUS_API_KEY",
    #    census.data = wru_tract_sex)
#name, sex, age
#geoname_sex_age_wru <- geoloc %>% st_drop_geometry( ) %>%
#  predict_race(
#    voter.file = .,
#    sex = T,
    #    age = T,
    #    census.geo = "tract",
    #    census.key = "CENSUS_API_KEY",
    #    census.data = wru_tract_sex_age)
#name, initial race
#geoname_race.init <- geoloc%>% st_drop_geometry( ) %>%
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


predict_list_weights
for(df in predict_list){
  df <- df %>% 
    pivot_longer(
      cols = starts_with("pred."),
      names_to = "pred_race",
      names_prefix = "pred.",
      values_to = "predict_value"
    )
}



## create list of predictions, create loop, convert all to long in order to weight


## create list of predictions, create loop, 
## collapse predictions into hispanic v. nonhispanic, convert all to long to weight

nameset <- data %>% slice(1:10) %>% select(surname,first,middle, def_rac, def_sex, def_dob, def_st)
data(voters)
predict_race(nameset)
predict_race(nameset,surname.only = TRUE)