#### WRU Exploratory Research ####
#This script explores the utility of the WRU pacakge in determinig Hisp/Non-Hisp in the HCo Crim. Ct Dataset
#Of interest are the the layers of added precision, confounding features, work with census data and geographic data.
#Not addressed in WRU package article "(insert link here)" is effect on M vs F prediction, increased or decreased accuracy (would only apply in case using first name)
#Other questions that have poppped up are signing officer race, is this useful?
#Methods to confirm "accuracy" rate
#Finally, the package will give a likelihood across 5+ racial categories, methods to determine race:
#1) take highest and use that
#2) weight the entries according to top two, three to all five categories

#run init_environ.R shapefile_prep.R, wru_dataprep.R
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



#general pop stats
#can use either _wru as we are not yet getting into weights and apps of those numbers
#top ten off_cat
genpop_offcat <- name_wru %>% 
  group_by(offcat) %>% 
  summarise(n = n()) %>% arrange(desc(n))
#top ten by off_cat and race
genpop_offcat_race <- name_wru %>% 
  group_by(offcat,race) %>% 
  summarise(n = n()) %>% arrange(desc(n))

#use pred_value as weights, group by pred_race and off_cat, sum on pred_value
weighted_offcat_pred_race_name <- name_wru_weight %>% filter(pred_race != "nonhisp") %>% 
  group_by(offcat, pred_race) %>% 
  summarise(n = sum(predict_value)) %>% arrange(desc(n))
weighted_offcat_pred_eth_name <- name_wru_weight %>% filter(pred_race == "nonhisp" | pred_race == "his") %>% 
  group_by(offcat, pred_race) %>% 
  summarise(n = sum(predict_value)) %>% arrange(desc(n))

weighted_offcat_pred_race_geoname <- geoname_wru_weight %>% filter(pred_race != "nonhisp") %>% 
  group_by(offcat, pred_race) %>% 
  summarise(n = sum(predict_value)) %>% arrange(desc(n))
weighted_offcat_pred_eth_geoname <- geoname_wru_weight %>% 
  filter(pred_race == "nonhisp" | pred_race == "his") %>% 
  group_by(offcat, pred_race) %>% 
  summarise(n = sum(predict_value)) %>% arrange(desc(n))

