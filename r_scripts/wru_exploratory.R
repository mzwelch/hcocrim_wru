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

library(tidyverse)
library(highcharter)
library(jastyle)
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



####general pop stats####
#can use either _wru as we are not yet getting into weights and apps of those numbers
#top ten off_cat
genpop_offcat <- name_wru %>% 
  group_by(offcat) %>% 
  summarise(n = n()) %>% arrange(desc(n))
genpop_offcat <- genpop_offcat %>% 
  mutate(percent = n/sum(n)*100) %>% 
  mutate(label = sub(".^","chr%",round(percent,0)))
genpop_offcat %>% slice(1:10) %>% 
  hchart("bar",hcaes(x = offcat, y = percent)) %>% 
  hc_title(text = "More offenders are charged with non-sexual assault than other offense categories") %>% 
  hc_subtitle(text = "Top Ten Offense Categories in Harris County Criminal Court, 2014-2019") %>% 
  hc_caption(text = "Harris County Criminal Court, 2014-2019") %>% 
  hc_yAxis(title = list(text = "% of cases receiving offense category charge")) %>%
  hc_xAxis(title = list(text = "Offense Category")) %>% 
  hc_plotOptions(
    bar = list(
      dataLabels = list(
        enabled = TRUE,
        formatter = JS(
          "
        function(){return(this.point.label)}
       "
        )
      )
    )
  ) %>% 
  hc_add_theme(ja_hc_theme()) 



#top ten by off_cat and race
genpop_offcat_race <- name_wru %>% 
  group_by(offcat,race) %>% 
  summarise(n = n()) %>% arrange(desc(n)) %>% ungroup()  %>% 
  mutate(race = case_when(race == 1 ~ "White",
                          race == 2 ~ "Black",
                          race == 4 ~ "Asian",
                          race == 5 ~ "Native American/Unknown"))
total_by_race <- genpop_offcat_race %>% 
  group_by(race) %>% 
  summarise(total = sum(n))
genpop_offcat_race <- genpop_offcat_race %>% 
  left_join(total_by_race, by = "race") %>% 
  mutate(percent = n/total * 100) %>% 
  mutate(label = sub(".^","chr%",round(percent,0))) %>% 
  group_by(race) %>% arrange(desc(n), .by_group = T)
  
genpop_offcat_race %>% filter(n > mean(n)) %>% 
  hchart("bar",hcaes(x = race,y = percent, group = offcat)) %>% 
  hc_title(text = "Prostitution and Unlawful Possession/Conduct with a Weapon are charge categories almost unique to asian and black offenders respectively") %>%
  hc_subtitle(text = "Share of Offense Categories by Police-Defined Race, categories greater than racial group average") %>% 
  hc_caption(text = "Harris County Criminal Court, 2014-2019") %>% 
  hc_xAxis(title = "Police-Defined Race") %>% 
  hc_yAxis(title = "% offense category", max = 20) %>% 
  hc_plotOptions(
    bar = list(
      dataLabels = list(
        enabled = TRUE,
        formatter = JS(
          "
        function(){return(this.point.label)}
       "
        )
      )
    )
  )   %>% 
  hc_add_theme(ja_hc_theme())





#use pred_value as weights, group by pred_race and off_cat, sum on pred_value
weighted_offcat_pred_race_name <- name_wru_weight %>% filter(pred_race != "nonhisp") %>% 
  group_by(offcat, pred_race) %>% 
  summarise(n = sum(predict_value)) %>% arrange(desc(n))

total_by_race <- weighted_offcat_pred_race_name %>% 
  group_by(pred_race) %>% 
  summarise(total = sum(n))
weighted_offcat_pred_race_name <- weighted_offcat_pred_race_name %>% 
  left_join(total_by_race, by = "pred_race") %>% 
  mutate(percent = n/total * 100) %>% 
  mutate(label = sub(".^","chr%",round(percent,0)))

weighted_offcat_pred_race_name %>% filter(n > mean(n)) %>% 
  hchart("bar",hcaes(x = pred_race,y = percent, group = offcat)) %>% 
  hc_title(text = "Census name prediction suggests Hispanics charged under Alcohol-Driving comprise the largest offense category") %>%
  hc_subtitle(text = "Share of Offense Categories by Race Predicted by Name") %>% 
  hc_caption(text = "Harris County Criminal Court, 2014-2019") %>% 
  hc_xAxis(title = "Prediced Race by Name") %>% 
  hc_yAxis(title = "% offense category", max = 100) %>% 
  hc_plotOptions(
    bar = list(
      dataLabels = list(
        enabled = TRUE,
        formatter = JS(
          "
        function(){return(this.point.label)}
       "
        )
      )
    )
  )   %>% 
  hc_add_theme(ja_hc_theme())


weighted_offcat_pred_race_geoname <- geoname_wru_weight %>% filter(pred_race != "nonhisp") %>% 
  group_by(offcat, pred_race) %>% 
  summarise(n = sum(predict_value)) %>% arrange(desc(n))

total_by_race <- weighted_offcat_pred_race_name %>% 
  group_by(pred_race) %>% 
  summarise(total = sum(n))
weighted_offcat_pred_race_name <- weighted_offcat_pred_race_name %>% 
  left_join(total_by_race, by = "pred_race") %>% 
  mutate(percent = n/total * 100) %>% 
  mutate(label = sub(".^","chr%",round(percent,0)))

weighted_offcat_pred_race_name %>% 
  hchart("bar",hcaes(x = pred_race,y = percent, group = offcat), stacking = "normal") %>% 
  hc_title(text = "Census name prediction suggests Hispanics charged under Alcohol-Driving comprise the largest offense category") %>%
  hc_subtitle(text = "Share of Offense Categories by Race Predicted by Name") %>% 
  hc_caption(text = "Harris County Criminal Court, 2014-2019") %>% 
  hc_xAxis(title = "Prediced Race by Name") %>% 
  hc_yAxis(title = "% offense category", max = 100) %>% 
  hc_plotOptions(
    bar = list(
      dataLabels = list(
        enabled = TRUE,
        formatter = JS(
          "
        function(){return(this.point.label)}
       "
        )
      )
    )
  )   %>% 
  hc_add_theme(ja_hc_theme())


#use pred_value as weights, group by pred_ETHNICITY and off_cat, sum on pred_value
weighted_offcat_pred_eth_geoname <- geoname_wru_weight %>% 
  filter(pred_race == "nonhisp" | pred_race == "his") %>% 
  group_by(offcat, pred_race) %>% 
  summarise(n = sum(predict_value)) %>% arrange(desc(n))
weighted_offcat_pred_eth_name <- name_wru_weight %>% filter(pred_race == "nonhisp" | pred_race == "his") %>% 
  group_by(offcat, pred_race) %>% 
  summarise(n = sum(predict_value)) %>% arrange(desc(n))
