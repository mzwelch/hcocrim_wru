#wru example
# WRU Add race -----------------------------------------------------------
#Shannon: download tract dataset first
#texas tracts (2010), using wru package
wru_tract <- get_census_data(key = Sys.getenv("CENSUS_API_KEY"),
                                state = c("TX"), age = F, sex = F,
                              census.geo = "tract")
# 
wru_place <- get_census_data(key = Sys.getenv("CENSUS_API_KEY"),
                              state = c("TX"), age = F, sex = F,
                              census.geo = "place")
# 
save(wru_tract, wru_place, file = here::here("data", "wru_census_tx.RData"))

load(here::here("data", "wru_census_tx.RData"))

#Shannon: clean up harris county defendant names here
harris_wru <- geoloc %>% 
  select(geoid,
         cas, fda, offcat, surname, first, middle,
         race, sex, def_dob, age, geoid) %>% 
  mutate(
    state = "TX",
    county = substr(geoid, 3,5),
    tract = substr(geoid, 6,11))



harris_wru_predict <- harris_wru %>% filter(!is.na(geoid)) %>% 
  predict_race(voter.file = ., 
               census.data = wru_tract,
               census.geo = "tract", 
               census.key = Sys.getenv("CENSUS_API_KEY")) 

# WRU Add race -----------------------------------------------------------
#Shannon: download tract dataset first
# wru_tract <- get_census_data(key = Sys.getenv("CENSUS_API_KEY"),
#                                state = c("TX"), age = F, sex = F,
#                                census.geo = "tract")
# 
# wru_place <- get_census_data(key = Sys.getenv("CENSUS_API_KEY"),
#                              state = c("TX"), age = F, sex = F,
#                              census.geo = "place")
# 
# save(wru_tract, wru_place, file = here::here("data", "wru_census_tx.RData"))

load(here::here("data", "wru_census_tx.RData"))

#Shannon: clean up harris county defendant names here
harris_wru <- cases %>% 
  select(geoid,
         hcad_num, case_number, case_file_date, 
         plaintiff_name, plaintiff_atty_name,
         defendant_name, defendant_atty_name) %>% 
  mutate(
    state = "TX",
    county = substr(geoid, 3,5),
    tract = substr(geoid, 6,11),
    defendant_name = tolower(defendant_name),
    other_occ = ifelse(str_detect(defendant_name, "occupant"), 1, 0)
  )

harris_wru_commas <- harris_wru %>% 
  filter(other_occ==0) %>% 
  separate(defendant_name, into=c("surname", "first_middle"), sep = ", ") %>% 
  dplyr::select(case_number, surname)

harris_wru_others <- harris_wru %>% 
  filter(other_occ==1) %>% 
  mutate(
    defendant_name = str_replace_all(defendant_name, "and all other occupants", ""),
    defendant_name = str_replace_all(defendant_name, "and all  other occupants", ""),
    defendant_name = str_replace_all(defendant_name, "& all other occupants", ""),
    defendant_name = str_replace_all(defendant_name, "and all occupants", ""),
    defendant_name = str_replace_all(defendant_name, "& all occupants", ""),
    defendant_name = str_replace_all(defendant_name, " occupants ", ""),
    defendant_name = str_replace_all(defendant_name, " and ", ""),
    defendant_name = str_replace_all(defendant_name, " all ", ""),
    defendant_name = str_replace_all(defendant_name, " other ", ""),
    defendant_name = str_replace_all(defendant_name, "&", ""),
    defendant_name = str_replace_all(defendant_name, "and/or", ""),
    defendant_name = str_replace_all(defendant_name, "allof", ""),
    defendant_name = str_replace_all(defendant_name, " occupants ", ""),
    defendant_name = str_replace_all(defendant_name, " occupants", ""),
    defendant_name = str_replace_all(defendant_name, " other ", ""),
    defendant_name = str_replace_all(defendant_name, "otheroccupants", ""),
    defendant_name = str_replace_all(defendant_name, "other occupants", ""),
    
    defendant_name = str_replace_all(defendant_name, ",", ""),
    defendant_name = str_replace_all(defendant_name, "\\.", ""),
    
    unknown = ifelse(str_detect(defendant_name, "unknown"), 1, 0),
    
    de_place = ifelse(str_detect(defendant_name, " de "),
                      str_locate(defendant_name, " de "), NA) ,
    
    defendant_name = str_trim(defendant_name),
    word_count = str_count(defendant_name, '\\w+'),
    surname = stringr::word(defendant_name, 2),
    surname = ifelse(nchar(surname)==1, stringr::word(defendant_name, 3), surname),
    surname = ifelse(word_count==3, stringr::word(defendant_name, -1), surname),
    surname = ifelse(nchar(surname)==1, stringr::word(defendant_name, 2), surname),
    surname = ifelse(unknown==1, "", surname),
    surname = ifelse(surname=='jr', stringr::word(defendant_name, -2), surname),
    surname = ifelse(!is.na(de_place), 
                     str_trim(substr(defendant_name, de_place, nchar(defendant_name))), surname)
  )%>% 
  dplyr::select(case_number, surname)


predict_race(voter.file = geoloc, 
               census.geo = "tract", 
               census.key = Sys.getenv("CENSUS_API_KEY")) 
predict_race(voter.file = geoloc, 
             census.data = wru_tract,
             census.geo = "tract", 
             census.key = Sys.getenv("CENSUS_API_KEY")) 