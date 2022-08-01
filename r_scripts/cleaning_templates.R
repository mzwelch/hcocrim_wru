## this script is for managing the data cleaning pipeline
## used to keep track of which indicators' data have been cleaned, bind indicator data together, and save
## also determine next indicator to clean and push combined data to github





# Datalab dashboard: https://datalab.texas2036.org/
# Knoema documentation: https://github.com/Knoema/knoema-r-driver

###--- read in indicator inventory -----
copy_goals = read_sheet("https://docs.google.com/spreadsheets/d/1kDehMQ8SXnA7TcVAwaTfLQcuIL90N5OCK7g1RBDgWkM/edit#gid=0",
                        sheet = "goals") %>% 
  left_join(read_sheet("https://docs.google.com/spreadsheets/d/1kDehMQ8SXnA7TcVAwaTfLQcuIL90N5OCK7g1RBDgWkM/edit#gid=0",
                       sheet = "pillars"))

copy_indicators = read_sheet("https://docs.google.com/spreadsheets/d/1kDehMQ8SXnA7TcVAwaTfLQcuIL90N5OCK7g1RBDgWkM/edit#gid=339762969",
                             sheet = "indicators")

ready_indicators = copy_indicators %>% 
  filter(!is.na(data_lab_url)) %>% 
  select(metric_id, data_lab_url, everything())

###--- source cleaning scripts: 1 script/indicator ----
#setwd(here::here("r-scripts"))
# source("early_learning_4th_grade_reading.R")
# source("early_learning_prek.R")
# source("early_learning_primary.R")
# source("early_learning_kinder_readiness.R")
##add in new cleaning scripts, keep commented out for now
#HERE


###--- save and combine each indicator's clean data ----
# not really sure what to call these when reading them in...
# goal numbers can change which could be confusing, 
# but want something short and uniform just for readability and ease

# FOR loop, read and bind everything
#names of clean data tables
clean_data_list = list.files(here::here("data-clean","indicator_tables"))
#initialize combined table
combined = tibble()
for (i in seq_along(clean_data_list)){
  goal = readRDS(here::here("data-clean", "indicator_tables", clean_data_list[i])) #grab next clean .rds
  combined = rbind(combined, goal) #rbind to combined
}
#save combined
saveRDS(combined, file = here::here("data-clean", "dashboard_df.rds"))
#uncomment to view list and number of tables in dashboard_df.rds
#clean_data_list
#length(clean_data_list)

#add new cleaning file, add newest script to rbind call, combined data will be every clean indicator


###--- identify next indicator(s) to clean ----
files_processed = combined %>% 
  select(metric_id) %>% 
  distinct() 

file_inventory = files_processed %>% 
  mutate(cleaned = "yes") %>% 
  right_join(ready_indicators) %>% 
  mutate(cleaned = case_when(is.na(cleaned) ~ "no",
                             T ~ cleaned)) %>% 
  distinct()

next_indicator = file_inventory %>% 
  filter(cleaned == "no") #%>% 
#slice(1)

#add a flag to "skip"

# make a new script in rscripts w this name:
pull(next_indicator, metric_id)

