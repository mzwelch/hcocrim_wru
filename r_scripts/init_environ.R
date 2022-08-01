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

#### read in data and convert relevant fields to prepare for predict_race()####
#census data
#save census key
# Add key to .Renviron
Sys.setenv(CENSUS_API_KEY="490cdf0e468ffc29cd51ec5fd2eddf11c8f7b225")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key
Sys.getenv("CENSUS_API_KEY")
