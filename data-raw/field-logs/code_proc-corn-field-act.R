########################
#
# Date of creation: sept 15 2021
# Author: Gina
# Purpose: get easy access to planting/harvesting dates and doys for corn 2013-2020
# NOTES:
#
# Last updated:
#
#########################


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data
library(saapsim) #--for date to doy conversion

fa <- read_excel("data-raw/field-logs/2013-2020_corn-plant-harv-dates.xlsx")


mrs_cornplant <-
  fa %>%
  mutate(plant_date = ymd(corn_plant_date),
         harv_date = ymd(corn_harv_date)) %>%
  rowwise() %>%
  mutate(plant_doy = saf_date_to_doy(plant_date),
         harv_doy = saf_date_to_doy(harv_date)) %>%
  select(year, plant_date, plant_doy, harv_date, harv_doy)


mrs_cornplant %>%  write_csv("data-raw/field-logs/cornplant.csv")
usethis::use_data(mrs_cornplant, overwrite = T)
