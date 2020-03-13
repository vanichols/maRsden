#########################
# created: march 13 2020
#
# last updated:
#
# purpose: Process soil nitrate tests from Castellano's lab
#
# NOTES: Only have 2018 data right now. 2019 was a mess bc Javed was leaving
#
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(tidyverse)
library(lubridate)
library(janitor) #--used to clean data

pk <- read_csv("data-raw/plotkey/plotkey.csv") %>%
  filter(year %in% c(2018, 2019))


# 2018 --------------------------------------------------------------------

snraw <- read_csv("data-raw/soiln/tidy-soilN-marsden.csv") %>%
  clean_names()


sn19 <-
  snraw %>%
  mutate(
    #--dates
    date = as_date(date),
    year = year(date),
    doy = yday(date),
    #--depths
    depth_cm = case_when(
      str_detect("01", depth) ~ "0-30",
      str_detect("12", depth) ~ "30-60",
      str_detect("23", depth) ~ "60-90"),
    depth_cat = case_when(
      str_detect("01", depth) ~ 1,
      str_detect("12", depth) ~ 2,
      str_detect("23", depth) ~ 3)
    ) %>%
  left_join(pk) %>%
  select(year, date, doy, plot_id,
         depth_cat, depth_cm,
         no3_ppm, nh4_ppm,
         no3_kg, nh4_kg) %>%
  arrange(year, date, doy, plot_id)


# write it ----------------------------------------------------------------


mrs_soiln <- sn19

mrs_soiln %>% write_csv("data-raw/soiln/soiln.csv")
usethis::use_data(mrs_soiln, overwrite = T)
