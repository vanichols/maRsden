#############################
#
# created: Dec 16 2019
# Process FACTS weather data from Ames site. Need to add 2020 still
#
# last updated: feb 18 2020 (added dates)
#                oct 13 2020 (replaced it with qc'd facts weather)
#               aug 7 2021 (need 2020 data, adding more years to pkg data)
#
##############################

rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)

# wea ----------------------------------------------------------------

wea <- read_excel("data-raw/weather/raw_wea_ames-facts-1980-2019.xlsx")

#--not qc'd, mesonet data for 2020
wea20 <-
  read_excel("data-raw/weather/nwscoop_ames-2020.xlsx") %>%
  mutate(year = year(day),
         date = ymd(day)) %>%
  select(year, date, doy, highc, lowc, precipmm, narr_srad) %>%
  rename("day" = doy,
         "maxt_c" = highc,
         "mint_c" = lowc,
         "rain_mm" = precipmm,
         "radn_MJm2" = narr_srad)

# wrangle -----------------------------------------------------------------

doy_tib <- tibble::tibble(
  date = seq(lubridate::ymd("1980-01-01"), lubridate::ymd("2020-12-31"), by = "1 day")) %>%
  dplyr::mutate(day = lubridate::yday(date),
                year = lubridate::year(date))

#--get date
mrs_wea <-
  wea %>%
#  filter(year > 2017) %>%
  left_join(doy_tib) %>%
  bind_rows(wea20) %>%
  select(date, year, day, everything())


mrs_wea %>% write_csv("data-raw/weather/mrs_wea.csv")
usethis::use_data(mrs_wea, overwrite = T)
