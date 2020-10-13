#############################
#
# created: Dec 16 2019
# Process FACTS weather data from Ames site. Need to add 2020 still
#
# last updated: feb 18 2020 (added dates)
#                oct 13 2020 (replaced it with qc'd facts weather)
##############################

rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl)

# wea ----------------------------------------------------------------

wea <- read_excel("data-raw/weather/raw_wea_ames-facts-1980-2019.xlsx")


# wrangle -----------------------------------------------------------------

doy_tib <- tibble::tibble(
  date = seq(lubridate::ymd("2018-01-01"), lubridate::ymd("2020-12-31"), by = "1 day")) %>%
  dplyr::mutate(day = lubridate::yday(date),
                year = lubridate::year(date))

#--get date
mrs_wea <-
  wea %>%
  filter(year > 2017) %>%
  left_join(doy_tib) %>%
  select(date, year, day, everything())


mrs_wea %>% write_csv("data-raw/weather/mrs_wea.csv")
usethis::use_data(mrs_wea, overwrite = T)
