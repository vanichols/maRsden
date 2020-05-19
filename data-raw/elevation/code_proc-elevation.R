########################
#
# Date of creation: March 13 2020
#
# Author: Gina
#
# Purpose: Process 2018 and 2019 biomass and associated LAI data
#
# NOTES:
#
#
# Last updated: Feb 18 2020: added date
#
#########################


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data

pk <- read_csv("data-raw/plotkey/plotkey.csv") %>%
  filter(year == 2018)


# 2018 data ---------------------------------------------------------------

graw <- read_excel("data-raw/earrows/rd_mars-grain-samples-2018.xlsx",
                   skip = 5,
                   na = "NA")


mrs_earrows <-
  graw %>%
  mutate(date = ymd(date),
         year = year(date),
         doy = yday(date),
         harv_crop = trt,
         ear_id = paste0("p", plot, "-", ear_no),
         rows_nu = row_no) %>%
  left_join(pk, by = c("plot", "harv_crop", "year")) %>%
  select(year, date, doy, plot_id, ear_id, rows_nu)

mrs_earrows %>%  write_csv("data-raw/earrows/earrows.csv")

usethis::use_data(mrs_earrows, overwrite = T)
