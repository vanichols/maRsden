########################
#
# Date of creation: Dec 18 2019
# Author: Gina
# Purpose: Process 2018 and 2019 biomass and associated LAI data
# NOTES:
#
# In 2019, it was always 8 plants
# # I was inconsistent where I lumped the ear husks, so they are included in stem/tass
# # ear is first measured on 8/1, w/no husk values
# # ear is reported on 8/19, again w/no husk values
# # on 9/20 there is ear, but also husk, cob, kernals.
# #   for this date, the ear value is the sum of cob and kernals (500 were taken out later)
# In 2018, num of plants differed
# # once I started partitioning the ears, the ears value is blank
#
# Last updated: Feb 18 2020: added date
#
#########################


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data
library(fuzzyjoin) #--to do fuzzy joining of dates

#--this is the plotkey data available in the package
pk <- read_csv("data-raw/plotkey/plotkey.csv")

mydir <- "data-raw/cornlai/"


# 2018 data ---------------------------------------------------------------

# this is a fucking mess.

bm18raw <- read_excel("data-raw/cornbio/rd_mars-destructive_sampling.xlsx",
                      skip = 5,
                      na = "NA") %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date),
         harv_crop = trt,
         block = paste0("b", rep)) %>%
  left_join(pk)

lai18 <-
  bm18raw %>%
  mutate(lai_cm2pl = ds_gLAI_cm2 / ds_noplsubsam) %>%
  select(year, date, doy, plot_id, lai_cm2pl)


# 2019 LAI and biomass ----------------------------------------------------

#--LAI

lai19raw <-
  tibble(files = list.files(mydir)) %>%
  mutate(path = paste0(mydir, files)) %>%
  filter(grepl('lai', files)) %>%
  filter(grepl('.xlsx', files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  fill(date)

lai19 <-
  lai19raw %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date)) %>%
  left_join(pk) %>%
  mutate(lai_cm2pl = LAI_cm2 / subsampl_no) %>%
  select(year, date, doy, plot_id, lai_cm2pl) %>%
  arrange(year, date, doy, plot_id)


# make lai and biomass datasets -------------------------------------------

mrs_cornlai <-
  bind_rows(lai18, lai19) %>%
  arrange(year, doy, plot_id)

mrs_cornlai %>%  write_csv("data-raw/cornlai/cornlai.csv")

usethis::use_data(mrs_cornlai, overwrite = T)
