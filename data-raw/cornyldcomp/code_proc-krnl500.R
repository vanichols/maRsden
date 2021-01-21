########################
#
# Date of creation: Jan 21 2021
# Author: Gina
# Purpose: Process yield component info
# NOTES:
#
# In 2019, it was always 8 plants
#
#########################


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data

pk <- read_csv("data-raw/plotkey/plotkey.csv")


# 2018 data ---------------------------------------------------------------

# this is a fucking mess.

bm18raw <- read_excel("data-raw/cornbio_gn/rd_mars-destructive_sampling.xlsx",
                      skip = 5,
                      na = "NA") %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date),
         harv_crop = trt,
         block = paste0("b", rep)) %>%
  left_join(pk)


bm18_grain <-
  bm18raw %>%
  select(year, date, doy, plot_id, ds_nopl,
         ds_krnl500_g) %>%
  filter(!is.na(ds_krnl500_g)) %>%
  rename("krnl500_g" = ds_krnl500_g) %>%
  select(year, plot_id, krnl500_g)


# 2019 biomass ----------------------------------------------------

my19dir <- "data-raw/cornbio_gn/2019-data/"

#--biomass

bm19raw <-
  tibble(files = list.files(my19dir)) %>%
  mutate(path = paste0(my19dir, files)) %>%
  filter(grepl('biomass', files)) %>%
  filter(grepl(".xlsx", files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  fill(date, plot)


bm19_grain <-
  bm19raw %>%
  mutate(wgtbag_g = ifelse(is.na(wgtbag_g), 0, wgtbag_g),
         wgt_g = wgtall_g - wgtbag_g) %>%
  select(-wgtall_g, -wgtbag_g) %>%
  mutate(wgt_g = ifelse(is.na(wgt_g), 0, wgt_g)) %>%
  # fix date and plot things
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date)) %>%
  filter(organ == 'kernals500',
         wgt_g > 0) %>%
  left_join(pk) %>%
  rename("krnl500_g" = wgt_g) %>%
  select(year, plot_id, krnl500_g)



# 2020 --------------------------------------------------------------------

my20dir <- "data-raw/cornbio_gn/2020-data/"

#--biomass

bm20raw <-
  tibble(files = list.files(my20dir)) %>%
  mutate(path = paste0(my20dir, files)) %>%
  filter(grepl('biomass', files)) %>%
  filter(grepl(".xlsx", files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  fill(date, plot)


bm20_grain <-
  bm20raw %>%
  # fix date
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date)) %>%
  left_join(pk) %>%
  rename("krnl500_g" = weight_g) %>%
  filter(organ == "kernals500") %>%
  select(year, plot_id, krnl500_g)


# combine my datasets -------------------------------------------

mrs_krnl500 <-
  bind_rows(bm18_grain, bm19_grain, bm20_grain)



mrs_krnl500 %>%
  ggplot(aes(plot_id, krnl500_g)) +
  geom_point() +
  facet_grid(.~year)

mrs_krnl500 %>%  write_csv("data-raw/cornyldcomp/mrs_krnl500.csv")

usethis::use_data(mrs_krnl500, overwrite = T)
