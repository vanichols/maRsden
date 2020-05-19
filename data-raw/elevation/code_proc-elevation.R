########################
#
# Date of creation: May 19 2020
#
# Author: Gina
#
# Purpose: Process elevation data from Jess/GIS
#
# NOTES:
#
#
# Last updated:
#
#########################


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data

pk <- read_csv("data-raw/plotkey/plotkey.csv") %>%
  filter(year == 2018)

elev_raw <- read_excel("data-raw/elevation/raw-Jess/marsden_Table.xls")

b1 <- seq(11, 19)
b2 <- seq(21, 29)
b3 <- seq(31, 39)
b4 <- seq(41, 49)


mrs_elevation <-
  elev_raw %>%
  filter(!(Id %in% c(0, 10, 29))) %>%
  mutate(plot = c(b1, b2, b3, b4)) %>%
  clean_names() %>%
  select(-objectid, -id, -sum, -variety) %>%
  pivot_longer(min:median) %>%
  mutate(value = value/100) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  rename(mean_elev_m = mean) %>%
  select(plot, count, area, mean_elev_m, everything())


mrs_elevation %>%
  ggplot(aes(plot, mean_elev_m)) +
  geom_point()

mrs_elevation %>%  write_csv("data-raw/elevation/elevation.csv")

usethis::use_data(mrs_elevation, overwrite = T)
