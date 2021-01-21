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
  filter(year %in% c(2018, 2020))


# 2018 data ---------------------------------------------------------------

graw18 <- read_excel("data-raw/cornyldcomp/rd_mars-grain-samples-2018.xlsx",
                   skip = 5,
                   na = "NA")


mrs_earrows18 <-
  graw18 %>%
  mutate(date = ymd(date),
         year = year(date),
         doy = yday(date),
         harv_crop = trt,
         ear_id = paste0("p", plot, "-", ear_no),
         rows_nu = row_no) %>%
  left_join(pk, by = c("plot", "harv_crop", "year")) %>%
  select(year, date, doy, plot_id, ear_id, rows_nu)


# 2020 data ---------------------------------------------------------------

graw20 <- read_excel("data-raw/cornyldcomp/rd_mars-grain-samples-2020.xlsx",
                   skip = 5,
                   na = "NA")

mrs_earrows20 <-
  graw20 %>%
  fill(date, plot) %>%
  mutate(date = ymd(date),
         year = year(date),
         doy = yday(date),
         ear_id = paste0("p", plot, "-", ear_no),
         rows_nu = row_no,
         plot_id = paste(year, plot, sep = "_")) %>%
  select(year, date, doy, plot_id, ear_id, rows_nu)



# bind together -----------------------------------------------------------

mrs_earrows <-
  mrs_earrows18 %>%
  bind_rows(mrs_earrows20)

library(beeswarm)
library(ggbeeswarm)
mrs_earrows %>%
  left_join(pk) %>%
  separate(plot_id, into = c("year", "plot")) %>%
  ggplot(aes(rot_trt, rows_nu)) +
  geom_jitter(aes(color = block, shape = rot_trt), size = 5) +
  facet_grid(.~year)



mrs_earrows %>%  write_csv("data-raw/cornyldcomp/mrs_earrows.csv")

usethis::use_data(mrs_earrows, overwrite = T)
