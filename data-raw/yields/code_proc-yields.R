#########################
# created: sept 17 2020
#
# purpose: process corn yields
#
# NOTES:
#
# last updated:
#
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(janitor) #--used to clean data




pk <- read_csv("data-raw/plotkey/plotkey.csv")

mrs_cornylds <-
  read_csv("data-raw/yields/raw/td_corn-yields.csv") %>%
  mutate(block = paste0("b", block)) %>%
  rename("harv_crop" = "trt") %>%
  left_join(pk) %>%
  arrange(year)


mrs_cornylds %>%  write_csv("data-raw/yields/mrs_cornylds.csv")

usethis::use_data(mrs_cornylds, overwrite = T)


