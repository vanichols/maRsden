#########################
# created: oct 20 2020
#
# purpose: process historical corn yields received from ML
#
# NOTES: final data must be in DRY Mg/ha
#
# last updated:
#
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(lubridate)
library(stringr)
library(janitor) #--used to clean data
library(readxl)

pk <- read_csv("data-raw/plotkey/plotkey.csv")

#--final desired prod for ref
read_csv("data-raw/yields/mrs_cornylds.csv")

# corn --------------------------------------------------------------------

dhis <- read_excel("data-raw/yields/raw/Marsden 2003-17 Corn Yields.xlsx", skip = 5) %>%
  clean_names() %>%
  rename(harv_crop = "trt")

#--plots only start in 2012, need to update that...

dat <-
  pk %>%
  left_join(dhis %>% mutate(block = paste0("b", block))) %>%
  mutate(yield_Mgha = mg_ha_15_5_percent_moisture * (1-0.155)) %>%
  filter(!is.na(yield_Mgha))

#--viz check
dat %>%
  ggplot(aes(year, yield_Mgha, color = rot_trt, shape = harv_crop)) +
  geom_point() +
  geom_line() +
  facet_grid(.~block)

#--keep only plot_id and yields
dat2 <-
  dat %>%
  select(plot_id, yield_Mgha)


dat2 %>%  write_csv("data-raw/yields/raw/rd_cornyld-2012-2017.csv")
