#########################
# created: sept 17 2019
#
# purpose: process corn yields
#
# NOTES: final data is in DRY Mg/ha
#
# last updated: 10/6/2020 (adding 2020 data, cleaning things up)
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


#--2008 onwards plots were managed the same.
#--2003-2007 the C2 was conventional herb and C3/C4 were low input herb (ie tillage)
#--currently plot key only goes back to 2012, so yields are only presesnted for 2012-2017
dhist <- read_csv("data-raw/yields/raw/rd_cornyld-2012-2017.csv")


d18 <-
  read_csv("data-raw/yields/raw/rd_cornyld-2018.csv", skip = 4) %>%
  mutate(trt = gsub("[[:punct:]]+", "", trt)) %>%
  rename(harv_crop = rot) %>%
  group_by(plot, harv_crop) %>%
  summarise(yield_Mgha = (1-0.155) * mean(yield_Mgha155, na.rm = T)) %>%
  mutate(year = 2018)


d19 <-
  read_csv("data-raw/yields/raw/rd_cornyld-2019.csv", skip = 4) %>%
  fill(plot, rot) %>%
  mutate(trt = gsub("[[:punct:]]+", "", trt)) %>%
  rename(harv_crop = rot) %>%
  group_by(plot, harv_crop) %>%
  summarise(yield_Mgha = (1-0.155) * mean(yield_Mgha155, na.rm = T)) %>%
  mutate(year = 2019)


d20 <-
  read_csv("data-raw/yields/raw/rd_cornyld-2020.csv", skip = 5) %>%
  mutate(trt = str_to_lower(trt)) %>%
  rename(harv_crop = rot) %>%
  group_by(plot, harv_crop) %>%
  summarise(yield_Mgha = (1-0.155) * mean(yield_Mgha155, na.rm = T)) %>%
  mutate(year = 2020)




mrs_cornylds <-
  d18 %>%
  bind_rows(d19) %>%
  bind_rows(d20) %>%
  left_join(pk) %>%
  ungroup() %>%
  select(plot_id, yield_Mgha) %>%
  bind_rows(dhist) %>%
  arrange(plot_id)


mrs_cornylds %>%  write_csv("data-raw/yields/mrs_cornylds.csv")

usethis::use_data(mrs_cornylds, overwrite = T)

#--quick look
library(ggplot2)
mrs_cornylds %>%
  left_join(pk) %>%
  ggplot(aes(rot_trt, yield_Mgha)) +
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 2) +
  facet_grid(.~year)

ggsave("data-raw/yields/fig_corn-yields.png")
