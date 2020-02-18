#########################
## Created: Dec 17 2019
## Author: Gina
## Purpose: Process max root depth data from 2018 and 2019
##
## NOTES:
##
## last updated: feb 18 2020 (added dates)
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data
#library(fuzzyjoin) #--didn't work for me

pk <- read_csv("data-raw/_tidy/plotkey.csv")


# 2018 --------------------------------------------------------------------

rd18raw <- read_excel("data-raw/_raw/vn/2018/rd_rootdepth18.xlsx", skip = 5)

rd18 <-
  rd18raw %>%
  mutate(date = as_date(date)) %>%
  group_by(date, plot, trt, stage, block) %>%
  summarise(rootdepth_in = mean(rootdepth_in, na.rm = T)) %>%
  mutate(year = year(date),
         doy = yday(date),
         block = paste0("b", block)) %>%
  left_join(pk) %>%
  ungroup() %>%
  mutate(rootdepth_cm = rootdepth_in * 2.54) %>%
  select(year, date, doy, plot_id, stage, rootdepth_cm)



# 2019 --------------------------------------------------------------------

myrootdir <- "data-raw/_raw/vn/2019/"

rd19raw <-
  tibble(files = list.files(myrootdir)) %>%
  mutate(path = paste0(myrootdir, files)) %>%
  # keep only maxrootdepth ones
  filter(grepl('maxrootdepth', files)) %>%
  # make sure I don't get .txt files by mistake
  filter(grepl('.xlsx', files)) %>%
  # read each file
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  fill(date, plot) %>%
  mutate(mrd_cm = ifelse(is.na(maxrootdepth_cm), maxrootdepth_in*2.54, maxrootdepth_cm)) %>%
  group_by(date, plot) %>%
  summarise(mrd_cm = mean(mrd_cm, na.rm = T)) %>%
  ungroup() %>%
  mutate(date = as_date(date)) %>%
  # sampled over two days, fix it
  mutate(date2 = ifelse(date == as.Date("2019-09-16"), as.Date("2019-09-17"), as.Date(date)),
         date3 = as_date(date2)) %>%
  select(-date, -date2) %>%
  rename("date" = "date3")


rd19 <- rd19raw %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date)) %>%
  left_join(pk) %>%
  select(year, date, doy, plot_id, mrd_cm) %>%
  rename("rootdepth_cm" = mrd_cm)

# have to get phenology to merge with it. hmm

phenraw <-
  tibble(files = list.files(myrootdir)) %>%
  mutate(path = paste0(myrootdir, files)) %>%
  filter(grepl('phen', files)) %>%
  filter(grepl('.xlsx', files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  fill(date, plot) %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date)) %>%
  select(year, date, doy, plot, rep, stage) %>%
  # replace the one VT with R1
  mutate(stage = ifelse(stage == "VT", "R1", stage)) %>%
  mutate(stagenum = as.numeric(str_sub(stage, 2)),
           stagelet = str_sub(stage, 1, 1)) %>%
  # find average stage for a plot
  group_by(year, date, doy, plot, stagelet) %>%
  summarise(stage = mean(stagenum, na.rm=T),
            stage2 = round(stage, 0)) %>%
  ungroup() %>%
  mutate(stage = paste0(stagelet, stage2)) %>%
  left_join(pk) %>%
    select(year, date, doy, plot_id, stage) %>%
  # wyatt forgot to do 2019_21 on day 213, just make it R1
  mutate(stage = ifelse( (plot_id == "2019_21" & doy == 213), "R1", stage)) %>%
  # make dummy doy2 to merge w/roots
    mutate(doy2 = case_when(
      doy == 190 ~ 189,
      doy == 205 ~ 203,
      doy == 213 ~ 212,
      doy == 232 ~ 231,
      TRUE ~ doy)) %>%
  select(-doy)

phenraw

rd19phen <- rd19 %>%
  mutate(doy2 = doy) %>%
  left_join(phenraw) %>%
  select(-doy2)


# planting happened June 3 2019. Add that dummy spot I guess
rd19dat <-
  pk %>%
  filter(year == 2019) %>%
  select(plot_id) %>%
  unique() %>%
  mutate(date = ymd("2019-06-03"),
         year = year(date),
         doy = yday(date),
         stage = "planting",
         rootdepth_cm = 0) %>%
  #select(-date) %>%
  bind_rows(rd19phen) %>%
  arrange(year, date, doy, plot_id)

# combine 2018 and 2019 data ----------------------------------------------

mrs_rootdepth <- bind_rows(rd18, rd19dat)


mrs_rootdepth %>% write_csv("data-raw/_tidy/rootdepth_vn.csv")
usethis::use_data(mrs_rootdepth, overwrite = T)
