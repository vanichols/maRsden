#########################
# created: Dec 17 2019
#
# last updated: feb 18 2020 (added dates)
#              2019 planting has too many plots
#              march 13 2020 (updated pkg structure, fixed phenology)
#              june 12 2020 (add 2020 data)
#
# purpose: Process max root depth data from 2018 and 2019
#
# NOTES:
#
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data
library(maRsden)
data("mrs_plotkey")


pk <-
  mrs_plotkey %>%
  filter(year > 2017)

# 2018 --------------------------------------------------------------------

rd18raw <- read_excel("data-raw/rootdepth/rd_rootdepth18.xlsx", skip = 5)

rd18 <-
  rd18raw %>%
  mutate(date = as_date(date)) %>%
  group_by(date, plot, trt, stage, block) %>%
  summarise(rootdepth_in = mean(rootdepth_in, na.rm = T)) %>%
  mutate(year = year(date),
         doy = yday(date),
         block = paste0("b", block)) %>%
  left_join(pk %>% filter(year == 2018)) %>%
  ungroup() %>%
  mutate(rootdepth_cm = rootdepth_in * 2.54,
         pl_stage = stage) %>%
  select(date, doy, plot_id, rootdepth_cm)



# 2019 --------------------------------------------------------------------

myrootdir <- "data-raw/rootdepth/"

rd19raw <-
  tibble(files = list.files(myrootdir)) %>%
  mutate(path = paste0(myrootdir, files)) %>%
  # keep only maxrootdepth ones
  filter(grepl('maxrootdepth', files)) %>%
  filter(grepl('2019', files)) %>%
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
  rename("date" = "date3") %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date)) %>%
  left_join(pk %>% filter(year == 2019)) %>%
  select(date, doy, plot_id, mrd_cm) %>%
  rename("rootdepth_cm" = mrd_cm)

#--make a dummy planting (6/3/2020) data point w/rootdepth = 0
plant19 <-
  pk %>%
  filter(year == 2019, rot_trt %in% c("C4", "C2")) %>%
  select(plot_id) %>%
  unique() %>%
  mutate(date = ymd("2019-06-03"),
         doy = yday(date),
         rootdepth_cm = 0)

rd19 <-
  rd19raw %>%
  bind_rows(plant19)



# 2020 data ---------------------------------------------------------------

#--corn was planted 4/23/2020
plant20 <-
  pk %>%
  filter(year == 2020, rot_trt %in% c("C4", "C2")) %>%
  select(plot_id) %>%
  unique() %>%
  mutate(date = ymd("2020-04-23"),
         doy = yday(date),
         rootdepth_cm = 0)


rd20raw <-
  tibble(files = list.files(myrootdir)) %>%
  mutate(path = paste0(myrootdir, files)) %>%
  # keep only maxrootdepth ones
  filter(grepl('maxrootdepth', files)) %>%
  filter(grepl('2020', files)) %>%
  filter(!grepl("20200522", files)) %>% #--this first day was a bust
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
  mutate(date = as_date(date),
         doy = yday(date)) %>%
  left_join(pk %>% filter(year == 2020)) %>%
  select(date, doy, plot_id, mrd_cm) %>%
  rename("rootdepth_cm" = mrd_cm)

rd20 <-
  rd20raw %>%
  bind_rows(plant20) %>%
  arrange(date, plot_id)

# combine 2018, 2019, 2020 data ----------------------------------------------

mrs_rootdepth <-
  bind_rows(rd18, rd19, rd20) %>%
  arrange(date, plot_id)

usethis::use_data(mrs_rootdepth, overwrite = T)
