#########################
# created: Dec 17 2019
#
# last updated: feb 18 2020 (added dates)
#              2019 planting has too many plots
#              march 13 2020 (updated pkg structure, fixed phenology)
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
#library(fuzzyjoin) #--didn't work for me

pk <- read_csv("data-raw/plotkey/plotkey.csv")


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
  left_join(pk) %>%
  ungroup() %>%
  mutate(rootdepth_cm = rootdepth_in * 2.54,
         pl_stage = stage) %>%
  select(year, date, doy, plot_id, pl_stage, rootdepth_cm)



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
  rename("date" = "date3")


#--note this doesn't have a 'planting' data point
rd19 <- rd19raw %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date)) %>%
  left_join(pk) %>%
  select(year, date, doy, plot_id, mrd_cm) %>%
  rename("rootdepth_cm" = mrd_cm)


# have to get phenology to merge with it

phenroots <-
  read_csv("data-raw/phen/phen.csv") %>%
  filter(year == 2019) %>%
  select(year, date, doy, plot_id, pl_stage) %>%
  mutate(
    #--replace the one VT with R1
    pl_stage = ifelse(pl_stage == "VT", "R1", pl_stage),
    #--wyatt forgot to do 2019_21 on day 213, just make it R1
    pl_stage = ifelse( (plot_id == "2019_21" & doy == 213), "R1", pl_stage),
    #--separate into numbers/letters
    snum = as.numeric(str_sub(pl_stage, 2)),
    slet = str_sub(pl_stage, 1, 1)) %>%
  # find average stage for a plot
  group_by(year, date, doy, plot_id, slet) %>%
  summarise(snum_mn = mean(snum, na.rm=T),
            snum_mn = round(snum_mn, 0)) %>%
  ungroup() %>%
  mutate(pl_stage = paste0(slet, snum_mn)) %>%
  left_join(pk) %>%
  select(year, date, doy, plot_id, pl_stage) %>%
  # make dummy doy2 to merge w/roots
  mutate(doy_tmp = case_when(
      doy == 190 ~ 189,
      doy == 205 ~ 203,
      doy == 213 ~ 212,
      doy == 232 ~ 231,
      TRUE ~ doy)) %>%
  select(-doy, -date)

#--combine root data w/phen
rdphen19 <-
  rd19 %>%
  left_join(phenroots, by = c("doy" = "doy_tmp",
                              "year", "plot_id")) %>%
  #--no phen 9/17, say R6
  mutate(pl_stage = ifelse(doy == 260, "R6", pl_stage))

#--make a dummy planting (6/3/2020) data point w/rootdepth = 0
plant19 <-
  pk %>%
  filter(year == 2019, rot_trt %in% c("C4", "C2")) %>%
  select(plot_id) %>%
  unique() %>%
  mutate(date = ymd("2019-06-03"),
         year = year(date),
         doy = yday(date),
         pl_stage = "planting",
         rootdepth_cm = 0)

rd19phen2 <-
  rdphen19 %>%
  bind_rows(plant19)

# combine 2018 and 2019 data ----------------------------------------------

mrs_rootdepth <-
  bind_rows(rd18, rd19phen2) %>%
  arrange(year, date, doy, plot_id)

mrs_rootdepth %>% write_csv("data-raw/rootdepth/rootdepth.csv")
usethis::use_data(mrs_rootdepth, overwrite = T)
