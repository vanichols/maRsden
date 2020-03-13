########################
#
# Date of creation: march 13 2020
#
# last updated:
#
# Purpose: Process 2018 and 2019 phenology data
#
# NOTES:
#
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

mydir <- "data-raw/phen/"


# 2018 data ---------------------------------------------------------------


praw18 <- read_excel("data-raw/phen/rd_mars-phenology.xlsx",
                      skip = 5)


p18 <-
  praw18 %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date),
         harv_crop = trt,
         block = paste0("b", rep),
         pl_id = paste0("p", plot, "-", phen_plno)) %>%
  left_join(pk) %>%
  #--name things nicer
  rename(pls_nu = phen_nopl,
         plht_cm = phen_plht_cm,
         pl_stage = phen_stage,
         devleaves_nu = phen_nodevleaves,
         grleaves_nu = phen_nogreenleaves,
         funleaves_nu = phen_nofunctleaves) %>%
  select(year, date, doy, plot_id, pls_nu,
         pl_id, plht_cm, pl_stage,
         devleaves_nu, grleaves_nu, funleaves_nu)


# 2019 data ----------------------------------------------------


praw19 <-
  tibble(files = list.files(mydir)) %>%
  mutate(path = paste0(mydir, files)) %>%
  filter(grepl('phen', files)) %>%
  filter(grepl('2019', files)) %>%
  filter(grepl('.xlsx', files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  fill(date, plot, totpl_no)

p19 <-
  praw19 %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date)) %>%
  left_join(pk) %>%
  #--deal w/ plant heights
  mutate(plht_cm = ifelse(is.na(plht_cm), plht_in * 2.54, plht_cm),
         pl_id = paste0("p", plot, "-", rep),
         funleaves_nu = GL_no) %>%
  rename(pls_nu = totpl_no,
         pl_stage = stage,
         devleaves_nu = potL_no,
         grleaves_nu = GL_no) %>%
  select(year, date, doy, plot_id, pls_nu,
         pl_id, plht_cm, pl_stage,
         devleaves_nu, grleaves_nu, funleaves_nu)


# combine -----------------------------------------------------------------

mrs_phen <-
  p18 %>%
  bind_rows(p19) %>%
  arrange(year, date, doy, plot_id)

# make lai and biomass datasets -------------------------------------------

mrs_phen %>%  write_csv("data-raw/phen/phen.csv")

usethis::use_data(mrs_phen, overwrite = T)
