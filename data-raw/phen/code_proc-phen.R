# Date of creation: march 13 2020
# Purpose: Process all phenology data
# NOTES:
#
#
# last updated: 6/12/2020, include 2020 data


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data
library(fuzzyjoin) #--to do fuzzy joining of dates

#--this is the plotkey data available in the package
data("mrs_plotkey")

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
  left_join(mrs_plotkey %>% filter(year == 2018)) %>%
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


#--2018 also did phenology 'unofficially' the days I sampled roots

p18sup <-
  read_excel("data-raw/rootdepth/rd_rootdepth18.xlsx", skip = 5) %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date),
         harv_crop = trt,
         block = paste0("b", block)) %>%
  left_join(mrs_plotkey %>% filter(year == 2018)) %>%
  select(year, date, doy, plot_id, stage) %>%
  filter(stage != "planting") %>%
  distinct() %>%
  rename("pl_stage" = stage)


p18all <-
  p18 %>%
  bind_rows(p18sup)


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
  left_join(mrs_plotkey %>% filter(year == 2019)) %>%
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


# 2020 data ----------------------------------------------------


praw20 <-
  tibble(files = list.files(mydir)) %>%
  mutate(path = paste0(mydir, files)) %>%
  filter(grepl('phen', files)) %>%
  filter(grepl('2020', files)) %>%
  filter(grepl('.xlsx', files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  fill(date, plot)

p20 <-
  praw20 %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date)) %>%
  left_join(mrs_plotkey %>% filter(year == 2020)) %>%
  mutate(plht_cm = NA,
         totpl_no = NA,
         pl_id = paste0("p", plot, "-", rep),
         greenleaf_nu = ifelse(is.na(greenleaf_nu), potleaf_nu - deadleaf_nu, greenleaf_nu),
         funleaves_nu = ifelse(is.na(funleaves_nu), greenleaf_nu, funleaves_nu),
         stage = paste0(class, stage)) %>%
  rename(pls_nu = totpl_no,
         pl_stage = stage,
         devleaves_nu = potleaf_nu,
         grleaves_nu = greenleaf_nu) %>%
  select(year, date, doy, plot_id, pls_nu,
         pl_id, plht_cm, pl_stage,
         devleaves_nu, grleaves_nu, funleaves_nu)



# combine -----------------------------------------------------------------

mrs_phen <-
  p18all %>%
  bind_rows(p19) %>%
  bind_rows(p20) %>%
  arrange(year, date, doy, plot_id)

# save-------------------------------------------

usethis::use_data(mrs_phen, overwrite = T)
