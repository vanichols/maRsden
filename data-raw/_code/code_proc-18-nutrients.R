#########################
## Created Dec 16 2019
## Author Gina
## Purpose: Process Agsource data on soil saamples
## NOTES: 
##
#########################

rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) # used to read Excel files

pk <- read_csv("data-raw/_tidy/plotkey.csv")

agraw <- read_excel("data-raw/_raw/vn/2018/rd_mars-soil_nutrients.xlsx", skip = 5, na = "NA")



# process -----------------------------------------------------------------

mrs_nutrients18 <- 
  agraw %>% 
  mutate(depth = str_sub(samp_id, -1),
         block = tolower(str_sub(samp_id, 1, 2)),
         plot = as.numeric(str_sub(samp_id, 4, 5)),
         trt = str_sub(samp_id, 7, 8),
         depth_cm = ifelse(depth == 1, "0-30", ifelse(depth == 2, "30-60", "60-90")),
         date = as_date("2018-05-16"),
         year = year(date),
         doy = yday(date)) %>%
  gather(PH:SALTS, key = 'msmt', value = 'value') %>%
  mutate(msmt = tolower(msmt)) %>% 
  left_join(pk) %>%
  select(year, doy, plot_id, depth_cm, msmt, value) %>% 
  arrange(year, doy, plot_id, depth_cm, msmt)



# save it -----------------------------------------------------------------

mrs_nutrients18 %>% write_csv("data-raw/_tidy/nutrients18_vn.csv")
usethis::use_data(mrs_nutrients18, overwrite = T)
