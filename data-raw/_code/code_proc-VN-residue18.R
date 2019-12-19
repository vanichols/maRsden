#########################
## Created Dec 19 2019
## Author Gina
## Purpose: Process 2018 residue samples
## NOTES:
##
#########################

rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) # used to read Excel files

pk <- read_csv("data-raw/_tidy/plotkey.csv")

resraw <- read_excel("data-raw/_raw/vn/2018/rd_mars-residue.xlsx", skip = 5, na = "NA") %>%
  select(-notes)


# process -----------------------------------------------------------------

mrs_residue18 <-
  resraw %>%
  mutate(year = year(date),
         doy = yday(date),
         residue_g = residueandbag_g - drybag_g,
         block = tolower(block),
         harv_crop = trt) %>%
  left_join(pk) %>%
  group_by(year, doy, plot_id) %>%
  summarise(residue_g = mean(residue_g, na.rm = T))



# save it -----------------------------------------------------------------

mrs_residue18 %>% write_csv("data-raw/_tidy/residue18_vn.csv")
usethis::use_data(mrs_residue18, overwrite = T)
