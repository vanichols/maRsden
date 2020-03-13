#########################
# created: Dec 19 2019
#
# updated: march 13 2020 (new pkg structure)
#
# purpose: Process 2018 residue samples
#
# NOTES:
#
#########################

rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) # used to read Excel files

pk <- read_csv("data-raw/plotkey/plotkey.csv")

resraw <- read_excel("data-raw/residue/rd_mars-residue.xlsx", skip = 5, na = "NA") %>%
  select(-notes)


# process -----------------------------------------------------------------

mrs_residue <-
  resraw %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date),
         residue_g = residueandbag_g - drybag_g,
         block = tolower(block),
         harv_crop = trt) %>%
  left_join(pk) %>%
  group_by(year, date, doy, plot_id) %>%
  summarise(residue_g = mean(residue_g, na.rm = T))



# save it -----------------------------------------------------------------

mrs_residue %>% write_csv("data-raw/residue/residue.csv")
usethis::use_data(mrs_residue, overwrite = T)
