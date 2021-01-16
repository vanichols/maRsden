########################
#
# Date of creation: Jan 15 2021
# Author: Gina
# Purpose: Combine Will and my data, we were inconsistent with husks/cobs so it will be less nuanced
# NOTES:
#
# Last updated:
#
#########################


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data

pk <- read_csv("data-raw/plotkey/plotkey.csv")


wo <- read_csv("data-raw/cornbio_wo/cornbio_wo.csv")

gn <- read_csv("data-raw/cornbio_gn/cornbio_gn.csv")


# just keep full plant data -----------------------------------------------


mrs_cornbio <-
  wo %>%
  filter(organ == "plant") %>%
  mutate(mass_gpl = mass_g/4) %>% #--will did 4 corn plants
  bind_rows(gn %>% filter(organ == "plant")) %>%
  left_join(pk) %>%
  filter(rot_trt %in% c("2y", "4y")) %>%
  select(year, date, doy, plot_id, mass_gpl)


mrs_cornbio %>%
  left_join(pk) %>%
  ggplot(aes(doy, mass_gpl)) +
  geom_point(aes(color = rot_trt)) +
  facet_grid(.~year)


mrs_cornbio %>%  write_csv("data-raw/cornbio/cornbio.csv")
usethis::use_data(mrs_cornbio, overwrite = T)
