#############################
##
## Dec 16 2019
## Process Will's data
## Writes file to _tidy and put in package
##
## last updated: feb 18 2020 (added date)
##############################

rm(list=ls())
library(tidyverse)
library(lubridate)
library(janitor)


# h2o data ----------------------------------------------------------------


swr <- read_csv("data-raw/_raw/wo/raw_soilH2O13-14.csv", skip = 1)
plotkey <- read_csv("data-raw/_tidy/plotkey.csv")


# wrangle -----------------------------------------------------------------

sw <-
  swr  %>%
  #--fix date
  mutate(date = as.character(date),
         lubedate = mdy(date),
         year = year(lubedate),
         doy = yday(lubedate)) %>%
  select(-date) %>%
  rename(date = lubedate) %>%
  select(year, date, doy, everything()) %>%
  #--gather depths
  gather(`0-10cm_soilH2O_g.g`:`0-30cm_soilH2O_g.g`, key = depth, value = soilh2o_g.g) %>%
  mutate(depth_cm = str_remove(depth, "cm_soilH2O_g.g")) %>%
  #--fix plot/block things
  mutate(block = paste0("b", block)) %>%
  left_join(plotkey) %>%
  #--clean up
  select(year, date, doy, plot_id, depth_cm, soilh2o_g.g)



# viz ---------------------------------------------------------------------

sw %>%
  ggplot(aes(doy, soilh2o_g.g)) +
  geom_line(aes(color = plot_id)) +
  facet_grid(year ~ depth_cm)


# write it ----------------------------------------------------------------

# to tidy folder (just for reference?)

sw %>% write_csv("data-raw/_tidy/soilh2o_wo.csv")

# name it what I want the actual dtaa call to be
mrs_soilh2o_wo <- sw

usethis::use_data(mrs_soilh2o_wo, overwrite = T)
