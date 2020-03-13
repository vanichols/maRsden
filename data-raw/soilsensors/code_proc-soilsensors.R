########################
#
# created: march 13 2020
#
# updated:
#
#
# purpose: process soil sensor data
#
# NOTES:
#
#
#########################


rm(list=ls())
library(tidyverse)
library(lubridate)
library(janitor) #--used to clean data

#--this is the plotkey data available in the package
pk <- read_csv("data-raw/plotkey/plotkey.csv") %>%
  filter(year %in% c(2018, 2019),
         harv_crop %in% c("C2", "C4"))


ssraw <- readRDS("data-raw/soilsensors/tidyQC-soil-marsden.rds") %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date),
         plot = as.numeric(plot))

ss <- ssraw %>%
  left_join(pk) %>%
  select(year, date, doy, plot_id,
         sensor_type, sensor_name, sensor_depth_cm, sensor_unit, value)


#--something funny is happening in 2018....
ss %>%
  filter(sensor_unit == "soilVWC") %>%
  left_join(pk) %>%
  ggplot(aes(doy, value, group = plot_id)) +
  geom_line(aes(color = rot_trt)) +
  facet_grid(sensor_depth_cm~year)



# make datasets -------------------------------------------

mrs_soilsensors <- ss %>%
  arrange(year, doy, plot_id, sensor_depth_cm)

mrs_soilsensors %>%  write_csv("data-raw/soilsensors/soilsensors.csv")

usethis::use_data(mrs_soilsensors, overwrite = T)
