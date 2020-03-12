#########################
## Created Dec 19 2019
## Author Gina
## Purpose: Process ML stand counts.
## NOTES: Every year has to be done separately, nothing is consistent.
##
#########################

rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) # used to read Excel files
library(janitor)

pk <- read_csv("data-raw/_tidy/plotkey.csv")
mperac <- 4046.86

# 2012 --------------------------------------------------------------------

o12 <- read_excel("data-raw/_raw/ml/stand_counts/2012 std cnts - all crops.xlsx",
           sheet = "2012 oats gn",
           skip = 1) %>%
  mutate(date = as_date("2012-04-23"),
         year = year(date),
         doy = yday(date)) %>%
  left_join(pk) %>%
  select(year, doy, plot_id, crop, pl_m2)

c12 <- read_excel("data-raw/_raw/ml/stand_counts/2012 std cnts - all crops.xlsx",
           sheet = "2012 Corn",
           skip = 6) %>%
  clean_names() %>%
  select(plot, mean, plants_acre_1) %>%
  filter(!is.na(mean)) %>%
  fill(plot) %>%
  mutate(date = as_date("2012-05-24"),
         year = year(date),
         doy = yday(date),
         plot = as.numeric(plot),
         crop = "corn",
         pl_m2 = plants_acre_1 / mperac) %>%
  left_join(pk) %>%
  select(year, doy, plot_id, crop, pl_m2)

s12 <- read_excel("data-raw/_raw/ml/stand_counts/2012 std cnts - all crops.xlsx",
                  sheet = "2012 SB",
                  skip = 6) %>%
  clean_names() %>%
  select(plot, mean, plants_acre_1) %>%
  filter(!is.na(mean)) %>%
  fill(plot) %>%
  mutate(date = as_date("2012-06-01"),
         year = year(date),
         doy = yday(date),
         plot = as.numeric(plot),
         crop = "soy",
         pl_m2 = plants_acre_1 / mperac) %>%
  left_join(pk) %>%
  select(year, doy, plot_id, crop, pl_m2)


# 2013 --------------------------------------------------------------------

o13 <- read_excel("data-raw/_raw/ml/stand_counts/2013 std cnts - all crops.xlsx",
                  sheet = "2013 oats gn") %>%
  mutate(year = year(date),
         doy = yday(date)) %>%
  left_join(pk) %>%
  select(year, doy, plot_id, crop, pl_m2)

c13 <- read_excel("data-raw/_raw/ml/stand_counts/2013 std cnts - all crops.xlsx",
                  sheet = "2013 Corn - Table 1",
                  skip = 6) %>%
  clean_names() %>%
  select(plot, mean, plants_acre_1) %>%
  filter(!is.na(mean)) %>%
  fill(plot) %>%
  mutate(date = as_date("2013-06-06"),
         year = year(date),
         doy = yday(date),
         plot = as.numeric(plot),
         crop = "corn",
         pl_m2 = plants_acre_1 / mperac) %>%
  left_join(pk) %>%
  group_by(year, doy, plot_id, crop) %>%
  summarise(pl_m2 = mean(pl_m2, na.rm = T))

s13 <- read_excel("data-raw/_raw/ml/stand_counts/2013 std cnts - all crops.xlsx",
                  sheet = "2013 SB - Table 1",
                  skip = 6) %>%
  clean_names() %>%
  select(plot, mean, plants_acre_1) %>%
  filter(!is.na(mean)) %>%
  fill(plot) %>%
  mutate(date = as_date("2013-07-19"),
         year = year(date),
         doy = yday(date),
         plot = as.numeric(plot),
         crop = "soy",
         pl_m2 = plants_acre_1 / mperac) %>%
  left_join(pk) %>%
  group_by(year, doy, plot_id, crop) %>%
  summarise(pl_m2 = mean(pl_m2, na.rm = T))


# 2014 --------------------------------------------------------------------

c14 <- read_excel("data-raw/_raw/ml/stand_counts/2014 std cnts - corn.xlsx",
                  skip = 6) %>%
  clean_names() %>%
  select(plot, mean, plants_acre_1) %>%
  filter(!is.na(mean)) %>%
  fill(plot) %>%
  mutate(date = as_date("2014-06-05"),
         year = year(date),
         doy = yday(date),
         plot = as.numeric(plot),
         crop = "corn",
         pl_m2 = plants_acre_1 / mperac) %>%
  left_join(pk) %>%
  group_by(year, doy, plot_id, crop) %>%
  summarise(pl_m2 = mean(pl_m2, na.rm = T))


# 2015 --------------------------------------------------------------------

o15 <- read_excel("data-raw/_raw/ml/stand_counts/2005-2016 std cnts oat legume.xlsx",
                  sheet = "2015") %>%
  mutate(date = as_date("2015-06-01"), #--unknown, made this up
         year = year(date),
         doy = yday(date)) %>%
  left_join(pk) %>%
  select(year, doy, plot_id, crop, pl_m2)

c15 <- read_excel("data-raw/_raw/ml/stand_counts/2015 std cnts - corn soy.xlsx",
                  sheet = "2015 Corn",
                  skip = 7) %>%
  clean_names() %>%
  select(plot, mean, plants_acre) %>%
  filter(!is.na(mean)) %>%
  fill(plot) %>%
  mutate(date = as_date("2014-06-05"),
         year = year(date),
         doy = yday(date),
         plot = as.numeric(plot),
         crop = "corn",
         pl_m2 = plants_acre / mperac) %>%
  left_join(pk) %>%
  group_by(year, doy, plot_id, crop) %>%
  summarise(pl_m2 = mean(pl_m2, na.rm = T))

s15 <- read_excel("data-raw/_raw/ml/stand_counts/2015 std cnts - corn soy.xlsx",
                  sheet = "2015 Soybean",
                  skip = 7) %>%
  clean_names() %>%
  select(plot, mean, plants_acre) %>%
  filter(!is.na(mean)) %>%
  fill(plot) %>%
  mutate(date = as_date("2014-07-08"),
         year = year(date),
         doy = yday(date),
         plot = as.numeric(plot),
         crop = "soy",
         pl_m2 = plants_acre / mperac) %>%
  left_join(pk) %>%
  group_by(year, doy, plot_id, crop) %>%
  summarise(pl_m2 = mean(pl_m2, na.rm = T))


# 2016 --------------------------------------------------------------------

o16 <- read_excel("data-raw/_raw/ml/stand_counts/2005-2016 std cnts oat legume.xlsx",
                  sheet = "2016") %>%
  mutate(date = as_date("2016-06-01"), #--unknown, made this up
         year = year(date),
         doy = yday(date)) %>%
  left_join(pk) %>%
  select(year, doy, plot_id, crop, pl_m2)

c16 <- read_excel("data-raw/_raw/ml/stand_counts/2016 std cnts - corn soy.xlsx",
                  sheet = "2016 Corn",
                  skip = 7) %>%
  clean_names() %>%
  select(plot, mean, plants_acre) %>%
  filter(!is.na(mean)) %>%
  fill(plot) %>%
  mutate(date = as_date("2014-06-23"),
         year = year(date),
         doy = yday(date),
         plot = as.numeric(plot),
         crop = "corn",
         pl_m2 = plants_acre / mperac) %>%
  left_join(pk) %>%
  filter(!is.na(plot_id)) %>% #--it's just a thing bc of Matt's arrangement
  group_by(year, doy, plot_id, crop) %>%
  summarise(pl_m2 = mean(pl_m2, na.rm = T))

s16 <- read_excel("data-raw/_raw/ml/stand_counts/2016 std cnts - corn soy.xlsx",
                  sheet = "2016 Soybean",
                  skip = 7) %>%
  clean_names() %>%
  select(plot, mean, plants_acre) %>%
  filter(!is.na(mean)) %>%
  fill(plot) %>%
  mutate(date = as_date("2014-06-29"),
         year = year(date),
         doy = yday(date),
         plot = as.numeric(plot),
         crop = "soy",
         pl_m2 = plants_acre / mperac) %>%
  left_join(pk) %>%
  group_by(year, doy, plot_id, crop) %>%
  summarise(pl_m2 = mean(pl_m2, na.rm = T))


# 2017 --------------------------------------------------------------------


