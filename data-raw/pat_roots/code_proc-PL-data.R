#############################
##
## Dec 12 2019
## Process Pat Laziki's data (2008-2010)
## Published in PLOS-ONE  https://doi.org/10.1371/journal.pone.0164209
## Writes file to _tidy and put in package
##
##############################

#NOTE: don't have plot ids for 2008-2010, I start at 2012....

rm(list=ls())
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)


# read in data ------------------------------------------------------------
plotkey <- read_csv("data-raw/_tidy/plotkey.csv")

# Raw data from PLOS-ONE
pl <- read_excel("data-raw/_raw/pl/data_Lazicki et al (2016).xlsx", na = ".") %>%
  clean_names()

# Meta data to help interpret it (good job Pat!)
meta <- read_excel("data-raw/_raw/pl/data_Lazicki et al (2016) meta data.xlsx", sheet = "meta_data")
dateid <- read_excel("data-raw/_raw/pl/data_Lazicki et al (2016) meta data.xlsx", sheet = "date_table") %>%
  mutate(date = date_id)

# get it into the form I want ---------------------------------------------

#practice
pl %>%
  select(block, system, depth, crop, date) %>%
  left_join(dateid) %>%
  mutate(block = paste0("b", block),
         rot_trt = paste0(system, "y"),
         harv_crop = paste0(crop, system)) %>%
  left_join(plotkey)


# first, get year into pl
pl2 <-
  pl %>%
  left_join(dateid) %>%
  # get things into shape to merge w/plotkey
  mutate(block = paste0("b", block),
         rot_trt = paste0(system, "y"),
         harv_crop = paste0(crop, system)) %>%
  left_join(plotkey) %>%
  # now just clean it up
  select(-(id:system), -(crop:date),
         -(date_id:year),
         -(rot_trt:plot)) %>%
  select(plot_id, season, depth, everything())

#plplt <-
  pl %>%
  select(block, system, crop) %>%



    select(-block, -system, -crop) %>%
    rename("block" = block2) %>%
    select(block, rot_trt

