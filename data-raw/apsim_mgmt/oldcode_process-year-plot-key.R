#################################
#  
# author: gina
# purpose: create a year-plot-trt-key for 2012-2019
#
# created: june 17 2019
# last modified: june 18 (added 2012-2014 data)
#
# notes: 
#
###################################

rm(list = ls())
library(tidyverse)
library(readxl) #--reads excel files
library(janitor) #--cleans data
library(lubridate) #--for dates
library(here) #--for setting directory

setwd(here())


dat <- 
  read_excel("_data/raw/year-plot-trt-key.xlsx") %>%
  mutate(crop_abb = str_sub(trt, 1, 1),
         system = paste0(str_sub(trt, 2,2), "yr"))

write_csv(dat, "_data/tidy/td_year-plot-trt-key.csv")
