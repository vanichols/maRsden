#################################
#
# author: gina
# purpose: create apsim-style management file from excel activity log
#
# created: june 14 2019
# last modified: june 19 (finished 2012 and 2013 CS rotation)
#                june 20 (fixed dates, added crop_class)
#                dec 1 2019 (loooking over it)
#                dec 3 2019 (split from original, redesigning)
#               july 27 2020 (adding to pkg, just doing 2yr rot for now)
#
# notes: only using 2yr rotation for now
#
# inputs: Marsden-activites-for-apsim-noplots.xlsx (created by hand)
#
# outputs: apmgmt_plant
#
###################################

rm(list = ls())
library(tidyverse)
library(readxl) #--reads excel files
library(janitor) #--cleans data
library(lubridate) #--for dates


# initialize --------------------------------------------------------------

source("data-raw/apsim_mgmt/fun_clean-operations.R")
load("data/mrs_plotkey.rda")
plotkey <- mrs_plotkey %>%
  filter(year > 2011)

#--make it more manageable for trouble shooting, just for now
myplots <- c(11) #--if I just want to look at certain plots
myyears <- c(2012, 2013)


# conversions -------------------------------------------------------------

ac_in_a_ha <- 2.47105 #--acre per ha
mm_in_an_in <- 25.4 #--mm per in
lb_in_a_kg <- 0.453592 #--lbs in a kg
lbac_kgha <-  lb_in_a_kg * ac_in_a_ha

oats_sds_lb <- 15000
redclover_sds_lb <- 275000 #https://www.stockseed.com/Shop/legumes/medium-red-clover
alf_sds_lb <- 200000


# Planting ####################### ----------------------------------------

#--excel book I made by hand, Mickala is checking it

praw <-
  read_excel("data-raw/apsim_mgmt/Marsden-activites-for-apsim-noplots.xlsx", sheet = "planting", skip = 1) %>%
  remove_empty("rows")


#--clean it

pap1 <-
  praw %>%
  f_cleanup()  %>% #--helper fun
  rename(harv_crop = trt) %>%
  left_join(plotkey) %>%
  select(year, crop, harv_crop, block, plot, rot_trt, everything())

pap2 <-
  pap1 %>%
  #--filter to just 2yr sys
  filter(rot_trt == "2y")

pap3 <-
  pap2 %>%
  #--if sds_ac is blank, it's bc I only have sds_lbsac, and need to assume seeds/lb
  mutate(sds_ac = case_when(
    crop == "oats" ~ sds_lbsac * oats_sds_lb,
    crop == "red clover" ~ sds_lbsac * redclover_sds_lb,
    crop == "lucerne"~ sds_lbsac * alf_sds_lb,
    TRUE ~ sds_ac))

pap4 <-
  pap3 %>%
  #--plant populations
  mutate(
    #--sds_ac (everything) go to plants/m2 (this is the planting density)
    plantingden_m2 = sds_ac * ac_in_a_ha * 1/10000
    )

#--something wrong with the red clover? Are stand counts typically very low in April?
pap5 <-
  pap4 %>%
  # change all stand counts to plants_m2
  # oat, clover, alf are in plants/m2
  # corn and soy are in plants/ac
    mutate(
      stand_counts_plants_m2 = case_when(
        (is.na(stand_counts_plants_m2) & !is.na(stand_counts_plants_acre) ~
           stand_counts_plants_acre %>% ha_to_ac() / 10000),
        TRUE ~ stand_counts_plants_m2
      )
    ) %>%
  #--if there is no stand count, use the plantingden_m2
  mutate(
    plants_m2 = case_when(
      is.na(stand_counts_plants_m2) ~ plantingden_m2,
      TRUE ~ stand_counts_plants_m2
    )
  ) %>%
  select(year, crop, harv_crop, block, plot, rot_trt, plantingden_m2, plants_m2, everything())


pap6 <-
  pap5 %>%
  #--row spacing and depths to mm
  mutate(rowspacing_mm = rowspacing_in * mm_in_an_in,
         sowingdepth_mm = sowingdepth_in * mm_in_an_in)

pap7 <-
  pap6 %>%
  #--round things, cultivar is going to need some help....
  mutate(plants_m2 = round(plants_m2, 2),
         rowspacing_mm = round(rowspacing_mm, 0),
         sowingdepth_mm = round(sowingdepth_mm, 0),
         #--round maize cultivars to nearest 5?
         MG2 = round((as.numeric(MG))/5)*5,
         #--crop class is plant for everything but maize
         cropclass = ifelse(crop == "maize", "maize", "plant")
         ) %>%

  mutate(cultivar = case_when(str_detect(crop, "maize") ~ paste0("B_", MG2),
                           str_detect(crop, "soybean") ~ paste0("MG_", MG)))


pap <-
  pap7 %>%
  #--build Action column
  mutate(Action = paste0(crop,
                    " sow plants = ",
                    plants_m2,
                    " (plants/m^2), ",
                    "sowing_depth = ",
                    sowingdepth_mm,
                    "., cultivar = ",
                    cultivar,
                    ", row_spacing = ",
                    rowspacing_mm,
                    ", crop_class = ",
                    cropclass,
                    " !", notes)) %>%

  select(Date, Action)

pap


# write it ----------------------------------------------------------------

pap %>% write_csv("data-raw/apsim_mgmt/apmgmt_plant.csv")
