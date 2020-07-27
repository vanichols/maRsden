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
#               july 27 2020 (adding to pkg)
#
# notes: just trying to do plot 13 and 18 for 2012-2013 for now
#        need to update conversion factors (don't have internet now...)
#
# inputs:
#         Marsden-activites-for-apsim.xlsx (in raw folder)
# outputs: td_op-plot13-2012-13
#
###################################

rm(list = ls())
library(tidyverse)
library(readxl) #--reads excel files
library(janitor) #--cleans data
library(lubridate) #--for dates
library(maRsden)



# initialize --------------------------------------------------------------

source("data-raw/apsim_mgmt/fun_clean-planting-tab.R")
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
  f_cleanp()  %>% #--helper fun
  rename(harv_crop = trt) %>%
  left_join(plotkey) %>%
  select(year, crop, harv_crop, block, plot, rot_trt, everything())

pap2 <-
  pap1 %>%
  #--filter to 2012 and 2013, plot 11 (soybean then oats/clover)
  filter(plot %in% myplots & year %in% myyears)

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



# stopped dec 3, seems good up until here


# fertilizer ####################### ----------------------------------------

# PROBLEMS: manure has to call SurfaceOrganicMatter, so do it separately.

#--excel book I made by hand
fraw <- read_excel("_data/raw/Marsden-activites-for-apsim.xlsx",
                    sheet = "fertilizer", skip = 1) %>%
  remove_empty("rows")

# this isn't right....
#--clean it, ignoring manure

fap <-
  fraw %>%
  f_procop() %>% #--helper fun
  left_join(key) %>%
  filter(plot %in% myplots & year %in% myyears) %>%

  #--remove manure entries
  filter(!grepl("manure", type)) %>%

  #--recode and calculate things
    mutate(

      amount_kgha = amount_lbac * lbac_kgha,

      pct_N = case_when(str_detect(type, "uan_28") ~ 0.28,
                        str_detect(type, "uan_32") ~ 0.32,
                        TRUE ~ 1),

      amount_kgha = amount_kgha * pct_N,
      type_apsim = recode(type,
                          uan_28 = "UAN_N",
                          uan_32 = "UAN_N")
      ) %>%

  #--filter to 2012 and 2013, plot 13 (C then S)
  filter(plot == 13 & year %in% c(2012, 2013)) %>%

  #--build Action column
  mutate(Action = paste0("Fertiliser apply amount = ",
                         amount_kgha,
                         " (kg/ha), depth = ",
                         depth_mm,
                         " (mm), type = ",
                         type_apsim,
                         " ()")) %>%

  select(Date, Action)

fap



#--do manure

fap_man <-
  fraw %>%
  f_procop() %>% #--helper fun

  left_join(key, by = c("year", "trt", "crop_abb", "system")) %>%
  filter((plot == 13 & year %in% c(2012, 2013)) %>%
  #--keep only manure entries
  filter(grepl("manure", type)) %>%

  #--recode and calculate things
  mutate(

    man_amount_kgha = wet_tonac * mois_pct/100 * 2000 * lbac_kgha,
    ) %>%

  #--filter to 2012 and 2013, plot 13 (C then S)


  #--build Action column
  mutate(Action = paste0("Fertiliser apply amount = ",
                         amount_kgha,
                         " (kg/ha), depth = ",
                         depth_mm,
                         " (mm), type = ",
                         type_apsim,
                         " ()")) %>%

  select(Date, Action)

fap

# tillage ####################### ----------------------------------------


#--excel book I made by hand

traw <- read_excel("_data/raw/Marsden-activites-for-apsim.xlsx",
                   sheet = "tillage", skip = 1) %>%
  remove_empty("rows")


#--clean it

tap <-
  traw %>%
  f_procop() %>% #--helper fun

  left_join(key, by = c("year", "trt", "crop_abb", "system")) %>%

  #--unit conversions
  mutate(depth_mm = depth_in * mm_in) %>%

  #--assign f_incorp based on tillage type
  mutate(

    f_incorp = case_when(
      str_detect(tillage, "field cultivation leveling") ~ 0.5,
      str_detect(tillage, "field cultivation") ~ 0.5,
      str_detect(tillage, "lightly disk") ~ 0.2,
      str_detect(tillage, "chisel plow") ~ 0.7,
      str_detect(tillage, "moldboard plow") ~ 0.9,
      TRUE ~ 0)

    ) %>%

  #--filter to 2012 and 2013, plot 13 (C then S)
  filter(plot == 13 & year %in% c(2012, 2013)) %>%

  #--build Action column
  mutate(Action = paste0("SurfaceOrganicMatter tillage type = user_defined, ",
                         "f_incorp = ",
                         f_incorp,
                         ", depth = ",
                         depth_mm,
                         " (mm)")
         ) %>%

  select(Date, Action)


tap

# harvest ####################### ----------------------------------------

# just deal with grain harvests

#--excel book I made by hand

hraw <- read_excel("_data/raw/Marsden-activites-for-apsim.xlsx",
                   sheet = "harvest",
                   skip = 1) %>%
  remove_empty("rows")


#--clean it

hraw2 <-
  hraw %>%
  f_procop() %>% #--helper fun

  left_join(key, by = c("year", "trt", "crop_abb", "system")) %>%

  filter(harvest == 'grain') %>%

  #--filter to 2012 and 2013, plot 13 (C then S)
  filter(plot == 13 & year %in% c(2012, 2013))

# Make the 'harvest' action


hhar <-
  hraw2 %>%
  #--build Action column
  mutate(Action = paste0(crop,
                       " harvest")) %>%
  select(Date, Action)

#--Make the 'end crop' action

hend <-
  hraw2 %>%
  #--build Action column
  mutate(Action = paste0(crop,
                         " end_crop")) %>%
  select(Date, Action)

hap <-
  bind_rows(hhar, hend) %>%
  arrange(Date)


# notes ####################### ----------------------------------------

#--excel book I made by hand

nraw <- read_excel("_data/raw/Marsden-activites-for-apsim.xlsx",
                   sheet = "notes") %>%
  remove_empty("rows")


#--clean it

nap <-
  nraw %>%
  f_procop() %>% #--helper fun

  left_join(key, by = c("year", "trt", "crop_abb", "system")) %>%

  #--filter to 2012 and 2013, plot 13 (C then S)
  filter(plot == 13 & year %in% c(2012, 2013)) %>%

  mutate(Action = paste("!",
                         crop,
                         notes,
                        sep = " ")
         ) %>%
  select(Date, Action)

# make operations schedule ####################### ----------------------------------------

apop <-
  bind_rows(pap, fap, tap, hap, nap) %>%
  mutate(lubdate = dmy(Date),
         #--make same-day operations happen in sensical order
         myorder = case_when(
           str_detect(Action, "Fertiliser") ~ 1,
           str_detect(Action, "tillage") ~ 2,
           str_detect(Action, "sow") ~ 3)
         ) %>%
  arrange(lubdate, myorder) %>%
  select(-lubdate, -myorder)


write_csv(apop, "_data/tidy/td_op-plot13-2012-13.csv")
