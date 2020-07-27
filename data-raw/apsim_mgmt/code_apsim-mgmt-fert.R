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
#
# inputs:
# outputs:
#
###################################

rm(list = ls())
library(tidyverse)
library(readxl) #--reads excel files
library(janitor) #--cleans data
library(lubridate) #--for dates
library(maRsden)



# initialize --------------------------------------------------------------

source("data-raw/apsim_mgmt/fun_clean-operations.R")
load("data/mrs_plotkey.rda")
plotkey <- mrs_plotkey %>%
  filter(year > 2011)

#--make it more manageable for trouble shooting, just for now
myplots <- c(13) #--if I just want to look at certain plots
myyears <- c(2012, 2013)


# conversions -------------------------------------------------------------

ac_in_a_ha <- 2.47105 #--acre per ha
mm_in_an_in <- 25.4 #--mm per in
lb_in_a_kg <- 0.453592 #--lbs in a kg
lbac_to_kgha <-  lb_in_a_kg * ac_in_a_ha # multiply by 1.12



# fertilizer --------------------------------------------------------------


# PROBLEMS: manure has to call SurfaceOrganicMatter, so do it separately.

#--excel book I made by hand
fraw <- read_excel("data-raw/apsim_mgmt/Marsden-activites-for-apsim-noplots.xlsx",
                    sheet = "fertilizer", skip = 1) %>%
  remove_empty("rows")

#--clean it, ignoring manure

fap1 <-
  fraw %>%
  f_cleanup() %>% #--helper fun
  rename(harv_crop = trt) %>%
  left_join(plotkey)

fap2 <-
  fap1 %>%
  filter(plot %in% myplots & year %in% myyears) %>% #--corn then soy
  #--remove manure entries
  filter(!grepl("manure", type))

fap3 <-
  fap2 %>%
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
      )

fap4 <-
  fap3 %>%
  #--round them
  mutate(amount_kgha = round(amount_kgha, 2))

#NOTE: the potash application in 2013 doesn't have a date

fap <-
  fap4 %>%
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
  f_cleanup() %>% #--helper fun
  rename(harv_crop = trt) %>%
  left_join(plotkey) %>%
  filter(plot == 18 & year %in% c(2012, 2013)) %>% #--alfalfa plot
  #--keep only manure entries
  filter(grepl("manure", type)) %>%

  #--recode and calculate things
  mutate(
    man_amount_kgha = wet_tonac * mois_pct/100 * 2000 * lbac_kgha,
    ) #%>%

  #--build Action column (not sure what this looks like for manure...)
  # THIS IS WRONG
  # mutate(Action = paste0("Fertiliser apply amount = ",
  #                        amount_kgha,
  #                        " (kg/ha), depth = ",
  #                        depth_mm,
  #                        " (mm), type = ",
  #                        type_apsim,
  #                        " ()")) %>%
  #
  # select(Date, Action)

fap_man

