#########################
# created: april 20 2020
#
# last updated:
#
# purpose: Process root dist data from 2019 ML exp
#
# NOTES:
#
#########################


##### Clear environment and load packages #####
rm(list = ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data


pk <- read_csv("data-raw/plotkey/plotkey.csv") %>%
  filter(year == 2019)


# 2019 --------------------------------------------------------------------

rd19raw <- read_excel("data-raw/rootdist_ml/2019 Marsden Farm Root Biomass Data single sheet.xlsx") %>%
  clean_names()

dap19 <- read_excel("data-raw/rootdist_ml/DAP-to-date.xlsx") %>%
  rename(days_after_planting = DAP) %>%
  mutate(date = ymd(date))




# wrangle -----------------------------------------------------------------


rd19 <-
  rd19raw %>%
  left_join(dap19) %>%
  mutate(year = year(date)) %>%
  #--he specifies the side of the plot, doesn't matter for my purposes
  mutate(plot = parse_number(plot)) %>%
  left_join(pk)

rd19 %>%
  mutate(depth = as.factor(depth)) %>%
  ggplot(aes(fct_rev(depth), root_weights_g, color = harv_crop, group = harv_crop)) +
  stat_summary(fun = "mean") +
  stat_summary(fun = "mean", geom = "line") +
  facet_grid(days_after_planting ~ .) +
  coord_flip()


# write data ----------------------------------------------

mrs_rootdist_ml <-
  rd19 %>%
  arrange(year, date, plot_id, depth) %>%
  select(date, plot_id, days_after_planting, depth,
         total_soil_weight_g_4_subsamples_bulked,
         root_weights_g, soil_volume_cm_3,
         mass_volume_g_cm_3)

mrs_rootdist_ml %>% write_csv("data-raw/rootdist_ml/mrs_rootdist_ml.csv")
usethis::use_data(mrs_rootdist_ml, overwrite = T)
