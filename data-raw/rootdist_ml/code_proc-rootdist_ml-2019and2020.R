#########################
# created: july 21 2021
#
# last updated:
#
# purpose: Process root dist data from 2019 and 2020 ML exp
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

rd19 <-
  rd19raw %>%
  left_join(dap19) %>%
  mutate(year = year(date)) %>%
  #--he specifies the side of the plot, doesn't matter for my purposes
  mutate(plot = parse_number(plot)) %>%
  left_join(pk) %>%
  select(year, date, days_after_planting, depth, plot_id, root_weights_g)

rd19

# 2020 --------------------------------------------------------------------

d4 <-
  read_excel("data-raw/rootdist_ml/2020-data-from-matt/Marsden 2020 Root Biomass Data_12Feb2021.xlsx",
                 skip = 7, sheet = "D4") %>%
  clean_names() %>%
  remove_empty() %>%
  slice(1:32) %>%
  mutate(plot = parse_number(plot),
         date = ymd("2020/04/27"),
         year = year(date),
         days_after_planting = 4) %>%
  fill(plot, rotation) %>%
  mutate(plot_id = paste(year, plot, sep = "_")) %>%
select(date, plot_id, depth, days_after_planting, root_weights_g)  %>%
  mutate(root_weights_g = as.numeric(root_weights_g))


d27 <-
  read_excel("data-raw/rootdist_ml/2020-data-from-matt/Marsden 2020 Root Biomass Data_12Feb2021.xlsx",
             skip = 3, sheet = "D27") %>%
  clean_names() %>%
  remove_empty() %>%
  slice(1:32) %>%
  mutate(plot = parse_number(plot),
         date = ymd("2020/05/22"),
         year = year(date),
         days_after_planting = 27) %>%
  fill(plot, rotation) %>%
  mutate(plot_id = paste(year, plot, sep = "_")) %>%
  select(date, plot_id, depth, days_after_planting, root_weights_g)  %>%
  mutate(root_weights_g = as.numeric(root_weights_g))

d50 <-
  read_excel("data-raw/rootdist_ml/2020-data-from-matt/Marsden 2020 Root Biomass Data_12Feb2021.xlsx",
             skip = 3, sheet = "D50") %>%
  clean_names() %>%
  remove_empty() %>%
  slice(1:32) %>%
  mutate(plot = parse_number(plot),
         date = ymd("2020/06/12"),
         year = year(date),
         days_after_planting = 50) %>%
  fill(plot, rotation) %>%
  mutate(plot_id = paste(year, plot, sep = "_")) %>%
  rename("root_weights_g" = root_weight_g) %>%
  select(date, plot_id, depth, days_after_planting, root_weights_g)  %>%
  mutate(root_weights_g = as.numeric(root_weights_g))

d68 <-
  read_excel("data-raw/rootdist_ml/2020-data-from-matt/Marsden 2020 Root Biomass Data_12Feb2021.xlsx",
             skip = 3, sheet = "D68") %>%
  clean_names() %>%
  remove_empty() %>%
  slice(1:32) %>%
  mutate(plot = parse_number(plot),
         date = ymd("2020/06/30"),
         year = year(date),
         days_after_planting = 68) %>%
  fill(plot, rotation) %>%
  mutate(plot_id = paste(year, plot, sep = "_")) %>%
  rename("root_weights_g" = root_weight_g) %>%
  select(date, plot_id, depth, days_after_planting, root_weights_g)  %>%
  mutate(root_weights_g = as.numeric(root_weights_g))


d96 <-
  read_excel("data-raw/rootdist_ml/2020-data-from-matt/Marsden 2020 Root Biomass Data_12Feb2021.xlsx",
             skip = 3, sheet = "D96") %>%
  clean_names() %>%
  remove_empty() %>%
  slice(1:32) %>%
  mutate(plot = parse_number(plot),
         date = ymd("2020/07/28"),
         year = year(date),
         days_after_planting =96) %>%
  fill(plot, rotation) %>%
  mutate(plot_id = paste(year, plot, sep = "_")) %>%
  #rename("root_weights_g" = root_weight_g) %>%
  select(date, plot_id, depth, days_after_planting, root_weights_g)  %>%
  mutate(root_weights_g = as.numeric(root_weights_g))

d117 <-
  read_excel("data-raw/rootdist_ml/2020-data-from-matt/Marsden 2020 Root Biomass Data_12Feb2021.xlsx",
             skip = 3, sheet = "D117") %>%
  clean_names() %>%
  remove_empty() %>%
  slice(1:32) %>%
  mutate(plot = parse_number(plot),
         date = ymd("2020/08/18"),
         year = year(date),
         days_after_planting =117) %>%
  fill(plot, rotation) %>%
  mutate(plot_id = paste(year, plot, sep = "_")) %>%
  #rename("root_weights_g" = root_weight_g) %>%
  select(date, plot_id, depth, days_after_planting, root_weights_g)  %>%
  mutate(root_weights_g = as.numeric(root_weights_g))

rd20 <-
  d4 %>%
  bind_rows(d27) %>%
  bind_rows(d50) %>%
  bind_rows(d68) %>%
  bind_rows(d96) %>%
  bind_rows(d117)


# write data ----------------------------------------------

mrs_rootdist_ml <-
  rd19 %>%
  bind_rows(rd20) %>%
  arrange(year, date, plot_id, depth)


mrs_rootdist_ml %>% write_csv("data-raw/rootdist_ml/mrs_rootdist_ml.csv")
usethis::use_data(mrs_rootdist_ml, overwrite = T)
