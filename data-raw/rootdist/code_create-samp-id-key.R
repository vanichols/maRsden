#########################
# created: may 13 2020
#
# purpose: create samplie id key from google sheet, which was copy-pasted (argh)
#
# NOTES: googlesheets not authorized (?!)
#
# last updated:
#
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(dplyr)
library(readr)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data


pk <- read_csv("data-raw/plotkey/plotkey.csv") %>%
  filter(year == 2019)


alf <- read_excel("data-raw/rootdist/misc/sample_ids.xlsx", sheet = "alfalfa") %>%
  select(site:samp_id_short)
mai <- read_excel("data-raw/rootdist/misc/sample_ids.xlsx", sheet = "maize") %>%
  select(site:samp_id_short)
oat <- read_excel("data-raw/rootdist/misc/sample_ids.xlsx", sheet = "oats") %>%
  select(site:samp_id_short)



samp_ids <-
  alf %>%
  bind_rows(mai) %>%
  bind_rows(oat) %>%
  mutate(samp_date = ymd(samp_date),
         year = year(samp_date),
         doy = yday(samp_date)) %>%
  left_join(pk, by = c("plot", "year")) %>%
    select(samp_date, year, doy, plot_id, depth_cm, samp_id_short)



mrs_rootdist_sampids <-
  samp_ids %>%
  arrange(plot_id, samp_date)

mrs_rootdist_sampids %>% write_csv("data-raw/rootdist/tmp_rootdist_sampids.csv")
