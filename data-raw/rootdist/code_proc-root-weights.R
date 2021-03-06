#########################
# created: may 13 2020
#
# purpose: process root weights
#
# NOTES:
#
# last updated:
#
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(stringr)
library(janitor) #--used to clean data




pk <- read_csv("data-raw/plotkey/plotkey.csv") %>%
  filter(year == 2019)

samp_ids <- read_csv("data-raw/rootdist/tmp_rootdist_sampids.csv")



# root weights ------------------------------------------------------------


myrootdir <- "data-raw/rootdist/root-weights/"

raw_wgts <-
  tibble(files = list.files(myrootdir)) %>%
  mutate(path = paste0(myrootdir, files)) %>%
  filter(grepl('root-weight', files)) %>%
  # make sure I don't get .txt files by mistake
  filter(grepl('.csv', files)) %>%
  # read each file
  mutate(data = path %>% purrr::map(read_csv, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  select_at(vars(-contains("X"))) %>%
  select(-notes) %>%
  mutate(samp_id_short = tolower(samp_id_short),
         samp_id_short = str_remove(samp_id_short, " "),
         samp_id_short = str_replace_all(samp_id_short, "-", "_"))


mrs_rootwgts <-
  raw_wgts %>%
  arrange(samp_id_short) %>%
  left_join(samp_ids, by = c("samp_id_short"))

#--checking values
library(ggplot2)
library(tidyverse)
library(maRsden)

mrs_rootwgts %>%
  left_join(mrs_plotkey) %>%
  filter(grepl("C", harv_crop)) %>%
  mutate(depth_cm = factor(depth_cm, levels = c("0-15", "15-30", "30-60", "60-90", "90-120"))) %>%
  ggplot(aes(x = reorder(depth_cm, desc(depth_cm)), weight_mg)) +
  geom_jitter(aes(color = rot_trt)) +
  coord_flip()

#--where is this 90-120 value coming from?


mrs_rootwgts %>% write_csv("data-raw/rootdist/rootwgts.csv")
usethis::use_data(mrs_rootwgts, overwrite = T)
