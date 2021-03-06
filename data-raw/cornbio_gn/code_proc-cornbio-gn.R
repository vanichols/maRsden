########################
#
# Date of creation: Dec 18 2019
# Author: Gina
# Purpose: Process 2018 and 2019 biomass
# NOTES: COmbine it with Will's in a another dataset
#
# In 2019, it was always 8 plants
# # I was inconsistent where I lumped the ear husks, so they are included in stem/tass
# # ear is first measured on 8/1, w/no husk values
# # ear is reported on 8/19, again w/no husk values
# # on 9/20 there is ear, but also husk, cob, kernals.
# #   for this date, the ear value is the sum of cob and kernals (500 were taken out later)
# In 2018, num of plants differed
# # once I started partitioning the ears, the ears value is blank
#
# Last updated: Feb 18 2020: added date
#                  Jan 15 20201, trying to fix ears thing, separating grain500 to another dataset
#
#########################


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data

pk <- read_csv("data-raw/plotkey/plotkey.csv")


# 2018 data ---------------------------------------------------------------

# this is a fucking mess.

bm18raw <- read_excel("data-raw/cornbio_gn/rd_mars-destructive_sampling.xlsx",
                      skip = 5,
                      na = "NA") %>%
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date),
         harv_crop = trt,
         block = paste0("b", rep)) %>%
  left_join(pk)


bm18a <-
  bm18raw %>%
  select(year, date, doy, plot_id, ds_nopl,
         ds_gleafwtsubsam_g, ds_gleafwtother_g, ds_deadleafwt_g,
         ds_stemtass_g, ds_ears_g, ds_husks_g,
         ds_cobs_g, ds_kernals_g, ds_krnl500_g) %>%
  # I was inconsistent where I lumped the ear husks, so they are included in stem/tass
  # call it 'ear', sum of cob and kernals
  # do leaf, stemtasshusk, ear, plant
  mutate(leaf = ds_gleafwtsubsam_g + ds_gleafwtother_g + ds_deadleafwt_g,
         stemtasshusk = ds_stemtass_g + ds_husks_g ,
         ear = ds_ears_g + ds_kernals_g + ds_cobs_g, #-this works for 2018, I either did ear or kernal+cob
         plant = leaf + stemtasshusk + ear,
         nu_pl = ds_nopl) %>%
  select(-contains("ds")) %>%
  gather(leaf:plant, key = organ, value = mass_g) %>%
  mutate(mass_gpl = mass_g/nu_pl) %>%
  select(year, date, doy, plot_id, organ, mass_g, mass_gpl)


#--add planting date, 5/8, 0 biomass point

plots18 <-
  bm18 %>%
  pull(plot_id) %>%
  unique()

organs18 <- bm18 %>% pull(organ) %>% unique()

bm18pl <-
  tibble(year = rep(2018, length(plots18)*length(organs18)),
         date = as_date("2018-04-23"),
         doy = yday(date),
         plot_id = rep(plots18, each = length(organs18)),
         organ = rep(organs18, times = 8),
         mass_g = 0,
         mass_gpl = 0)

bm18b <-
  bm18pl %>%
  bind_rows(bm18a)

bm18b %>%
  ggplot(aes(doy, mass_gpl, color = organ)) +
  geom_point()

# 2019 biomass ----------------------------------------------------

my19dir <- "data-raw/cornbio_gn/2019-data/"

#--biomass

bm19raw <-
  tibble(files = list.files(my19dir)) %>%
  mutate(path = paste0(my19dir, files)) %>%
  filter(grepl('biomass', files)) %>%
  filter(grepl(".xlsx", files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  fill(date, plot)


bm19a <-
  bm19raw %>%
  # subtract weight of bag
  mutate(wgtbag_g = ifelse(is.na(wgtbag_g), 0, wgtbag_g),
         wgt_g = wgtall_g - wgtbag_g) %>%
  select(-wgtall_g, -wgtbag_g) %>%
  mutate(wgt_g = ifelse(is.na(wgt_g), 0, wgt_g)) %>%
  # fix date and plot things
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date)) %>%
  left_join(pk) %>%
  select(year, date, doy, plot_id, organ, wgt_g) %>%
  arrange(year, date, doy, plot_id) %>%
  pivot_wider(names_from = organ, values_from = wgt_g)


bm19b <-
  bm19a %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  # do leaf, stemtasshusk, ear, plant
  mutate(stemtasshusk = stemtass + ear_husk,
         ear =  ear,
         leaf = brnleaf + LAIgleaf + othergleaf,
         plant = stemtasshusk + ear + leaf) %>%
  select(year, date, doy,
         plot_id,
         plant, leaf, stemtasshusk, ear) %>%
  pivot_longer(plant:ear) %>%
  rename("organ" = name,
         "mass_g" = value) %>%
  mutate(mass_gpl = mass_g/8)  #--always 8 plants in 2019


#--add planting date, 6/3, 0 biomass point

plots19 <-
  bm19b %>%
  pull(plot_id) %>%
  unique()

organs19 <- bm19b %>% pull(organ) %>% unique()

bm19pl <-
  tibble(year = rep(2019, length(plots19)*length(organs19)),
         date = as_date("2019-06-03"),
         doy = yday(date),
         plot_id = rep(plots19, each = length(organs19)),
         organ = rep(organs19, times = 8),
         mass_g = 0,
         mass_gpl = 0)

bm19c <-
  bm19pl %>%
  bind_rows(bm19b)



bm19c %>%
  ggplot(aes(doy, mass_gpl, color = organ)) +
  geom_point()


# 2020 --------------------------------------------------------------------

my20dir <- "data-raw/cornbio_gn/2020-data/"

#--biomass

bm20raw <-
  tibble(files = list.files(my20dir)) %>%
  mutate(path = paste0(my20dir, files)) %>%
  filter(grepl('biomass', files)) %>%
  filter(grepl(".xlsx", files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  fill(date, plot)


bm20a <-
  bm20raw %>%
  # fix date
  mutate(date = as_date(date),
         year = year(date),
         doy = yday(date)) %>%
  left_join(pk) %>%
  rename("wgt_g" = weight_g) %>%
  select(year, date, doy, plot_id, organ, wgt_g) %>%
  arrange(year, date, doy, plot_id) %>%
  pivot_wider(names_from = organ, values_from = wgt_g)


bm20b <-
  bm20a %>%
  mutate_if(is.numeric, replace_na, 0) %>%
  # do leaf, stemtasshusk, ear, plant
  mutate(stemtasshusk = stem + reprod + stemtass,
         ear =  ear,
         leaf = greenleaf + brnleaf,
         plant = stemtasshusk + ear + leaf) %>%
  select(year, date, doy,
         plot_id,
         plant, leaf, stemtasshusk, ear) %>%
  pivot_longer(plant:ear) %>%
  rename("organ" = name,
         "mass_g" = value) %>%
  mutate(mass_gpl = mass_g/8)  #--always 8 plants in 2020?


#--add planting date, 4/23, 0 biomass point

plots20 <-
  bm20b %>%
  pull(plot_id) %>%
  unique()

organs20 <- bm20b %>% pull(organ) %>% unique()

bm20pl <-
  tibble(year = rep(2020, length(plots20)*length(organs20)),
         date = as_date("2020-04-23"),
         doy = yday(date),
         plot_id = rep(plots20, each = length(organs20)),
         organ = rep(organs20, times = 8),
         mass_g = 0,
         mass_gpl = 0)

bm20c <-
  bm20pl %>%
  bind_rows(bm20b)


bm20c %>%
  ggplot(aes(doy, mass_gpl, color = organ)) +
  geom_point()


# combine my datasets -------------------------------------------

mrs_cornbio_gn <-
  bind_rows(bm18b, bm19c, bm20c) %>%
  arrange(year, date, doy, plot_id, organ) %>%
  mutate_if(is.numeric, replace_na, 0)

mrs_cornbio_gn %>%
  ggplot(aes(doy, mass_gpl)) +
  geom_point(aes(color = organ)) +
  facet_grid(.~year)

mrs_cornbio_gn %>%  write_csv("data-raw/cornbio_gn/cornbio_gn.csv")

usethis::use_data(mrs_cornbio_gn, overwrite = T)
