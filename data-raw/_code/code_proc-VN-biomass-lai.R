#########################
##
## Date of creation: Dec 18 2019
## Author: Gina
## Purpose: Process 2018 and 2019 biomass and associated LAI data
## NOTES: 
##
## In 2019, it was always 8 plants
## # I was inconsistent where I lumped the ear husks, so they are included in stem/tass
## # ear is first measured on 8/1, w/no husk values
## # ear is reported on 8/19, again w/no husk values
## # on 9/20 there is ear, but also husk, cob, kernals. 
## #   for this date, the ear value is the sum of cob and kernals (500 were taken out later)
## In 2018, num of plants differed
## # once I started partitioning the ears, the ears value is blank


#########################


rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data
library(fuzzyjoin) #--to do fuzzy joining of dates

pk <- read_csv("data-raw/_tidy/plotkey.csv")

mydir <- "_theme-2019data/_data/raw/"
mydir <- "data-raw/_raw/vn/2019/"




# 2018 data ---------------------------------------------------------------

# this is a fucking mess. 

bm18raw <- read_excel("data-raw/_raw/vn/2018/rd_mars-destructive_sampling.xlsx", 
                      skip = 5,
                      na = "NA") %>% 
  mutate(year = year(date),
         doy = yday(date),
         harv_crop = trt,
         block = paste0("b", rep)) %>% 
  left_join(pk)


bm18 <- 
  bm18raw %>% 
  select(year, doy, plot_id, ds_nopl, 
         ds_gleafwtsubsam_g, ds_gleafwtother_g, ds_deadleafwt_g,
         ds_stemtass_g, ds_ears_g, ds_husks_g, 
         ds_cobs_g, ds_kernals_g, ds_krnl500_g) %>% 
  # To be consistent with Will, need:
  # plant, leaf, stalk, cob/tassle, grain. SO let's make it stalk/cob/tassle
  mutate(leaf = ds_gleafwtsubsam_g + ds_gleafwtother_g + ds_deadleafwt_g,
         stalkcobtass = ds_stemtass_g + ds_husks_g + ds_cobs_g + ds_ears_g,
         grain = ds_kernals_g,
         grain500 = ds_krnl500_g,
         plant = leaf + stalkcobtass + grain,
         nu_pl = ds_nopl) %>% 
  select(-contains("ds")) %>% 
  gather(leaf:plant, key = organ, value = mass_g) %>% 
  mutate(mass_gpl = mass_g/nu_pl) %>% 
  select(year, doy, plot_id, organ, mass_gpl)
  
lai18 <- 
  bm18raw %>% 
  mutate(lai_cm2pl = ds_gLAI_cm2 / ds_noplsubsam) %>% 
  select(year, doy, plot_id, lai_cm2pl) 


# 2019 LAI and biomass ----------------------------------------------------

#--LAI

lai19raw <- 
  tibble(files = list.files(mydir)) %>%
  mutate(path = paste0(mydir, files)) %>%
  filter(grepl('lai', files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  fill(date)

lai19 <- 
  lai19raw %>% 
  mutate(year = year(date),
         doy = yday(date)) %>% 
  left_join(pk) %>% 
  mutate(lai_cm2pl = LAI_cm2 / subsampl_no) %>% 
  select(year, doy, plot_id, lai_cm2pl) %>% 
  arrange(year, doy, plot_id)

#--biomass

bm19raw <- 
  tibble(files = list.files(mydir)) %>%
  mutate(path = paste0(mydir, files)) %>%
  filter(grepl('biomass', files)) %>%
  mutate(data = path %>% map(read_excel, skip = 5)) %>%
  select(data) %>%
  unnest(cols = c(data)) %>%
  fill(date, plot)


bm19 <- 
  bm19raw %>% 
  # subtract weight of bag
  mutate(wgtbag_g = ifelse(is.na(wgtbag_g), 0, wgtbag_g),
         wgt_g = wgtall_g - wgtbag_g) %>%
  select(-wgtall_g, -wgtbag_g) %>%
  mutate(wgt_g = ifelse(is.na(wgt_g), 0, wgt_g)) %>%
  # fix date and plot things
  mutate(year = year(date),
         doy = yday(date)) %>% 
  left_join(pk) %>% 
  select(year, doy, plot_id, organ, wgt_g) %>% 
  arrange(year, doy, plot_id) %>% 
  spread(organ, value = wgt_g) %>% 
  # To be consistent with Will, need:
  # plant, leaf, stalk, cob/tassle, grain. SO let's make it stalk/cob/tassle
  mutate(stalkcobtass = stemtass + ear_husk + ear_cob,
         grain = ear_kernals,
         grain500 = kernals500,
         leaf = brnleaf + LAIgleaf + othergleaf,
         plant = stalkcobtass + grain + leaf) %>% 
  select(year, doy, plot_id, plant, leaf, stalkcobtass, grain, grain500) %>% 
  gather(plant:grain500, key = organ, value = mass_g) %>% 
  mutate(mass_gpl = mass_g/8) %>% #--always 8 plants in 2019
  select(-mass_g)

# make lai and biomass datasets -------------------------------------------

mrs_cornbio_vn <- 
  bind_rows(bm18, bm19) %>% 
  arrange(year, doy, plot_id, organ)

mrs_cornbio_vn %>%  write_csv("data-raw/_tidy/cornbio_vn.csv")  
usethis::use_data(mrs_cornbio_vn, overwrite = T)

mrs_cornlai_vn <- 
  bind_rows(lai18, lai19) %>% 
  arrange(year, doy, plot_id)

mrs_cornlai_vn %>%  write_csv("data-raw/_tidy/cornlai_vn.csv")  
usethis::use_data(mrs_cornlai_vn, overwrite = T)

  
# # merge 'em ---------------------------------------------------------------
# 
# #--growth analysis = ga
# 
# garaw <- 
#   lairaw %>%
#   left_join(bmraw) %>%
#   mutate(tot_g_m2 = tot_g,
#          tot_g_pl  = tot_g / totpl_no,
#          LAI_m2_pl = LAI_cm2 / subsampl_no / (100^2),
#          LAI_m2_m2 = LAI_cm2 / (100^2),
#          date = as.Date(date)) %>%
#    select(date, plot, tot_g_m2, tot_g_pl, LAI_m2_pl, LAI_m2_m2) 
# 
# garaw %>%
#   write_csv("_theme-2019data/_data/tidy/td_lai-bm.csv")
