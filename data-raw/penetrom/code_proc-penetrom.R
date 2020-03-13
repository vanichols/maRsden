#########################
# Created: Dec 16 2019
#
# updated: feb 18 2020 (added date)
#          march 13 2020 (updated pkg structure)
#
# Purpose: Process penetrometer data from 2018 and 2019
#
# NOTES:
#
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data

pk <- read_csv("data-raw/plotkey/plotkey.csv")


# 2018 --------------------------------------------------------------------


# Data from first sampling June 11 2019 ------------------------------------------------
# or is it may 9? that's the time stamp....

#--soil moisture measurements
smraw <- read_excel("data-raw/penetrom/rd_penetrometer-soilmois-20180509.xlsx", skip = 5) %>%
  mutate(samp_ID = paste(block, trt, N, sep = "-"))

#--'raw' file from olk, I changed the first two columns apparently, has no date
# taken in may
mayraw <- read_excel("data-raw/penetrom/rd_penetrometer-OLK-20180509.xlsx",
                  skip = 5)


#--doesn't include date, but file is named 20180716, taken in july assumed
julyraw <- read_excel("data-raw/penetrom/rd_penetrometer-Gina-20180716.xlsx", skip = 5)


# wrangle may data-----------------------------------------------------------------

#--get may soil moisture data clean and tidy
sm <-
  smraw %>%
  #--sampled over 2 days, just change it to the second day's date
  mutate(date = ymd("2018-05-10"),
         year = year(date),
         doy = yday(date)) %>%
  #select(-date) %>%
  #--soil depth is just to note if I didn't go deep
  filter(!is.na(soildepth_in)) %>%
  mutate_at(c("wetsoil_g", "wetbag_g", "drysoil_g", "drybag_g"), as.numeric) %>%
  #--calc moisture on a g.g basis
  mutate(wet = wetsoil_g - wetbag_g,
         dry = drysoil_g - drybag_g,
         soilh2o_g.g = (wet - dry) / dry) %>%
  #--omg I don't have plot, what was I doing
  mutate(block = tolower(block),
         harv_crop = trt,
         depth_cm = "0-45") %>%
  #--merge w/plot key
  left_join(pk) %>%
  select(year, date, doy, plot_id,
         soilpair_YN, depth_cm, soilh2o_g.g, samp_ID) %>%
  filter(!is.na(soilh2o_g.g))


#--get may penetrometer readings clean and tidy
may <-
  mayraw %>%
  #--remove 'Logger Started' rows
  filter(Number != 'Logger Started') %>%
  #--remove XX measurements (practice)
  filter(Number != 'XX') %>%
  #--Make N column that matches sm data
  mutate(N = str_trim( (str_sub(Number, -2, -1))),
         N = paste("N", N, sep = ""),
         N = ifelse(soilpair_YN == "N", paste("X", N, sep = ""), N)) %>%

  # Make samp_ID that matches sm data (block, trt, N)
  mutate(samp_ID = paste(block, trt, N, sep = "-")) %>%

  # gather depths into one col
  gather(depth_00:depth_18, key = "depth_in", value = "resis_psi") %>%
  select(-Number, -`Cone Size`) %>%
  # sort that shit
  arrange(samp_ID, depth_in) %>%
  mutate(date = ymd("2018-05-10"),
         year = year(date),
         doy = yday(date)) %>%
  # make mergable with plotkey
  mutate(block = tolower(block),
         harv_crop = trt) %>%
  left_join(pk) %>%
  select(year, date, doy, plot_id, soilpair_YN, samp_ID, depth_in, resis_psi) %>%
  # Merge soil moisture + olk penetrometer
  # NOTE: I think the depths are off. What it thinks is 1, is actually 0.  But whatever.
  left_join(sm) %>%
  # Change depth_in to cm, psi to kpa
  mutate(depth_in = as.numeric(str_sub(depth_in, -2, -1)),
         depth_cm = depth_in * 2.54,
         resis_kpa = resis_psi * 6.89476)

# wrangle july data-----------------------------------------------------------------

july <-
  julyraw %>%
  # remove 'Logger Started' rows
  filter(Number != 'Logger Started') %>%
  # remove XX measurements
  filter(Number != 'XX') %>%
  # Make an N column
  mutate(N = str_trim( (str_sub(Number, -2, -1))),
         N = paste("N", N, sep = "")) %>%
  # Make samp_ID that matches sm data (block, trt, N)
  # Make samp_ID and rep column
  mutate(block = paste0("B", plot %/% 10),
         samp_ID = paste(block, trt, N, sep = "-")) %>%
  # gather depths into one col
  gather(depth_00.0:depth_45.0, key = "depth_cm", value = "resis_kpa") %>%
  select(-Number, -`Cone Size`, -N) %>%
  # sort it
  arrange(samp_ID, depth_cm) %>%
  # fix date, no moisture data for this
  mutate(date = ymd("2018-07-16"),
         year = year(date),
         doy = yday(date),
         soilpair_YN = 'N',
         soilh2o_g.g = NA,
         depth_cm = as.numeric(str_sub(depth_cm, -4, -1))) %>%
  # make mergable with plotkey
  mutate(block = tolower(block),
         harv_crop = trt) %>%
  left_join(pk) %>%
  select(year, date, doy, plot_id, soilpair_YN, soilh2o_g.g, samp_ID, depth_cm, resis_kpa) %>%
  #--for some reason values below 40 are nonsense, filter them out
  filter(depth_cm <= 40)


july %>%
  group_by(date, doy, plot_id, depth_cm) %>%
  summarise(resis_kpa = mean(resis_kpa)) %>%
  ggplot(aes(depth_cm, resis_kpa, group = doy)) +
  geom_line(size = 2, aes(color = doy)) +
  coord_flip() +
  scale_x_reverse()  +
  facet_grid(.~plot_id)



# Combine may and july 2018 data ------------------------------------------

pen18 <- bind_rows(may, july) %>%
  select(year, date, doy, plot_id, soilh2o_g.g, depth_cm, resis_kpa)


# look at it --------------------------------------------------------------

pen18 %>%
  group_by(doy, plot_id, depth_cm) %>%
  summarise(resis_kpa = mean(resis_kpa)) %>%

  ggplot(aes(depth_cm, resis_kpa, group = doy)) +
  geom_line(size = 2, aes(color = doy)) +
  coord_flip() +
  scale_x_reverse()  +
  facet_grid(.~plot_id)


# 2019 data ---------------------------------------------------------------


# Data from June 11 2019 ------------------------------------------------

jun19raw <- read_excel("data-raw/penetrom/rd_20190611-penet.xlsx", skip = 4)

pen19 <-
  jun19raw %>%
  clean_names() %>%
  select(-longitude, -latitude, -cone_size, -trt_guess) %>%
  rename(plot = plot_guess) %>%
  #--remove 'Logger Started' rows
  filter(number != 'Logger Started',
         !is.na(plot_anon)) %>%
  #--assign date
  mutate(date = ymd("2019-06-11"),
         year = year(date),
         doy = yday(date)) %>%
  #--merge w/key
  left_join(pk) %>%
  #--Make and N column that matches sm data
  mutate(N = str_trim( (str_sub(number, -2, -1))),
         N = paste("N", N, sep = "")) %>%
  #--gather depths into one col, make numeric
  gather(x0_0_cm:x45_0_cm, key = "depth_cm", value = "resis_kpa") %>%
  mutate(depth_cm = str_sub(depth_cm, start = 2, end = -4),
         depth_cm = str_replace(depth_cm, "_", "."),
         depth_cm = as.numeric(depth_cm),
         soilpair_YN = 'N',
         soilh2o_g.g = NA) %>%
  #--weird 0 values and low values at >40 cm depth
  filter(resis_kpa >0) %>%
  filter(!(depth_cm > 40 & resis_kpa <500)) %>%
  select(year, date, doy, plot_id, soilh2o_g.g, depth_cm, resis_kpa)


pen19 %>%
  group_by(doy, plot_id, depth_cm) %>%
  summarise(resis_kpa = mean(resis_kpa)) %>%

  ggplot(aes(depth_cm, resis_kpa, group = doy)) +
  geom_line(size = 2, aes(color = doy)) +
  coord_flip() +
  scale_x_reverse()  +
  facet_grid(.~plot_id)



# combine 2018 and 2019 data ----------------------------------------------

# average over sub-reps, so we only have one value for each plot

mrs_penetrom <-
  bind_rows(pen18, pen19) %>%
  group_by(year, date, doy, plot_id, depth_cm) %>%
  summarise(soilh2o_g.g = mean(soilh2o_g.g, na.rm = T),
            resis_kpa = mean(resis_kpa, na.rm = T)) %>%
  arrange(year, date, doy, plot_id, depth_cm)

mrs_penetrom %>% write_csv("data-raw/penetrom/penetrom.csv")
usethis::use_data(mrs_penetrom, overwrite = T)
