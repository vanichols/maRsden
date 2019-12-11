#########################
##
## Date of creation: June 11 2019
## Date last modified: June 11 2019
##                     Oct 31 2019 this is a mess....
##
## Author: Gina
## Purpose: Process penetrometer data into one file
##
## Inputs:
##
## Outputs:
##
## NOTE:
##
#########################


##### Clear environment and load packages #####
rm(list=ls())
library(tidyverse)
library(lubridate)
library(readxl) #--used to read Excel files
library(janitor) #--used to clean data

# Data from first sampling June 11 2019 ------------------------------------------------
# or is it may 9? that's the time stamp....

#--key of plot/trt/block
mykey <- read_csv("data-raw/_raw/rd_year-plot-trt-key.csv") %>%
  mutate(block = paste0("B", plot %/% 10)) %>%
  filter(year == 2018)


#--soil moisture measurements
smraw <- read_excel("data-raw/_raw/2018/rd_penetrometer-soilmois-20180509.xlsx", skip = 5) %>%
  mutate(samp_ID = paste(block, trt, N, sep = "-"))

#--'raw' file from olk, I changed the first two columns apparently, has no date
olkraw <- read_excel("data-raw/_raw/2018/rd_penetrometer-OLK-20180509.xlsx",
                  skip = 5)


#--doesn't include date, but file is named 20180716
ginaraw <- read_excel("data-raw/_raw/2018/rd_penetrometer-Gina-20180716.xlsx", skip = 5)


# wrangle -----------------------------------------------------------------

sm <-
  smraw %>%
  mutate(date = as_date(date)) %>%
  filter(!is.na(soildepth_in)) %>%
  mutate_at(c("wetsoil_g", "wetbag_g", "drysoil_g", "drybag_g"), as.numeric) %>%
  mutate(wet = wetsoil_g - wetbag_g,
         dry = drysoil_g - drybag_g,
         soilmois_g.g = (wet - dry) / dry,
         date = ymd("2018-05-10")) %>%
  select(date, soilmois_g.g, soilpair_YN, block, trt, N, samp_ID)

summary(sm$date)
sm %>% ggplot(aes(date)) + geom_histogram()
olk <- olkraw %>%
  # remove 'Logger Started' rows
  filter(Number != 'Logger Started') %>%

  # remove XX measurements
  filter(Number != 'XX') %>%

  # Make and N column that matches sm data
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
  mutate(date = ymd("2018-05-10"))

# Merge soil moisture + olk penetrometer
# NOTE: I think the depths are off. What it thinks is 1, is actually 0.  But whatever.
#~~~~~~~~~~~~~~~~~~~~~~~~

samp1 <- olk %>% left_join(sm, by = c("samp_ID", "block", "trt", "N", "soilpair_YN", "date")) %>%

  # Change depth_in to numeric inches
  mutate(depth_in = as.numeric(str_sub(depth_in, -2, -1)),
         depth_cm = depth_in * 2.54)


samp2 <-
  ginaraw %>%
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
  gather(depth_00.0:depth_45.0, key = "depth_cm", value = "resis_KPa") %>%

  select(-Number, -`Cone Size`) %>%
  # sort that shit
  arrange(samp_ID, depth_cm) %>%

  # Change depth_in to numeric cm
  mutate(depth_cm = as.numeric(str_sub(depth_cm, -4, -1)))


# Combine them, I hate you Gina -------------------------------------------

#--Get them into the same format
samp1clean <-
  samp1 %>%
  left_join(mykey) %>%
  mutate(date = as_date(date),
         samp_doy = yday(date),
         resis_kpa = resis_psi * 6.89476) %>%
    select(year, date, samp_doy, plot, trt, soilpair_YN,
           soilmois_g.g, depth_cm, resis_kpa)

samp2clean <-
  samp2 %>%
  left_join(mykey) %>%
  mutate(date = as_date("2018-07-16"),
         samp_doy = yday(date),
         soilpair_YN = 'N',
         soilmois_g.g = NA) %>%
  rename("resis_kpa" = resis_KPa) %>%
  select(year, date, samp_doy, plot, block, trt, soilpair_YN,
         soilmois_g.g,
         samp_ID, depth_cm, resis_kpa)


pen18 <- bind_rows(samp1clean, samp2clean) %>% select(-block, -samp_ID)


# look at it --------------------------------------------------------------

pen18 %>%
  mutate(depth_in = depth_cm * 2.54,
         resis_psi = resis_kpa / 6.89476,
         block = plot %/% 10) %>%
  group_by(date, trt, plot, block, depth_in) %>%
  summarise(resis_psi = mean(resis_psi)) %>%

  ggplot(aes(depth_in, resis_psi,
             group = interaction(trt,block),
             color = trt)) +
  geom_line(size = 2, alpha = 1) +
  coord_flip() +
  scale_x_reverse()  +
  facet_grid(.~date)

pen18 %>% filter(is.na(date))



pen18 %>% write_csv("data-raw/_tidy/pen18.csv")


