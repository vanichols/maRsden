#############################
##
## Dec 11 2019
## updated Dec 12, using plot_id in all data
## Process Will's data
## Writes file to _tidy and put in package
##
##############################

rm(list=ls())
library(tidyverse)
library(lubridate)
library(janitor)

# 2013 --------------------------------------------------------------------

w13 <- read_csv("data-raw/_raw/wo/raw_cornbio13_wo.csv", skip = 2) %>%
  clean_names()

y13 <- w13  %>%
  mutate(date = as.character(date),
         lubedate = mdy(date),
         year = year(lubedate),
         doy = yday(lubedate)) %>%
  select(year, doy, everything(), -lubedate, -date)

# Separate his cols by mass (4-8), whole plant (9-12), leaf (13-16)
#  stalk (17-20), cob+tassle(21-24),  grain (25-28)
# Rename if necssary

# mass
m13 <- y13 %>% select(1:9) %>%
  rename("plant" = !!names(.[5]),
         "leaf" = !!names(.[6]),
         "stalk" = !!names(.[7]),
         "cobtass" = !!names(.[8]),
         "grain" = !!names(.[9])) %>%
  gather(organ, mass_g, -year, -doy, -system, -plot)


# whole plant
p13 <- y13 %>% select(1:4, 10:13) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "plant")

# leaf
l13 <- y13 %>% select(1:4, 14:17) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "leaf")

# stalk
s13 <- y13 %>% select(1:4, 18:21) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "stalk")

# cob tassle
ct13 <- y13 %>% select(1:4, 22:25) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "cobtassle")

# grain
g13 <- y13 %>% select(1:4, 26:29) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "grain")

CN13 <- bind_rows(p13, l13, s13, ct13, g13)
d13 <- CN13 %>% left_join(m13) %>%
  select(year, doy, plot, organ, mass_g, everything(), -system)


# 2014 --------------------------------------------------------------------

# same thing as above

w14 <- read_csv("data-raw/_raw/wo/raw_cornbio14_wo.csv", skip = 2) %>%
  clean_names()

y14 <- w14  %>%
  mutate(date = as.character(date),
         lubedate = mdy(date),
         year = year(lubedate),
         doy = yday(lubedate)) %>%
  select(year, doy, everything(), -lubedate, -date)

m14 <- y14 %>% select(1:9) %>%
  rename("plant" = !!names(.[5]),
         "leaf" = !!names(.[6]),
         "stalk" = !!names(.[7]),
         "cobtass" = !!names(.[8]),
         "grain" = !!names(.[9])) %>%
  gather(organ, mass_g, -year, -doy, -system, -plot)


p14 <- y14 %>% select(1:4, 10:13) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "plant")

l14 <- y14 %>% select(1:4, 14:17) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "leaf")

s14 <- y14 %>% select(1:4, 18:21) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "stalk")

ct14 <- y14 %>% select(1:4, 22:25) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "cobtassle")

g14 <- y14 %>% select(1:4, 26:29) %>%
  rename("totC_g" = !!names(.[5]),
         "totN_g" = !!names(.[6]),
         "CN_ratio" = !!names(.[7]),
         "N_prct" = !!names(.[8])) %>%
  mutate(organ = "grain")

CN14 <- bind_rows(p14, l14, s14, ct14, g14)
d14 <- CN14 %>% left_join(m14) %>%
  select(year, doy, plot, organ, mass_g, everything(), -system)



# write it ----------------------------------------------------------------
plotkey <- read_csv("data-raw/_tidy/plotkey.csv")

dat <- rbind(d13, d14) %>%
  left_join(plotkey) %>%
  select(plot_id, doy, everything(), -year, -block, -rot_trt, -harv_crop, -plot)

# to tidy folder (just for reference?)

dat %>% write_csv("data-raw/_tidy/cornbio_wo.csv")

# name it what I want the actual dtaa call to be
mrs_cornbio_wo <- dat

usethis::use_data(mrs_cornbio_wo, overwrite = T)
