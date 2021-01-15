#############################
##
## Dec 11 2019
## updated Dec 12, using plot_id in all data
## Process Will's data
## Writes file to _tidy and put in package
##
## last updated: feb 18 2020 (added dates)
##############################

rm(list=ls())
library(tidyverse)
library(lubridate)
library(janitor)

pk <- read_csv("data-raw/plotkey/plotkey.csv")

# 2013 --------------------------------------------------------------------

w13 <- read_csv("data-raw/cornbio_wo/raw_cornbio13_wo.csv", skip = 2) %>%
  clean_names()

y13 <-
  w13  %>%
  mutate(date = as.character(date),
         lubedate = mdy(date),
         year = year(lubedate),
         doy = yday(lubedate)) %>%
  select(-date) %>%
  rename(date = lubedate) %>%
  select(year, date, doy, everything())

# Separate his cols by mass (4-8), whole plant cn (9-12), leaf cn (13-16)
#  stalk (17-20), cob+tassle(21-24),  grain (25-28)
# Rename if necssary

# mass
m13 <-
  y13 %>%
  select(year:grain_g) %>%
  rename("plant" = whole_plant_g,
         "leaf" = leaf_g,
         "stalk" = stalk_g,
         "cobtass" = cob_tassle_g,
         "grain" = grain_g) %>%
  pivot_longer(plant:grain, names_to = "organ", values_to = "mass_g")


# whole plant
p13 <-
  y13 %>%
  select(year:plot,
         total_c_g:percent_n) %>%
  rename("totC_g" = total_c_g,
         "totN_g" = total_n_g,
         "CN_ratio" = c_n,
         "N_prct" = percent_n) %>%
  mutate(organ = "plant")

# leaf
l13 <-
  y13 %>%
  select(year:plot,
         total_c_g_1:percent_n_1) %>%
  rename("totC_g" = total_c_g_1,
         "totN_g" = total_n_g_1,
         "CN_ratio" = c_n_1,
         "N_prct" = percent_n_1) %>%
  mutate(organ = "leaf")

# stalk
s13 <-
  y13 %>%
  select(year:plot,
         total_c_g_2:percent_n_2) %>%
  rename("totC_g" = total_c_g_2,
         "totN_g" = total_n_g_2,
         "CN_ratio" = c_n_2,
         "N_prct" = percent_n_2) %>%
  mutate(organ = "stalk")

# cob tassle
ct13 <-
  y13 %>%
  select(year:plot,
         total_c_g_3:percent_n_3) %>%
  rename("totC_g" = total_c_g_3,
         "totN_g" = total_n_g_3,
         "CN_ratio" = c_n_3,
         "N_prct" = percent_n_3) %>%
  mutate(organ = "cobtassle")

# grain
g13 <-
  y13 %>%
  select(year:plot,
         total_c_g_4:percent_n_4) %>%
  rename("totC_g" = total_c_g_4,
         "totN_g" = total_n_g_4,
         "CN_ratio" = c_n_4,
         "N_prct" = percent_n_4) %>%
  mutate(organ = "grain")

CN13 <- bind_rows(p13, l13, s13, ct13, g13)

d13 <-
  CN13 %>%
  left_join(m13) %>%
  select(year, date, doy, plot, organ, mass_g, everything()) %>%
  filter(!is.na(year))


# 2014 --------------------------------------------------------------------

# same thing as above

w14 <- read_csv("data-raw/cornbio_wo/raw_cornbio14_wo.csv", skip = 2) %>%
  clean_names()

y14 <-
  w14  %>%
  mutate(date = as.character(date),
         lubedate = mdy(date),
         year = 2014, #--there are typos, it switches to 2019 in september...
         doy = yday(lubedate)) %>%
  select(-date) %>%
  rename(date = lubedate) %>%
  select(year, date, doy, everything())


# mass
m14 <-
  y14 %>%
  select(year:grain_g) %>%
  rename("plant" = whole_plant_g,
         "leaf" = leaf_g,
         "stalk" = stalk_g,
         "cobtass" = cob_tassle_g,
         "grain" = grain_g) %>%
  pivot_longer(plant:grain, names_to = "organ", values_to = "mass_g")


# whole plant cn
p14 <-
  y14 %>%
  select(year:plot,
         total_c_g:percent_n) %>%
  rename("totC_g" = total_c_g,
         "totN_g" = total_n_g,
         "CN_ratio" = c_n,
         "N_prct" = percent_n) %>%
  mutate(organ = "plant")

# leaf
l14 <-
  y14 %>%
  select(year:plot,
         total_c_g_1:percent_n_1) %>%
  rename("totC_g" = total_c_g_1,
         "totN_g" = total_n_g_1,
         "CN_ratio" = c_n_1,
         "N_prct" = percent_n_1) %>%
  mutate(organ = "leaf")

# stalk
s14 <-
  y14 %>%
  select(year:plot,
         total_c_g_2:percent_n_2) %>%
  rename("totC_g" = total_c_g_2,
         "totN_g" = total_n_g_2,
         "CN_ratio" = c_n_2,
         "N_prct" = percent_n_2) %>%
  mutate(organ = "stalk")

# cob tassle
ct14 <-
  y14 %>%
  select(year:plot,
         total_c_g_3:percent_n_3) %>%
  rename("totC_g" = total_c_g_3,
         "totN_g" = total_n_g_3,
         "CN_ratio" = c_n_3,
         "N_prct" = percent_n_3) %>%
  mutate(organ = "cobtassle")

# grain
g14 <-
  y14 %>%
  select(year:plot,
         total_c_g_4:percent_n_4) %>%
  rename("totC_g" = total_c_g_4,
         "totN_g" = total_n_g_4,
         "CN_ratio" = c_n_4,
         "N_prct" = percent_n_4) %>%
  mutate(organ = "grain")

CN14 <- bind_rows(p14, l14, s14, ct14, g14)

library(saapsim)

d14 <-
  CN14 %>%
  left_join(m14) %>%
  select(year, doy, plot, organ, mass_g, everything(), -date) %>%
  rowwise() %>%
  mutate(date = saf_doy_to_date(mydoy = doy, myyear = 2014)) #--year was wrong in date in september

# write it ----------------------------------------------------------------

mrs_cornbio_wo <-
  rbind(d13, d14) %>%
  left_join(pk) %>%
  select(year, date, doy, plot_id, organ, mass_g, totC_g, totN_g, CN_ratio, N_prct)

mrs_cornbio_wo %>%
  ggplot(aes(doy, mass_g, color = organ)) +
  geom_point() +
  facet_grid(.~year)

mrs_cornbio_wo %>%  write_csv("data-raw/cornbio_wo/cornbio_wo.csv")
usethis::use_data(mrs_cornbio_wo, overwrite = T)
