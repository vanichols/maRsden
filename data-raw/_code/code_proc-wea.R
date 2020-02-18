#############################
##
## Dec 16 2019
## Process Iowa Mesonet files
## Writes file to _tidy and put in package
##
## last updated: feb 18 2020 (added dates)
##############################

rm(list=ls())
library(tidyverse)
library(lubridate)


# h2o data ----------------------------------------------------------------


wear <- read_csv("data-raw/_raw/wea/raw_wthr1987-2017_im.csv")
wea18r <- read_csv("data-raw/_raw/wea/raw_wthr2018_im.csv")


# wrangle -----------------------------------------------------------------

# wea 'day' is a character, but wea18 day is a date, so must handle separately
wea <-
  wear %>%
  mutate(date = as.character(day),
         lubedate = mdy(date),
         year = year(lubedate),
         doy = yday(lubedate)) %>%
  select(-date) %>%
  rename(date = lubedate) %>%
  select(year, date, doy, everything(), -day)

wea18 <-
  wea18r %>%
  mutate(lubedate = ymd(day),
         year = year(lubedate),
         doy = yday(lubedate),
         date = as_date(lubedate)) %>%
  select(year, date, doy, everything(), -lubedate, -day)

# bind weather years tog here
dat <-
  bind_rows(wea, wea18) %>%
  arrange(year, doy) %>%
  rename("hiT_c" = highc,
         "loT_c" = lowc,
         "precip_mm" = precipmm,
         "narr_srad_mjday" = narr_srad) %>%
  select(station, station_name, year, date, doy, everything())

# viz check ---------------------------------------------------------------------

dat %>%
  ggplot(aes(doy, narr_srad_mjday)) +
  geom_line(aes(color = year))


# write it ----------------------------------------------------------------

# make a 2008-present dataset, and a historical one
# to tidy folder (just for reference?)

#--weather 2008-present
mrs_wea_08_pres <-
  dat %>%
  filter(year > 2007)

mrs_wea_08_pres %>% write_csv("data-raw/_tidy/wea_08_pres.csv")
usethis::use_data(mrs_wea_08_pres, overwrite = T)

#--historical weather 1987-present
mrs_wea_hist <-
  dat  %>%
  filter(year > 1986)

mrs_wea_hist %>% write_csv("data-raw/_tidy/wea_hist.csv")
usethis::use_data(mrs_wea_hist, overwrite = T)

