#############################
# created dec 11 2019
# updated dec 12 2019 (changed layout, added years 2008-2011 that need filled in)
# read in 'raw' plot trt key
# that raw file will be updated each year
# write a tidy file that will be used as data in the package
############################
library(readr)
library(dplyr)
library(stringr)
library(readxl)

# plotkey <- read_csv("data-raw/_raw/rd_year-plot-trt-key.csv") %>%
#   select(-crop_abb) %>%
#   rename("harv_crop" = trt,
#          "rot_trt" = system) %>%
#   mutate(block = str_sub(plot, 1, 1),
#          rot_trt = str_sub(rot_trt, 1,2)) %>%
#   mutate(block = paste0("b", block)) %>%
#   select(year, block, plot, rot_trt, harv_crop) %>%
#   mutate(plot_id = paste(year, plot, sep = "_"))




# 2012-2019 ---------------------------------------------------------------

pk19 <- read_excel("data-raw/_raw/rd_year-plot-trt-key2.xlsx",
                   sheet = "2012-2019") %>%
  mutate(block = str_sub(plot, 1, 1),
         rot_trt = str_sub(rot_trt, 1,2)) %>%
  mutate(block = paste0("b", block)) %>%
  select(year, block, plot, rot_trt, harv_crop) %>%
  mutate(plot_id = paste(year, plot, sep = "_"))


# 2008 through 2011 -------------------------------------------------------

# need to do


plotkey %>%
  write_csv("data-raw/_tidy/plotkey.csv")

mrs_plotkey <- plotkey
usethis::use_data(mrs_plotkey, overwrite = T)


