#############################
# created dec 11 2019
# updated dec 12 2019 (changed layout, added years 2008-2011 that need filled in)
#         march 13 2020 (updating pkg structure)
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


# 2002 through 2011 -------------------------------------------------------

# need to do. just extract from Matt's raw data

pk02 <-
  readxl::read_excel("data-raw/yields/raw/raw-from-ML/Corn yield 2002-2020.xlsx") %>%
  janitor::clean_names() %>%
  mutate(rot_trt = paste0(rotation, "y"),
         block = paste0("b", block),
         harv_crop = paste0("C", rotation),
         plot_id = paste(year, plot, sep = "_")) %>%
  select(plot_id, year, block, plot, rot_trt, harv_crop)


# 2012-2019 ---------------------------------------------------------------

pk19 <- read_excel("data-raw/plotkey/rd_year-plot-trt-key2.xlsx",
                   sheet = "2012-2019") %>%
  mutate(block = str_sub(plot, 1, 1),
         rot_trt = str_sub(rot_trt, 1,2)) %>%
  mutate(block = paste0("b", block)) %>%
  select(year, block, plot, rot_trt, harv_crop) %>%
  mutate(plot_id = paste(year, plot, sep = "_"))



# 2020 ---------------------------------------------------------------

pk20 <- read_excel("data-raw/plotkey/rd_year-plot-trt-2020.xlsx") %>%
  mutate(block = str_sub(plot, 1, 1),
         rot_trt = str_sub(rot_trt, 1,2)) %>%
  mutate(block = paste0("b", block)) %>%
  select(year, block, plot, rot_trt, harv_crop) %>%
  mutate(plot_id = paste(year, plot, sep = "_"))


plotkey <-
  pk02 %>%
  bind_rows(pk19) %>%
  bind_rows(pk20) %>%
  arrange(year, block, plot)


# make a map thing --------------------------------------------------------

mrs_plotcoords <-
  plotkey %>%
  select(plot) %>%
  mutate(
    plot_group = ifelse(plot < 30, 1, 2),
    x = ifelse(plot_group == 1, plot - 10, plot - 30),
    xend = x + 1,
    y = ifelse(plot_group == 2, 0, 6),
    yend = ifelse(plot_group == 2, 5, 11)) %>%
  select(-plot_group)

plotkey %>%
  write_csv("data-raw/plotkey/plotkey.csv")

mrs_plotkey <- plotkey
usethis::use_data(mrs_plotkey, overwrite = T)

mrs_plotcoords %>%
  write_csv("data-raw/plotkey/plotcoords.csv")

usethis::use_data(mrs_plotkey, overwrite = T)
usethis::use_data(mrs_plotcoords, overwrite = T)

