## Author: Gina
## created 6/18/2019
## last modified: 6/20/2019 (make date into dd/mm/yyyy format)
##                dec 3 2019 (clean up)
##
## helper functions to process things consistently

# clean the planting tab
f_cleanp <- function(mydat){

  # trouble
  #mydat <- praw 
  
  
  newdat <- 
    mydat %>%
    
    #--drop down years and date
    fill(year) %>%
    fill(date) %>%
    
    #--make APSIM-style date
    mutate(
      date = ymd(date),
      m = month(date),
      mm = str_pad(m, 2, pad = "0"),
      d = day(date),
      dd = str_pad(d, 2, pad = "0"),
      y = year(date),
      Date = paste(dd, mm, y, sep = "/")
    ) %>%
    
    
    #--trts are separated by commas, make them into ind rows
    # first get rid of unintended white space
    mutate(trt = str_replace_all(trt, " ", "")) %>%
    
    # separate rows w/more than one entry
    separate_rows(trt, sep = ",") %>%
    
   #--change crop names to apsim style
    mutate(crop = recode(
      crop,
      corn = "maize",
      alfalfa = "lucerne",
      soy = "soybean",
      #`red clover` = "?", #--don't know what to do with red clover
      oat = "oats"
    )) %>%
    
    #--crop_abb, trt, and system merge with key
    mutate(crop_abb = str_sub(trt, 1, 1),
           system = paste0(str_sub(trt, 2, 2), "yr"))
  
  return(newdat)
}