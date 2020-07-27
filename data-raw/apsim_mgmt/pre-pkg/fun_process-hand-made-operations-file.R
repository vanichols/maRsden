## Author: Gina
## created 6/18/2019
## last modified: 6/20/2019 (make date into dd/mm/yyyy format)
##
## helper functions to process things consistently

f_procop <- function(mydat){

  # trouble
  #mydat <- praw
  
  newdat <- 
    mydat %>%
    
  fill(year) %>%
  mutate(date = ymd(date),
         m = month(date),
         mm = str_pad(m, 2, pad = "0"),
         d = day(date),
         dd = str_pad(d, 2, pad = "0"),
         y = year(date),
         Date = paste(dd,mm,y,sep = "/")) %>%
  
    #--if trt says ALL, fill it in w/all things
    mutate(trt = ifelse(trt == "ALL", 
                        "C2, C3, C4, 
                        S2, S3, S4,
                        O3, O4,
                        A4", trt)) %>% 
    #--if crop says ALL, fill it in
    mutate(crop = ifelse(crop == "ALL", 
                        "corn, soy, oats, alfalfa", crop)) %>% 
    
    #--get one row for each trt
    # first get rid of unintended white space
    mutate(
      trt = str_replace_all(trt, " ", ""),
      plots = str_replace_all(plots, " ", ""),
      crop = str_replace_all(crop, " ", "")
    ) %>%
    
    # separate rows w/more than one entry
    separate_rows(trt, sep = ",") %>%
    separate_rows(plots, sep = ",") %>%
    separate_rows(crop, sep = ",") %>%
    
    rename("plot" = plots) %>%
    mutate(plot = as.numeric(plot)) %>% 
  
  #--change crop names to apsim style,
  #   cultivars to apsim's likings
  mutate(crop = recode(crop,
                       corn = "maize",
                       alfalfa = "lucerne",
                       soy = "soybean",
                       #`red clover` = "?",
                       oat = "oats"
  )) %>%
    mutate(crop_abb = str_sub(trt, 1, 1),
           system = paste0(str_sub(trt, 2,2), "yr")) 
  
  return(newdat)
}