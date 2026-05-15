# Define the function to calculate abundance
calc_vi <- function(abund, pir,
                    by = c("year", "week", "zone", "spp"), 
                    spp_cmplt = c("Tarsalis", "Pipiens"),
                    complete = T,
                    rm_zone = NULL) {
  
  #check packages
  #-------------------------------------------------------------------------------
  #FOR BOTH ABUNDANCE AND PIR
  if (!require("tidyverse")) install.packages("tidyverse")

  if("spp0" %in% names(abund)) {
    abund = abund %>% select(-spp0)
  }
  
  if("spp0" %in% names(pir)) {
    pir = pir %>% select(-spp0)
  }
  
  vi = full_join(abund, pir, by = grp_vars) %>%
    filter(!zone %in% rm_zone) %>%
    mutate(vi = round(abund*pir,2),
           vi_lci = round(abund*pir_lci,2),
           vi_uci = round(abund*pir_uci,2)
           )
  
  if(complete) {
    vi = vi %>% 
      complete(zone = grp_zones,
               spp = spp_cmplt,
               year, week)
  }

  vi = vi %>%
    arrange(year, week, zone, spp)
  
  return(vi)
  
} #end of function


