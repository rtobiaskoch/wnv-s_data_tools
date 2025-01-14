#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#LAST UPDATE: 24-01-13
#CHANGES: formerly called check_read_fun.R
# PACKAGES:
#   1. RIO
#   2. DPLYR
# INPUT - 
#   1. wnv-s_database.csv pulled from gsheets
#   2. config.R for wk and yr filter

# PROCESS -
#   1. reads in csv from gsheet and filters for year and week of interest
#   2. changes zone and spp to leveled factors
#   3. arranges it by year

# OUTPUT - 
#1. r dataframe object in global environment
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

library(dplyr)

check_read_fun = function(input_file, yr = NULL, wk = NULL) {
  
  if(is.null(wk)) {wk = week_filter}
  if(is.null(yr)) {yr = year_filter}
  
  if(file.exists(input_file)) {
    if(exists("year_filter")){
      rio::import(input_file) %>%
        filter(year %in% yr) %>%
        filter(week %in% wk) %>%
        mutate(zone = factor(zone, levels = zone_lvls),
               spp = factor(spp, levels = c("Pipiens", "Tarsalis", "All"))) %>%
        arrange(year, zone, week,spp)
    } else {
      print( "run config.R to get year and week filters or type in year filter")
    }
    
  }else{
    print( "add your data to the 1_input folder check the name in the config.R")
  }
  
}
