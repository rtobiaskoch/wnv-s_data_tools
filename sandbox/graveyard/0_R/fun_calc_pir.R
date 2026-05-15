#uses the datasheet (pooled data) with a test_code output from the PCR 

calc_pir <- function(df, 
                    grp_var = c("zone", "year", "week", "spp"), #variables to calculate the pools by
                    zone_complete = c("NW", "NE", "SE","SW", "LV", "BE", "BC"), # list of zones to complete the 
                    rm_zone = NULL)  {
  
  #check packages
  #-------------------------------------------------------------------------------
  #For
  if (!require("tidyverse")) install.packages("tidyverse")
  
  #FOR PIR
  if (!require("PooledInfRate")) {
    install.packages("devtools")
    devtools::install_github("https://github.com/CDCgov/PooledInfRate",build_vignettes = TRUE)
  }
  
  
  #check inputs
  #-------------------------------------------------------------------------------
  if ("csu_id" %in% grp_var) {
    stop("Cannot calculate abundance using csu_id as a grouping variable. 
         CSU IDs represent individual mosquito pools, not trap-level data.")
  }
  
  if(any(!grp_var %in% colnames(df))) {
    stop("one or more of the grouping variables (grp_var) do not exist in the data")
  }
  
  
req_var = c("trap_id","year","week","zone", "zone2",
              "method","spp","total")

if(any(!req_var %in% colnames(df))) {
  
  missing = setdiff(req_var, names(df))
  stop(cat("The required variables are not in your data.",
           paste0(missing, collapse = ",")))
}
  
  
  # Create base grouping variables
  #-------------------------------------------------------------------------------

  grp_var_sym <- syms(grp_var)
  #don't fill zones that you don't want in your analysis
  zone_complete = setdiff(zone_complete, rm_zone)
  
  # Calc PIR from the pool data
  #-------------------------------------------------------------------------------
  df_pir = df %>%
    dplyr::filter(!zone %in% rm_zone) %>%
    dplyr::filter(total > 1) %>% #remove the imputed 0 pools and single pools bc test_code otherwise will be NA 
    #breaks the pIR for vi by trap
    tidyr::unite(col = "grp", all_of(grp_var), sep = "_", remove = FALSE)
  
  mle = PooledInfRate::pIR(test_code ~ total|grp, data = df_pir, pt.method = "firth")
  
  
  df_pir = as.data.frame(mle) %>%
    separate(grp,
             into = {{grp_var}},
             sep = "_") %>%
    mutate(pir = round(P,4),
           pir_lci = round(Lower,4),
           pir_uci = round(Upper,4)
          ) %>%
   # mutate(across(all_of(grp_var), as.character)) %>% #ensure left_join will work
    select(-P, -Upper, -Lower)
  
  
  #if there are pools that have pools that are equal to 1
  #add back in the single pools
  if(df %>% 
     dplyr::filter(!zone %in% rm_zone) %>%
     group_by(!!!grp_var_sym) %>%
     summarise(total = sum(total, rm.na = T), .groups = "drop") %>%
     dplyr::filter(total == 1) %>% 
     nrow() > 0) { #if grp pool total == 1
    
    df_pir1 = df %>%
      dplyr::filter(!zone %in% rm_zone) %>%
      group_by(!!!grp_var_sym) %>%
      summarise(total = sum(total, rm.na = T),
                test_code =  max(test_code)) %>%
      dplyr::filter(total  <= 1) %>% #keep 0 ot 1 pools that break pir
      #make pir 1 if positive and 0 if negative because there is only 1 mosquito in pool
      mutate(pir = if_else(test_code == 0|is.na(test_code), 0.0000, 1),
             pir_lci = if_else(test_code == 0|is.na(test_code), 0.0000, 1),
             pir_uci = if_else(test_code == 0|is.na(test_code), 0.0000, 1)) %>%
      select(all_of(grp_var), pir, pir_lci, pir_uci)
    
    df_pir = rbind(df_pir, df_pir1)
  }
  
  
  #complete any combinations of traps and pools that aren't represented in the pooled data
  df_pir = df_pir %>%
    complete(!!!grp_var_sym)
  
  
  df_pir[is.na(df_pir)] = 0# Fill missing values with 0
  
  #rename to zone if zone2 exists
  if("zone2" %in% names(df_pir)) {
    df_pir =   df_pir %>%
      rename(zone = zone2)
  }
  
  
  #add spp if it wasn't in df it means its all spp
  if(!"spp" %in% names(df_pir)) {
    df_pir = df_pir %>%
      mutate(spp = "All")
  }
  
  return(df_pir)
  #END OF PIR

} #end of function


