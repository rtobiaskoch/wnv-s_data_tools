
calc_pools = function(df = database_update, 
                      grp_vars = c("zone", "year", "week", "spp"), #variables to calculate the pools by
                      grp_col = "zone",
                      grp_vals = c("NE", "SE", "NW", "SW"), #variable names in zone to calculate
                      grp_replace = "FC",
                      spp_cmplt = c("Pipiens", "Tarsalis"),
                      zone_cmplt = c("NE", "SE", "NW", "SW","LV", "BC", "BE")
                      ) {
  
  
  req_col = c(grp_vars,"test_code")
  miss_col = setdiff(req_col, names(df))
  
  if(length(miss_col) > 0){
    stop(cat("For calc pools columns: ", paste(miss_col, collapse = ","), " are missing."))
  }
   
  grp_vars_sym = rlang::syms(grp_vars)

  #complete missing function
  cmplt_missing = function(df) {
    df %>% 
      complete(year, week, zone = zone_cmplt, spp = spp_cmplt) %>%
      mutate(n_pools = replace_na(n_pools, 0)) %>%
      mutate(n_pos_pools = replace_na(n_pos_pools, 0))
  }

  
  
  #calculate number of pools  
  pools0 = df %>% 
    group_by(!!!grp_vars_sym) %>%
    summarise(n_pools = n(),
              n_pos_pools = sum(test_code, na.rm = T),
              .groups = "drop") %>%
    ungroup
  
  #fill in any missing pools
  pools0 =  pools0 %>% cmplt_missing()
  
  
  
  fc_pools = df %>% 
    filter(!!sym(grp_col) %in% grp_vals) %>%
    mutate(!!sym(grp_col) := grp_replace) %>%
    group_by(!!!grp_vars_sym) %>%
    summarise(n_pools = n(),
              n_pos_pools = sum(test_code, na.rm = T),
              .groups = "drop") %>%
    ungroup
  
  #fill in any missing pools
  fc_pools =  fc_pools %>% 
    complete(year, week, spp = spp_cmplt) %>%
    mutate(n_pools = replace_na(n_pools, 0)) %>%
    mutate(n_pos_pools = replace_na(n_pos_pools, 0))
  
  pools =  rbind(pools0, fc_pools)
  
  if(sum(pools0$n_pools) == nrow(df)) {
    cat("Number of pools match rows in input. Nice.")
  } else {
    stop("Number of pools don't match rows in input.")
  }
  
  return(pools)
  
  # Old version that separates out the pools by method
  # df %>%
  # filter(zone %in% fc_zones) %>%
  # mutate(zone = "FC") %>%
  # group_by(across(all_of(c(grp_vars, "method")))) %>%
  # summarise(n_pools = n(),
  #           n_pos_pools = sum(test_code, na.rm = T),
  #           .groups = "drop") %>%
  # pivot_wider(names_from = method, values_from = n, 
  #             names_prefix = "pools_", values_fill = 0) %>%
  # mutate(n_pools = pools_L + pools_G) %>%
  # ungroup
  
}




