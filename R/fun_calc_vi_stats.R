# Define the function to calculate abundance
calc_vi <- function(data, grp_var, rm_zone = c("BC")) {
  
  #check packages
  #-------------------------------------------------------------------------------
  #FOR BOTH ABUNDANCE AND PIR
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
  
  if(any(!grp_var %in% colnames(data))) {
    stop("one or more of the grouping variables (grp_var) do not exist in the data")
  }
  
  
  req_var = c("csu_id","trap_id","year","week","zone", 
              "method","spp","total","test_code")
  
  if(any(!req_var %in% colnames(data))) {
    stop(cat("one or more of the required variables are not in your data.",
             req_var))
  }
  
  
  # Create base grouping variables
  #-------------------------------------------------------------------------------
  base_vars <- c("year", "week", "trap_id")
  # Combine with user-provided grp_var and remove duplicates
  grp_var1 <- unique(c(base_vars, grp_var))
  # Convert grouping variables to symbols for use in dplyr
  grp_vars_sym <- syms(grp_var1)
  grp_var_final <- syms(grp_var)
  
  # Calc abundance
  #-------------------------------------------------------------------------------
  # First summarization by year, week, trap_id, and grp_var
  df_abund <- data %>%
    dplyr::filter(method == "L") %>%  # only run on Light Traps
    dplyr::filter(!zone %in% rm_zone) %>%  # remove specified zones
    group_by(!!!grp_vars_sym) %>%
    summarize(
      n_trap = n_distinct(trap_id),      
      mosq_L0 = sum(total, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Second summarization by just grp_var
  df_abund <- df_abund %>%
    group_by(!!!grp_var_final) %>%
    summarize(
      mosq_L = sum(mosq_L0),
      n_trap = sum(n_trap),
      abund = round(mosq_L/n_trap, 4),
      abund_sd = if_else(n_trap > 1,
                         round(sd(mosq_L0), 2),
                         0),
      .groups = 'drop') %>%   
    mutate(abund_lci = round(abund - (1.96*(abund_sd/n_trap^0.5)),4),
           abund_uci = round(abund + (1.96*(abund_sd/n_trap^0.5)),4)
    ) %>%
    mutate(abund_lci = if_else(abund_lci < 0, 0, abund_lci)) %>%
    mutate(across(all_of(grp_var), as.character)) #ensure left_join will work
  
  #END OF ABUNDANCE
  
  
  
  # Calc PIR from the pool data
  #-------------------------------------------------------------------------------
  df_pir = data %>%
    dplyr::filter(!zone %in% rm_zone) %>%
    dplyr::filter(total > 1) %>% #remove the imputed 0 pools  and single pools bc test_code is NA 
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
    mutate(across(all_of(grp_var), as.character)) %>% #ensure left_join will work
    select(-P, -Upper, -Lower)
  
  
  #if there are pools that have pools that are equal to 1
  #add back in the single pools
  if(data %>% 
     dplyr::filter(!zone %in% rm_zone) %>%
     group_by(!!!grp_var_final) %>%
     summarise(total = sum(total, rm.na = T), .groups = "drop") %>%
     dplyr::filter(total == 1) %>% 
     nrow() > 0) { #if grp pool total == 1
    df_pir1 = data %>%
      group_by(!!!grp_var_final) %>%
      summarise(total = sum(total, rm.na = T),
                test_code =  max(test_code)) %>%
      dplyr::filter(!zone %in% rm_zone) %>%
      dplyr::filter(total  <= 1) %>% #keep 0 ot 1 pools that break pir
      #make pir 1 if positive and 0 if negative because there is only 1 mosquito in pool
      mutate(pir = if_else(test_code == 0|is.na(test_code), 0, 1),
             pir_lci = if_else(test_code == 0|is.na(test_code), 0, 1),
             pir_uci = if_else(test_code == 0|is.na(test_code), 0, 1)) %>%
      select(all_of(grp_var), pir, pir_lci, pir_uci)
    
    df_pir = rbind(df_pir, df_pir1)
  }
  
  #END OF PIR
  
  
  # Calc Positive pools
  #------------------------------------------------------------------------------- 
  pos_pools = data %>% 
    dplyr::filter(!zone %in% rm_zone) %>%
    dplyr::filter(total > 1) %>%
    group_by(!!!grp_var_final) %>%
    summarise(n_pos_pool = sum(test_code, na.rm = T),
              .groups = "drop") %>%
    mutate(n_pos_pool = if_else(is.na(n_pos_pool), 0, n_pos_pool)) %>%
    mutate(across(all_of(grp_var), as.character)) #ensure left_join will work
  
  # COMBINE pir AND POS POOLS
  #-------------------------------------------------------------------------------  
  df_pir = left_join(df_pir, pos_pools, by = grp_var)
  
  # Calc VI by combining abundance and PIR
  #-------------------------------------------------------------------------------
  full_join(df_abund, df_pir, by = grp_var) %>%
    mutate(vi = round(abund * pir, 4),
           vi_lci = round(abund * pir_lci, 4),
           vi_uci = round(abund * pir_uci, 4))
  
} #end of function


