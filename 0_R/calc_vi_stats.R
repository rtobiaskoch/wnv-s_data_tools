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
    stop("one or more of your your grouping variables (grp_var) do not exist in the data")
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
    filter(method == "L") %>%  # only run on Light Traps
    filter(!zone %in% rm_zone) %>%  # remove specified zones
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
    filter(!zone %in% rm_zone) %>%
    tidyr::unite(col = "grp", all_of(grp_var), sep = "_", remove = FALSE)
  
  mle = PooledInfRate::pIR(test_code ~ total|grp, data =   df_pir, pt.method = "firth")
  
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
  
  #END OF PIR
  
  # Calc VI by combining abundance and PIR
  #-------------------------------------------------------------------------------
  full_join(df_abund, df_pir, by = grp_var) %>%
    mutate(vi = round(abund * pir, 4),
           vi_lci = round(abund * pir_lci, 4),
           vi_uci = round(abund * pir_uci, 4))
  
} #end of function


