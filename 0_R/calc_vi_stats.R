calc_vi_stats = function(df , grp_var) {
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse, PooledInfRate)
  #calculate the PIR
  df2 = df %>%
    tidyr::unite(col = "grp", all_of(grp_var), sep = "-", remove = FALSE)
  
  mle = PooledInfRate::pIR(test_code ~ total|grp, data = df2, pt.method = "firth")
  
  mle
  
  df_pir = as.data.frame(mle) %>%
    separate(grp,
             into = {{grp_var}},
             sep = "-") %>%
    transmute(year = as.integer(year),
              week = as.integer(week),
              zone = zone,
              pir = round(P,4),
              pir_lci = round(Lower,4),
              pir_uci = round(Upper,4)
    )
  #calculate the abundance  
  df_abund = df %>%
    filter(method == "L") %>%
    group_by(across(all_of(grp_var)), trap_id) %>%
    reframe(total = sum(total)) %>% # get total num mosquitoes per trap
    group_by(across(all_of(grp_var))) %>% #now get summary stats for the zone
    reframe(mosq_L = sum(total),
            n = n(),
            abund = round(mean(total),4),
            abund_sd = round(sd(total),4)) %>%
    ungroup %>%
    mutate(abund_lci = round(abund - (1.96*(abund_sd/n^0.5)),4),
           abund_uci = round(abund + (1.96*(abund_sd/n^0.5)),4)
    ) %>%
    mutate(abund_lci = if_else(abund_lci < 0, 0, abund_lci))
  
  #calculate the VI
  df_abund %>%
    left_join(df_pir, by = grp_var) %>% 
    mutate(vi = round(abund * pir,4),
           vi_lci = round(abund * pir_lci,4),
           vi_uci = round(abund * pir_uci,4)
    )
}