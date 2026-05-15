
calc_all = function(abund_input, pir_input) {
  
  
  #ABUNDANCE 
  #group by zone
  abund0 = calc_abund(abund_input, 
                      grp_var = c("zone", "year", "week", "spp"))
  
  #group FC
  abund_fc = calc_abund(abund_input, 
                        grp_var = c("zone2", "year", "week", "spp"))
  
  #group all spp
  abund_all = calc_abund(abund_input, 
                         grp_var = c("zone", "year", "week"))
  
  #group all FC
  abund_all_fc = calc_abund(abund_input, 
                            grp_var = c("zone2", "year", "week"))
  
  abund = bind_rows(abund0, 
                    abund_all, 
                    abund_fc, 
                    abund_all_fc) %>%
    distinct_all() %>%
    wnv_s_clean(all_cols = names(.), silence = T) %>%
    arrange(year, week, zone, spp)
  
  #PIR 
  pir0 = calc_pir(pir_input,
                  grp_var = c("zone", "year", "week", "spp"))
  
  pir_fc = calc_pir(pir_input , 
                    grp_var = c("zone2", "year","spp", "week"))
  
  pir_all = calc_pir(pir_input , 
                     grp_var = c("zone", "year", "week"))
  
  pir_fc_all = calc_pir(pir_input , 
                        grp_var = c("zone2", "year", "week"))
  
  pir = bind_rows(pir0, pir_all,
                  pir_fc, pir_fc_all) %>% 
    distinct_all() %>%
    wnv_s_clean(all_cols = names(.), silence = T) %>%
    arrange(year, week, zone, spp)
  
  #VI
  vi = calc_vi(abund, pir,
          by = grp_vars) %>%
   wnv_s_clean(rm_col = c("trap_status"), silence = T)
  
  
  return(vi)
  
}
