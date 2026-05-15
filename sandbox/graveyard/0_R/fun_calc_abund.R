#DESCRIPTION
#this abundance calculation uses the culex_sheet as its input instead of the pooled data from 
#the datasheet/wnv-s_database


calc_abund = function(df,
                     grp_var = c("zone", "year", "week", "spp"),
                     spp_keep = c("Tarsalis", "Pipiens"),
                     rm_zone = NULL) {
  
  #df input note
  #should be the cleaned all species datasheet
  
  #check inputs
  #-------------------------------------------------------------------------------
  
  if(any(!grp_var %in% colnames(df))) {
    stop("one or more of the grouping variables (grp_var) do not exist in the data")
  }

  
  req_var = c("trap_id","year","week","zone", "zone2", "trap_status", 
              "method","spp","total")
  
  if(any(!req_var %in% colnames(df))) {
    
    missing = setdiff(req_var, names(df))
    stop(cat("The required variables are not in your data.",
             paste0(missing, collapse = ",")))
  }
  
  # Create grouping variables
  #-------------------------------------------------------------------------------=
  grp_sym <- rlang::syms(grp_var)
  
   #get totals by variables you want to group by
  abund = df %>%
    filter(method == "L") %>%
    filter(trap_status != "malfunction" ) %>%
    filter(spp %in% spp_keep) %>%
    group_by(!!!grp_sym) %>%
    summarize(
      trap_L = if (all(is.na(trap_id))) 0 else n_distinct(trap_id, na.rm = TRUE), #if all na make 0 from filling in missing zones else count distinct
      mosq_L = sum(total, na.rm = TRUE),
      abund_sd = round(sd(total),4),
      .groups = 'drop'
     ) %>%
    mutate(abund = round(mosq_L/trap_L, 4),
           abund_lci = round(abund - (1.96*(abund_sd/trap_L^0.5)),4),
           abund_uci = round(abund + (1.96*(abund_sd/trap_L^0.5)),4)
    ) %>%
    mutate(abund_lci = if_else(abund_lci < 0, 0, abund_lci))
  
  #rename to zone if zone2 exists
  if("zone2" %in% names(abund)) {
    abund =   abund %>%
      rename(zone = zone2)
  }
  
  #add spp if it wasn't in df it means its all spp
  if(!"spp" %in% names(abund)) {
    abund = abund %>%
      mutate(spp = "All")
  }
  
  return(abund)
}  