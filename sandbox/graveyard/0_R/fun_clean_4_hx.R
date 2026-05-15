#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-----------------------H X :   V I / A L L ---------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

clean_wide = function(df, prefix) {
  df = df %>%
    select(year, week, zone, spp, abund, pir, vi) %>%
    group_by(zone, week, spp) %>%
    summarise(abund = mean(abund, na.rm = T), 
              pir = mean(pir, na.rm = T), 
              vi = mean(vi, na.rm = T),
              .groups = "drop") %>%
    filter(spp == "All") %>%
    select(-spp) %>%
    pivot_wider(names_from = zone, 
                values_from = c(abund, pir, vi),
                names_prefix = prefix)
  return(df)
}


create_hx_report <- function(df, zones, prefix, sigfig = 2) {
  # Create interleaved column names (current then historical for each zone)
  col_pairs <- purrr::map(zones, ~ c(paste0(prefix, "_", .x), 
                                     paste0(prefix, "_hx_", .x))) %>%
    purrr::flatten_chr()
  
  col_order <- c("week", col_pairs)
  
  # Ensure columns exist in the data
  existing_cols <- intersect(col_order, names(data))
  existing_cols = col_order
  
  df %>%
    arrange(week) %>%
    select(all_of(existing_cols)) %>%
    mutate(across(where(is.numeric), ~ round(.x, sigfig))) %>%
    mutate(across(.cols = -week, ~ ifelse(is.na(.), "", .)))
}

