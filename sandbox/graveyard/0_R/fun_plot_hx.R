

clean_long_hx_wk = function(ytd, hx,
                         rm_zone = "BC", 
                         comb_zone = T, 
                         grp_vars = c("year", "week", "zone", "spp")) {
  
  curr_hx_df = bind_rows(ytd, hx) %>%
    filter(!zone %in% rm_zone) %>%
    select(-any_of(c("mosq_L", "trap_L", "zone2"))) %>%
    pivot_longer(cols = -c(grp_vars, type),
                 names_to = "est",
                 values_to = "value") %>%
    mutate(type = factor(type, levels = c("hx", "current")),
           zone = factor(zone, levels = zone_lvls)) %>%
    pivot_wider(names_from = est, values_from = value) %>%
    group_by(zone, week, spp, type) %>%
    summarise(abund = mean(abund, na.rm = T), 
              pir = mean(pir, na.rm = T), 
              vi = mean(vi, na.rm = T)) %>%
    filter(spp == "All")
    
    return(curr_hx_df)
}
    
plot_hx = function(df, value, text, pallette = c("current" = "#e9724c",
                                                "hx"      = "grey50") ) {
  
    p = ggplot(df, aes(x = week, y = {{value}}, 
                   color = type, fill = type, group = type)) +
      geom_hline(yintercept = 0) +
      geom_area(position = "dodge", alpha = 0.3) +
      facet_grid(zone ~ .) +
      theme_classic() +
      ggtitle(text) +
      scale_color_manual(values = pallette) +
      scale_fill_manual(values = pallette)
 return(p)
}   


clean_long_hx = function(ytd, hx,
                         rm_zone = "BC", 
                         grp_var = c("year", "week", "zone", "spp", "type"),
                         est_keep = c("abund", "vi", "pir"),
                         spp_keep = c("All")) {
  
  
  curr_hx_df = bind_rows(ytd, hx) %>%
    filter(!zone %in% rm_zone) %>%
    filter(spp %in% spp_keep) %>%
    select(any_of(c(grp_var, est_keep))) %>%
    pivot_longer(cols = all_of(est_keep),
                 names_to = "est",
                 values_to = "value") %>%
    filter(est %in% est_keep) #keep only the estimates that you want to plot
}



plot_hx_line <- function(df, text, color_var = year, 
                         current_color = "#e9724c",
                         hx_start_grey = "grey40",
                         hx_end_grey = "grey80") {
  
  #takes following data structure:
  # structure(list(year = c(2019, 2019, 2019, 2019, 2019, 2019), 
  #                week = c(23, 23, 23, 23, 23, 23),
  #                zone = structure(c(1L,1L, 1L, 2L, 2L, 2L), levels = c("NW", "NE", "SE", "SW", "FC", "LV", "BE", "BC"), class = "factor"), 
  #                spp = structure(c(3L,3L, 3L, 3L, 3L, 3L), levels = c("Pipiens", "Tarsalis", "All","other spp", "none"), class = "factor"), 
  #                type = c("hx", "hx", "hx", "hx", "hx", "current"), 
  #                est = c("abund", "vi", "pir", "abund", "vi", "pir"), value = c(NA, NA, 0, 3.2, 0, 0)), 
  #                row.names = c(NA, -6L), class = c("tbl_df", "tbl", "data.frame"))
  
  # Convert color_var to symbol for tidy evaluation
  color_var <- enquo(color_var)
  
  # Ensure color column is character if numeric
  # if (is.numeric(df[[rlang::as_name(color_var)]])) {
  #   df <- df %>% mutate(!!color_var := as.character(!!color_var))
  # }
  
  # Create base plot
  p <- ggplot(df, aes(x = week, y = value, 
                      group = interaction(!!color_var, type))) +
    geom_hline(yintercept = 0) +
    facet_grid(est ~ zone, scales = "free_y") +
    theme_classic() +
    ggtitle(text)
  
  # Add historical data as grey gradient lines
  if ("type" %in% names(df) && any(df$type == "hx")) {
    p <- p + geom_line(
      data = ~ filter(.x, type == "hx"),
      aes(color = !!color_var),
      alpha = 0.5,
      linewidth = 0.5
    ) +
      scale_color_gradient(
        low = hx_start_grey,
        high = hx_end_grey,
        guide = guide_colorbar(title = "Year")
      )
  }
  
  # Add current data as colored area
  if ("type" %in% names(df) && any(df$type == "current")) {
    p <- p + geom_area(
      data = ~ filter(.x, type == "current"),
      aes(fill = "Current"),
      alpha = 0.8,
      position = "identity"
    ) +
      scale_fill_manual(
        values = c("Current" = current_color),
        guide = guide_legend(title = NULL)
      )
  }
  
  # Default if no type column exists
  if (!"type" %in% names(df)) {
    p <- p + geom_line(aes(color = !!color_var))
  }
  
  p = p + 
    theme(legend.position = "bottom")
  return(p)
}



  
  


