library(dplyr)
library(ggplot2)
library(plotly)
library(stringr)
library(rlang)

plot_std = function(df) {
  
  req_cols = c("sample_type", "year", "week", "plate","target", "log_copies")
  
  missing_cols <- setdiff(req_cols, names(df))
  
  if (length(missing_cols) > 0) {
    cat("\n Notice. Following columns are required for this function: ", paste(missing_cols, collapse = ", "), "\n")
    cat("Check input data.")
  }
  
  p_std = df %>% 
    mutate(sample_type = if_else(str_detect(sample_type, "std"), "std", "pos_ctrl")) %>% 
    mutate(grp = paste(year, week, plate, target, sample_type, sep = "-")) %>%
    filter(str_to_lower(target) == str_extract(csu_id, "^[^_]*")) %>% #match the virus in the standard name to the target
    mutate(cq = if_else(cq == 55.55, 40, cq)) %>%
    ggplot(aes(x = log_copies, y = cq, color = week, group = grp)) +
    geom_point(alpha = 0.4, size = 3) +
    geom_line() + 
    scale_y_reverse() +
    facet_wrap(~target) +
    ggtitle("Standards by Week") +
    theme_classic()
  
  return(p_std)

}



plot_pcr <- function(data, virus, pattern_2_keep = "WNV|CSU|RMRP|CDC|pos|neg",
                     copy_threshold, week_filter) {
  
  #cq_col <- sym(paste0("cq_", virus))
  #copies_col <- sym(paste0("copies_", virus))
  
  data_filtered <- data %>%
    filter(target_name == virus) %>%
    filter(str_detect(csu_id, regex(pattern_2_keep, ignore_case = TRUE))) %>%
    mutate(
      cq = if_else(cq == 55.55, 40, cq),
      test_code = if_else(amp_status == "No Amp", 0, test_code),
      test_code = factor(test_code, levels = c("1", "0"))
    )
  
  
  # Plot
  p <- ggplot(data_filtered) +
    geom_jitter(aes(x = sample_type, y = cq, color = test_code), size = 3, alpha = 0.6) +
    scale_y_reverse() +
    theme_minimal() +
    ggtitle(paste0("Week ", week_filter, " ", virus)) +
    theme(legend.position = "none")
  
  return(p)
}

