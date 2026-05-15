clean_standards = function(df, std_pattern = "std|pos ctrl") {
  
  #takes input data from the merging of the ct data from the pcr input 
  #and the platemap data that provides the id and associated metadata
  
  req_cols = c("sample_type", "ct_threshold", "well_position",
               "cq","copies",  "plate", "csu_id")
  
  missing_cols <- setdiff(req_cols, names(df))
  
  if (length(missing_cols) > 0) {
    cat("\n Notice. Following columns are required for this function: ", paste(missing_cols, collapse = ", "), "\n")
    cat("Check input data.")
  }
  
  df = df %>%
  filter(str_detect(pattern = std_pattern, sample_type)) %>%
  rename(target = target_name) %>%
  mutate(year = as.double(year)) %>%
  mutate(week = as.double(week)) %>%
  mutate(log_copies = if_else(copies == 0, 0, log10(copies))) %>%
  select(year, week, well_position, plate, target, csu_id, sample_type, cq, log_copies) %>%
  #select(-ct_threshold, well_position) %>%
  
  return(df)
  
}