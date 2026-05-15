
clean_pcr = function(df, 
                     y_pattern = "(?<=y)\\d+", 
                     w_pattern = "(?<=w)\\d+", 
                     p_pattern = "(?<=p)\\d+", 
                     undet_val = '55.55') {
  df = df %>%
  clean_names() %>%
  mutate(
    year = str_extract(file_name, y_pattern),# Extract year as the part after "y" and before "_w"
    week = str_extract(file_name, w_pattern), # Extract week as the part after "_w" and before "_p"
    plate = str_extract(file_name, p_pattern),     # Extract plate as the part after "_p" (end of the string)
    plate = if_else(is.na(plate), plate, paste0("plate_", plate)) # make na if na so it will throw an error if it isn't working
  )    %>%
  mutate(ct = if_else(str_detect(ct, "Undetermined"), undet_val, ct)) %>% # convert "undetermined to numeric 55.55 to avoid errors
  mutate(cq = round(as.numeric(ct), 2)) %>%
  mutate(well = as.numeric(well)) %>%#convert to numeric to sort
  mutate(quantity = as.numeric(quantity), #should produce NA's
         quantity = replace_na(quantity, 0),
         copies = if_else(cq >= 55.55, 0, round(as.numeric(quantity),2))
         #ct_threshold = as.numeric(ct_threshold)
  ) %>%
  mutate(test_code = if_else(copies > copy_threshold & amp_status == "Amp", 1, 0)) %>%
  arrange(plate, well) %>%
  # select(well_position, task, target_name, cq, ct_threshold, copies, plate) %>%
  select(well_position, target_name, cq, copies, test_code, ct_threshold, amp_status,  year, week , plate)# %>%
  # pivot_wider(names_from = target_name, 
  #             values_from = c(copies,cq)) %>%
  # rename(cq = cq_WNV)
  
  
  return(df)
  
}