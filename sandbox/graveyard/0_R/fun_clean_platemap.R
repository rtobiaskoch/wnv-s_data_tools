clean_platemap <- function(df,
                           y_pattern = "(?<=y)\\d+",
                           w_pattern = "(?<=w)\\d+",
                           p_pattern = "(?<=p)\\d+") {
  # Load libraries
  library(dplyr)
  library(stringr)
  
  df_cleaned <- df %>%
    mutate(
      csu_id = str_remove(csu_id, "-"),
      year = str_extract(file_name, y_pattern),
      week = str_extract(file_name, w_pattern),
      plate = str_extract(file_name, p_pattern),
      plate = if_else(is.na(plate), plate, paste0("plate_", plate)),
      well_position = paste0(row, column),
      well_position = str_remove(well_position, "\\.0")
    ) %>%
    #get sample_type
    mutate(sample_type = case_when(
      grepl("^CSU|^BOU|^CDC", csu_id, ignore.case = T) ~ "mozzy",
      grepl("neg|negative", csu_id, ignore.case = T) ~ "neg ctrl",
      grepl("pos|positive", csu_id, ignore.case = T) ~ "pos ctrl",
      grepl("1e2", csu_id, ignore.case = T) ~ "std 1e2",
      grepl("1e4", csu_id, ignore.case = T) ~ "std 1e4",
      grepl("1e6", csu_id, ignore.case = T) ~ "std 1e6",
      grepl("RMRP", csu_id, ignore.case = T) ~ "bird",
      T ~ "undefined" 
                                    )
    ) %>%
    filter(!is.na(csu_id)) %>%
    select(well_position, csu_id, sample_type, year, week, plate)
  
  return(df_cleaned)
}
