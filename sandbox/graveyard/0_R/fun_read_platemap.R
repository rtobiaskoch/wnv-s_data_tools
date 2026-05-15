read_platemap <- function(fn_path,
                          sheet = "pcr", #sheet name in excel file that contains your platemap
                          range = "A1:M9", #range in the sheet that contains your platemap
                          col_pattern = "(?<=x)\\d+", #the pattern in the column name that contains the column number
                          val_to = "csu_id" #column name you want to identify your sample with
                          ) {
  # Load necessary libraries
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(janitor)
  library(purrr)
  library(stringr)
  
  if(any(tools::file_ext(fn_path) != "xlsx")) {
    stop("Warning your platemap file is not an excel.")
  }
  
  # Read and reshape platemap data from each file
  platemap <- fn_path %>%
    purrr::map(~ readxl::read_excel(.x,
                     col_names = TRUE,
                     col_types = "text",
                     range = range,
                     sheet = sheet) %>%
          janitor::clean_names() %>%
          dplyr::rename(row = "x1") %>%
          tidyr::pivot_longer(cols = -row,
                       names_to = "column",
                       values_to = val_to) %>%
          dplyr::mutate(file_name = .x,
                 column = str_extract(column, col_pattern))
    ) %>%
    dplyr::bind_rows()
  
  return(platemap)
}
