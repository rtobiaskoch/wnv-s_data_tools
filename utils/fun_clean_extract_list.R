library(stringr)
library(dplyr)

extract_list = function(string, list) {
  
  # Create regex pattern like "NE|NW|SE|SW|LV|BE|BC"
  pattern <- str_c(list, collapse = "|")
  str_extract(string, pattern)
  
}
