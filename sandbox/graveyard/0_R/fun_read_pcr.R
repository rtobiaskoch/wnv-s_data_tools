

read_pcr = function(path, sheet) {
  library(purrr)
  library(dplyr)
  
  path %>% 
    map(~ {
      data = read_excel(.x, col_names = TRUE, sheet = sheet) %>% #iteratively read through all file names in path
        filter(`Block Type` %in% c("Well", as.character(1:96))) #keep only the 96 wells as the output as superfluous BS
      
      colnames(data) = data[1,] %>% as.character() #replace the nonsensical colnameswith the actual colnames that are location in the first row
      data %>% 
        filter(`Well Position` != "Well Position") %>% #removes the colnames from the first row
        mutate(file_name = .x) #add a column that is the file name where the observation came from
    }) %>%
    bind_rows() #put everything together
}