
merge_4_database = function(datasheet, cq, 
                            cols = c("csu_id", "trap_id", 
                                     "zone", "year", 
                                     "week", "trap_date",
                                     "method", "spp",
                                     "total", "test_code",
                                     "copies", "cq",
                                     "seq", "notes"),
                            virus,
                            copy_threshold = 500) {
  virus <- toupper(virus)
  
  #keep only pcr data for desired target
  cq = cq %>% filter(target_name == virus)
  
  merge = datasheet %>%   
  select(-c(test_code)) %>% # remove so that it will be pulled from the 
  full_join(cq, by = c("csu_id", "year", "week")) %>%
  mutate(cq = if_else(is.na(cq), 55.55, cq)) %>%
  mutate(copies = if_else(is.na(copies), 0, copies)) %>%
  mutate(seq = 0) %>%
  mutate(notes = if_else(copies > copy_threshold & amp_status != "Amp", amp_status, "")) %>% #add note of amp status if the copy number was higher
  filter(!is.na(trap_id)) %>%
  select(all_of(cols))
  
  return(merge)
}
 