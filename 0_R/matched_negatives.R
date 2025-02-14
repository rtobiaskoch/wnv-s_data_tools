#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# USER INPUT
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rm(list = ls())
fn = "1_input/wnv-s_database.csv"

df = read.csv(fn)
yr = 2024


matched_negatives = function(df, yr) {
  # Filter positive and negative pools
  
  df = df %>% filter(year == yr)
  
  # Filter positive and negative pools
  positives <- df %>% 
    filter(test_code == 1) %>%
    group_by(year, week, zone) %>%
    arrange(trap_id, spp) %>%
    mutate(row_id = row_number()) %>%
    ungroup()
  
  
  negatives <- df %>% 
    filter(test_code == 0) %>%
    group_by(year, week, zone) %>%
    arrange(trap_id, spp, abs(total - first(total))) %>%
    mutate(row_id = row_number()) %>%
    ungroup()


  # Ensure equal number of negatives per (year, week, zone)
  matched_negatives <-  negatives %>%
    semi_join(positives, by = c("year", "week", "zone", "row_id")) %>%
    select(-row_id) 
  
  # Interleave positives and matched negatives for readability
  bind_rows(matched_negatives, positives) %>%
    arrange(year, week, zone, trap_id, spp, desc(test_code)) %>%
    select(-row_id)
}

matched_pairs = matched_negatives(df, yr)

# Save results
write_csv(matched_pairs, "matched_negative_pools.csv")

 