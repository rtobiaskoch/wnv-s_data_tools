#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# MATCH NEGATIVE POOLS FUNCTION
# Matches WNV-negative mosquito pools to positive detections
# for comparative analysis
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

matched_negatives = function(df, yr) {
  # Filter data to only include specified year
  df = df %>% filter(year == yr)
  
  #--------------------------------------------------
  # PROCESS POSITIVE POOLS (WNV detections)
  #--------------------------------------------------
  positives <- df %>% 
    # Select only positive detections (test_code == 1)
    filter(test_code == 1) %>%
    # Group by spatial-temporal characteristics
    group_by(year, week, zone) %>%
    # Arrange by trap and species for consistent ordering
    arrange(trap_id, spp) %>%
    # Create row numbers within each group for matching
    mutate(row_id = row_number()) %>%
    ungroup()
  
  #--------------------------------------------------
  # PROCESS NEGATIVE POOLS (non-detections)
  #--------------------------------------------------
  negatives <- df %>% 
    # Select only negative samples (test_code == 0)
    filter(test_code == 0) %>%
    # Group using same spatial-temporal groups as positives
    group_by(year, week, zone) %>%
    # Arrange by:
    # 1. Trap ID and species to match positive ordering
    # 2. Absolute difference in total mosquitoes to find similar sample sizes
    arrange(trap_id, spp, abs(total - first(total))) %>%
    # Create matching row numbers
    mutate(row_id = row_number()) %>%
    ungroup()
  
  #--------------------------------------------------
  # MATCH NEGATIVES TO POSITIVES
  #--------------------------------------------------
  # Join negatives that share year/week/zone/row_id with positives
  matched_negatives <- negatives %>%
    semi_join(positives, by = c("year", "week", "zone", "row_id")) %>%
    left_join()
    # Remove the temporary row_id column
    select(-row_id) 
  
  #--------------------------------------------------
  # COMBINE AND FORMAT RESULTS
  #--------------------------------------------------
  # Combine positive and matched negative pools
  final_output <- bind_rows(matched_negatives, positives) %>%
    # Order results by:
    # 1. Temporal factors (year, week)
    # 2. Spatial factors (zone, trap_id)
    # 3. Biological factors (species)
    # 4. Put positives first in each group (desc(test_code))
    arrange(year, week, zone, trap_id, spp, desc(test_code)) %>%
    # Remove the temporary row_id column
    select(-row_id)
  
  
  # Check if any negatives were matched
  if (nrow(filter(final_output, test_code == 1)) != nrow(filter(final_output, test_code == 1))) {
    message("Warning: Some positives pools do not have matched negatives")
  }
  
  return(final_output)
}
 