library(dplyr)

# Read the data
wnv_data <- read.csv("1_input/wnv-s_database.csv")

# Function to find the best negative match for each positive observation
match_negatives <- function(data, yr) {
  
  data = data %>% filter(year == yr)
  # Separate positive and negative observations
  positives <- data %>% filter(test_code == 1)
  negatives <- data %>% filter(test_code == 0)
  
  # Initialize an empty list to store matches
  matches <- list()
  
  # Create a vector to track used negative IDs
  used_negatives <- c()
  
  # For each positive observation, find the best negative match
  for (i in 1:nrow(positives)) {
    pos <- positives[i, ]
    
    # Initialize best match and match level
    best_match <- NULL
    match_level <- "no_match"
    
    # Try to find matches in priority order
    # 1. Exact match on all criteria
    candidate_negatives <- negatives %>%
      filter(
        year == pos$year,
        week == pos$week,
        zone == pos$zone,
        trap_id == pos$trap_id,
        spp == pos$spp,
        !(csu_id %in% used_negatives)
      )
    
    if (nrow(candidate_negatives) > 0) {
      best_match <- candidate_negatives[1, ]
      match_level <- "exact_match"
    }
    
    # 2. Match year, week, zone, spp (relax trap_id)
    if (is.null(best_match)) {
      candidate_negatives <- negatives %>%
        filter(
          year == pos$year,
          week == pos$week,
          zone == pos$zone,
          spp == pos$spp,
          !(csu_id %in% used_negatives)
        )
      
      if (nrow(candidate_negatives) > 0) {
        best_match <- candidate_negatives[1, ]
        match_level <- "same_year_week_zone_spp"
      }
    }
    
    # 3. Match year, week, trap_id, spp (relax zone)
    if (is.null(best_match)) {
      candidate_negatives <- negatives %>%
        filter(
          year == pos$year,
          week == pos$week,
          trap_id == pos$trap_id,
          spp == pos$spp,
          !(csu_id %in% used_negatives)
        )
      
      if (nrow(candidate_negatives) > 0) {
        best_match <- candidate_negatives[1, ]
        match_level <- "same_year_week_trap_spp"
      }
    }
    
    # 4. Match year, week, spp (relax zone and trap_id)
    if (is.null(best_match)) {
      candidate_negatives <- negatives %>%
        filter(
          year == pos$year,
          week == pos$week,
          spp == pos$spp,
          !(csu_id %in% used_negatives)
        )
      
      if (nrow(candidate_negatives) > 0) {
        best_match <- candidate_negatives[1, ]
        match_level <- "same_year_week_spp"
      }
    }
    
    # 5. Match week, spp (relax year, zone, and trap_id)
    if (is.null(best_match)) {
      candidate_negatives <- negatives %>%
        filter(
          week == pos$week,
          spp == pos$spp,
          !(csu_id %in% used_negatives)
        )
      
      if (nrow(candidate_negatives) > 0) {
        best_match <- candidate_negatives[1, ]
        match_level <- "same_week_spp"
      }
    }
    
    # If we found a match
    if (!is.null(best_match)) {
      matches[[i]] <- data.frame(
        positive_id = pos$csu_id,
        negative_id = best_match$csu_id,
        year = pos$year,
        week = pos$week,
        zone = pos$zone,
        trap_id = pos$trap_id,
        spp = pos$spp,
        positive_copies = pos$copies,
        negative_copies = best_match$copies,
        match_level = match_level
      )
      
      # Add the matched negative to the used_negatives list
      used_negatives <- c(used_negatives, best_match$csu_id)
    } else {
      matches[[i]] <- data.frame(
        positive_id = pos$csu_id,
        negative_id = NA,
        year = pos$year,
        week = pos$week,
        zone = pos$zone,
        trap_id = pos$trap_id,
        spp = pos$spp,
        positive_copies = pos$copies,
        negative_copies = NA,
        match_level = "no_match"
      )
    }
  }
  
  # Combine all matches into a single data frame
  matched_pairs <- bind_rows(matches)
  return(matched_pairs)
}

# Find matches
matched_pairs2 <- match_negatives(wnv_data, 2024)

# View the results
head(matched_pairs2)