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
  
  # For each positive observation, find the best negative match
  for (i in 1:nrow(positives)) {
    pos <- positives[i, ]
    
    # Filter negatives by matching criteria
    candidate_negatives <- negatives %>%
      filter(
        year == pos$year,
        week == pos$week,
        zone == pos$zone,
        trap_id == pos$trap_id,
        spp == pos$spp
      )
    
    # If no exact matches, relax some criteria (zone first, then trap_id)
    if (nrow(candidate_negatives) == 0) {
      candidate_negatives <- negatives %>%
        filter(
          year == pos$year,
          week == pos$week,
          spp == pos$spp,
          zone == pos$zone
        )
    }
    
    if (nrow(candidate_negatives) == 0) {
      candidate_negatives <- negatives %>%
        filter(
          year == pos$year,
          week == pos$week,
          spp == pos$spp,
          trap_id == pos$trap_id
        )
    }
    
    if (nrow(candidate_negatives) == 0) {
      candidate_negatives <- negatives %>%
        filter(
          year == pos$year,
          week == pos$week,
          spp == pos$spp
        )
    }
    
    # If still no matches, try same species and same week in any year
    if (nrow(candidate_negatives) == 0) {
      candidate_negatives <- negatives %>%
        filter(
          week == pos$week,
          spp == pos$spp
        )
    }
    
    # Select the first match if any exist
    if (nrow(candidate_negatives) > 0) {
      best_match <- candidate_negatives[1, ]
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
        match_level = ifelse(
          pos$year == best_match$year & 
            pos$week == best_match$week & 
            pos$zone == best_match$zone & 
            pos$trap_id == best_match$trap_id & 
            pos$spp == best_match$spp,
          "same_trap_&_spp",
          ifelse(
            pos$year == best_match$year & 
              pos$week == best_match$week & 
              pos$zone == best_match$zone & 
              pos$spp == best_match$spp,
            "same_zone",
            ifelse(
              pos$year == best_match$year & 
                pos$week == best_match$week & 
                pos$trap_id == best_match$trap_id & 
                pos$spp == best_match$spp,
              "same_trap",
              ifelse(
                pos$year == best_match$year & 
                  pos$week == best_match$week & 
                  pos$spp == best_match$spp,
                "same_week_year_spp",
                "same_week_spp"
              )
            )
          )
        )
      )
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
head(matched_pairs)