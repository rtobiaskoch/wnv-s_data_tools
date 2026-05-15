
#get gsheet_pull off github
#source(https://raw.githubusercontent.com/rtobiaskoch/TK_useful_functions/refs/heads/main/gsheet_pull.R)

rm(list = ls())

list2env(readRDS("1_input/config_params.RDS"),          
         envir = .GlobalEnv)

pacman::p_load(tidyverse)

#check files
if(!file.exists(fn_trap)) {
  stop(paste("Input file", fn_trap, "doesn't exist. Please download or use gsheet_pull to add file to input."))
}

foco_traps0 = read.csv(fn_trap)

foco_traps = foco_traps0 %>%
 # filter(active == 1) %>%
  unite("temp", trap_id, zone, method, sep = "|") #unite to avoid erroneous expansion of all zones for each trap_id


if(!file.exists(fn_database_input)) {
  stop(paste("Input file", fn_database_input, "doesn't exist. Please download or use gsheet_pull to add file to input."))
}


#remove "-" from CSU ID if it exists
data_input = read.csv(fn_database_input) %>%
  mutate(csu_id = str_remove(csu_id, "-")) %>% 
  select(-any_of(c("lat", "long"))) #removes lat and long if they exist so that it can be replaced from foco_trap data

year = min(data_input$year):max(data_input$year)
week = min(data_input$week):max(data_input$week)
spp = unique(data_input$spp)
trap = foco_traps$temp

#generate all possible combinations of traps across the data set
trap_expand = tidyr::expand_grid(year, 
                          week,
                          trap,
                          spp)  %>%
  separate_wider_delim(trap, delim = "|", names = c("trap_id", "zone", "method")) %>%
  #remove new BE traps which all start with BE before 2024 
  filter(!(grepl("^BE", trap_id) & year < 2024))
  
#make sure that the mean number of traps is a whole number indicating that they aren't changing year to year
check_expand = trap_expand %>%
  group_by(year, week, zone) %>% 
  distinct(trap_id) %>% 
  count() %>% 
  group_by(zone) %>% 
  summarize(mean = mean(n), sd = sd(n))

check_trap_consistency <- function(df) {
  # Check if any mean values are not whole numbers
  if (any(df$mean %% 1 != 0) & any(df$sd == 0)) { # %% is the modulo operator (remainder after division)
    warning("Warning: The number of traps in your dataset week to week per zone is not consistent (non-integer mean).")
    return(invisible(FALSE)) # Return FALSE invisibly to signal the warning
  }
  
  # If no issues are found
  return(invisible(TRUE))
}

# Example usage:
check_trap_consistency(check_expand %>% filter(zone != 'BE'))

#get zones with no traps by week and plot
data_input_expand_no_traps = trap_expand %>%
  left_join(data_input, by = c("year", "week", "zone", "trap_id", "method", "spp")) %>%
  #remove zones with no data for entire week for a zone
  group_by(year, week, zone) %>% 
  filter(all(is.na(csu_id))) %>%
  distinct(year, week, zone)

# ggplot(data_input_expand_no_traps, aes(x = week, fill = zone)) + 
#   geom_bar() +
#   facet_wrap(~year) +
#   theme_classic() +
#   ggtitle("ZONES WITH NO TRAPS BY WEEK")
# ggsave("data_output/data_input_expand_no_traps.png")


data_input_expand = trap_expand %>%
  left_join(data_input, by = c("year", "week", "zone", "trap_id", "method", "spp")) %>%
  select(-any_of(c("lat", "long"))) %>%
  #remove zones with no data for entire week for a zone
  group_by(year, week, zone) %>% 
  filter(!all(is.na(csu_id))) %>% #remove weeks where there was no trapping in the particular zones
  ungroup %>%
  #fill in the missing trap date for traps with one spp but not another
  arrange(year, week, zone, trap_id) %>%
  group_by(year, week, zone, trap_id) %>% #fixed bug previously was group_by(year, week, zone)
  fill(trap_date, .direction = "downup") %>% 
  fill(county, .direction = "downup") %>%
  ungroup %>%
  #fill in the likely missing trap date for traps with no spp for that week
  arrange(year, week, zone) %>%
  group_by(year, week, zone) %>% #remove trap_id
  fill(trap_date, .direction = "downup") %>% 
  fill(county, .direction = "downup") %>%
  ungroup %>%
  #add in csu_id and total for 0 traps
  mutate(csu_id = if_else(is.na(csu_id), "CSU00000", csu_id)) %>%
  mutate(total = if_else(is.na(total), 0, total)) %>%
  #add lat and long back in
  left_join(foco_traps0 %>% select(trap_id, lat, long),
            by = "trap_id")  %>%
  mutate(csu_id = str_remove(csu_id, "-")) %>%
  distinct_all() %>%
  select(colnames(data_input))

#see if process missed any samples from original data
missing_id = anti_join(data_input, data_input_expand, by = "csu_id")


#check to see if any traps have more than one date for the same trap_id for any particular week
check_trap_date = data_input_expand %>%
  distinct(year, week, zone, trap_date, trap_id) %>%
  group_by(year, week, zone, trap_id) %>%
  count() %>%
  filter(n != 1) %>%
  left_join(data_input %>% select(year, week, zone, trap_id, csu_id), 
            by = c("year", "week", "zone", "trap_id")) %>%
  #factor mostly for filtering purposes
   mutate(year = factor(year),
          week = factor(week),
          zone = factor(zone))

source("0_R/fix_trap_date.R")
data_input_expand_update = fix_trap_dates(data_input_expand)

#check to see if any traps have more than one date for the same trap_id for any particular week
check_trap_date = temp %>%
  distinct(year, week, zone, trap_date, trap_id) %>%
  group_by(year, week, zone, trap_id) %>%
  count() %>%
  filter(n != 1) %>%
  left_join(data_input %>% select(year, week, zone, trap_id, csu_id), 
            by = c("year", "week", "zone", "trap_id")) %>%
  #factor mostly for filtering purposes
  mutate(year = factor(year),
         week = factor(week),
         zone = factor(zone))


changed_csu_ids <- get_changed_dates(data_input_expand, data_input_expand_update)

#check to make sure there are the same number of traps for every week for each zone
#sd should be 0 (except for BE which added traps)
check = data_input_expand_update %>%
  group_by(year, week, zone) %>% 
  distinct(trap_id) %>% 
  count() %>% 
  group_by(zone) %>% 
  summarize(mean = mean(n), sd = sd(n))

write.csv(data_input_expand_update, "data_output/wnv-s_database_0_expand.csv", row.names = F)

googlesheets4::sheet_write(data_input_expand_update,
                           ss = database_gsheet_key,
                           sheet = "data"
)
