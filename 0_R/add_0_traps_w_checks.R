
#get gsheet_pull off github
#source(https://raw.githubusercontent.com/rtobiaskoch/TK_useful_functions/refs/heads/main/gsheet_pull.R)

#INPUTS
#fn_trap
#fn_database_input
#foco_trap.csv
#wnv-s_database

pacman::p_load(tidyverse, argparse)

rm(list = ls())

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------------------------- U S E R  I N P U T ----------------------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Use config file for input filenames
#if no config file you can use arguments 
#if no arguments change the inputs below and they will be used in the script
fn_trap_hardcode = "1_input/foco_trap.csv"
fn_database_input_hardcode = "1_input/wnv-s_database.csv"
fn_config_file = "1_input/config_params.RDS" # if you dont have a config file you can make it NA
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------------- D E F I N E   P A R A M E T E R S ---------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#else define the necessary inputs yourself
if(file.exists(fn_config_file)){
  list2env(readRDS(fn_config_file),          
           envir = .GlobalEnv)
  cat("input parameters defined by 1_input/config_params.RDS")
} else { #if no config file use arguments, if no arguments use default
  

      # Create an argument parser
      parser <- ArgumentParser(description = "Script to handle trap and database input files")
      
      # Add arguments for fn_trap and fn_database_input
      parser$add_argument("--fn_trap", help = "File path for the trap input", type = "character")
      parser$add_argument("--fn_database_input", help = "File path for the database input", type = "character")
      
      # Parse arguments
      args <- parser$parse_args()
      
      # Assign default values if arguments are not provided
      fn_trap <- if (!is.null(args$fn_trap)) args$fn_trap else fn_trap_hardcode
      fn_database_input <- if (!is.null(args$fn_database_input)) args$fn_database_input else fn_database_input_hardcode
      
      # Output the values
      cat("Trap file:", fn_trap, "\n")
      cat("Database file:", fn_database_input, "\n")

}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------------------------- C H E C K   F I L E S----------------------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(!file.exists(fn_trap)) {
  stop(paste("Input file", fn_trap, "doesn't exist. Please download or use gsheet_pull to add file to input."))
}

foco_traps0 = read.csv(fn_trap)

foco_traps = foco_traps0 %>%
  # filter(active == 1) %>%
  unite("temp", trap_id, zone, method, sep = "|") #unite to avoid erroneous expansion of all zones for each trap_id

#for removing traps combinations that occur before they were first implemented
foco_trap_filter = foco_traps0 %>% 
  select(trap_id, active, start)

if(!file.exists(fn_database_input)) {
  stop(paste("Input file", fn_database_input, "doesn't exist. Please download or use gsheet_pull to add file to input."))
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------- C R E A T E   E X P A N D E D   D A T A S E T--------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

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
  separate_wider_delim(trap, delim = "|", names = c("trap_id", "zone", "method")) 


#make sure that the mean number of traps is a whole number indicating that they aren't changing year to year
check_expand = function(df) {
  df %>%
    group_by(year, week, zone) %>% 
    distinct(trap_id) %>% 
    count() %>% 
    group_by(year, zone) %>% 
    summarize(mean = mean(n), sd = sd(n))
}

check_expand_df = check_expand(trap_expand)

check_trap_consistency <- function(df) {
  # Check if any mean values are not whole numbers
  if (any(df$mean %% 1 != 0) & any(df$sd == 0)) { # %% is the modulo operator (remainder after division)
    stop("Warning: The number of traps in your dataset week to week per zone is not consistent (non-integer mean).")
    return(invisible(FALSE)) # Return FALSE invisibly to signal the warning
  }
  
  # If no issues are found
  return(invisible(TRUE))
}

# Example usage:
check_trap_consistency(check_expand_df %>% filter(zone != 'BE'))

#get zones with no traps by week and plot
data_input_expand_no_traps = trap_expand %>%
  left_join(data_input, by = c("year", "week", "zone", "trap_id", "method", "spp")) %>%
  #remove zones with no data for entire week for a zone
  group_by(year, week, zone) %>% 
  filter(all(is.na(csu_id))) %>%
  distinct(year, week, zone)



data_input_expand = trap_expand %>%
  left_join(data_input, by = c("year", "week", "zone", "trap_id", "method", "spp")) %>%
  select(-any_of(c("lat", "long"))) %>%
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
  left_join(foco_traps0 %>% select(trap_id, active, start, lat, long),
            by = "trap_id")  %>%
  mutate(csu_id = str_remove(csu_id, "-")) %>% #42515 25-01-13
  #FILTER
  #remove traps with no data for entire week for a zone
  group_by(year, week, zone) %>% 
  filter(!all(csu_id == "CSU00000")) %>% #remove weeks where there was no trapping in the particular zones n = 31261 25-01-13
  ungroup %>%
  distinct_all() #n = 31254 25-01-13

#CREATE DATASET anti_join list
data_input_expand_all_spp = data_input_expand %>%
  group_by(year, start, week, zone, trap_id, active) %>%
  summarize(total = sum(total)) %>%
  ungroup

inactive = data_input_expand_all_spp %>%
  filter(total == 0 & (active == 0 | active == 1 & year < start))

#LOGIC 1: anti_join will remove if trap is empty AND inactive (0). 
         #This ensures one off traps or typos are not replicated throughout database

#LOGIC 2: anti_join will remove if trap is empty AND trap is active before its first known start year. 
#         This ensures traps that were made active in recent years are not replicated before its actual start date.


filter_check = data_input_expand_all_spp %>%
  anti_join(inactive, by = c("year", "week", "zone", "trap_id")) %>%
  mutate(date_check = year < start)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------- P L O T   E X P A N D E D   D A T A S E T--------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
p_active  = data_input_expand %>%
  anti_join(inactive, by = c("year", "week", "zone", "trap_id")) %>%
  filter(year > 2014) %>%
  filter(active == 0) %>%
  group_by(year, week, zone, active, trap_id) %>%
  summarize(total = sum(total)) %>%
  mutate(active = factor(active, levels = c(0,1))) %>%
  ungroup %>%
  ggplot(aes(x = week, y = total, color = trap_id)) +
  geom_point() +
  facet_grid(zone~year) +
  theme_minimal()
  
plotly::ggplotly(p_active)
  
data_input_expand %>%
    anti_join(inactive, by = c("year", "week", "zone", "trap_id")) %>%
    filter(year > 2014) %>%
    filter(zone %in% fc_zones) %>%
    group_by(year, week, zone, trap_id, spp) %>%
    summarize(total = sum(total)) %>%
    ungroup %>%
    ggplot(aes(x = week, y = total, color = spp)) +
    geom_point(alpha = 0.5) +
    scale_color_manual(values = mozzy_pal2) +
    scale_x_continuous(breaks = seq(min(data_input_expand$week), max(data_input_expand$week), by = 2)) +
    theme(axis.text.x = element_text(angle = 90)) +
    facet_grid(zone~year) +
    ylim(0,1500) +
    theme_classic()

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------------------------- C R E A T E   N E W D A T A ------------------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_input_expand2 = data_input_expand %>%
  anti_join(inactive, by = c("year", "week", "zone", "trap_id")) #%>% #n = 22434
select(colnames(data_input)) 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------------------------- C H E C K   N E W D A T A --------------------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#see if process missed any samples from original data
missing_id = anti_join(data_input, data_input_expand2, by = "csu_id")

if(nrow(missing_id) > 0) {
  warning("add_0_traps dropped samples. check code.")
}


check_expand_df2 = check_expand(data_input_expand2)

# Example usage:
check_trap_consistency(check_expand_df2 %>% filter(zone != 'BE'))

write.csv(data_input_expand2, "2_output/wnv-s_database_0_expand.csv", row.names = F)

