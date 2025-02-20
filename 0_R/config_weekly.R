#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# H E A D E R
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#PURPOSE:
#1 checks and loads necessay package for pipeline
#2 defines input data directory
#3 checks if input directories exist
#4 checks if input files exist
#5 defines google sheet keys
#6 

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#L O A D   P A C K A G E S   F O R   P I P E L I N E 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rm(list = ls())
suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_unload()
  pacman::p_load(googlesheets4, googledrive, rio, readxl, openxlsx, googledrive, argparse, #importing and exporting
                 tidyverse, janitor, lubridate, rquery, #manipulation
                 PooledInfRate, #analysis
                 ggpubr, wesanderson, paletteer, leaflet, patchwork# plotting
                 )
  
  
})



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# D E F I N E   D I R E C T O R I E S   &   I N P U T   F I L E S 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# <<<<<<<------------------------------------------------------------------------------------------------ U S E R   I N P U T
dir_input <- "1_input"
dir_datasheet <- file.path(dir_input, "datasheet")
dir_pcr <- file.path(dir_input, "pcr")
dir_platemap <- file.path(dir_input, "platemap")
config_params_file = file.path(dir_input, "config_params.RDS")

#SCRIPTS
dir_scripts <- "0_R"

#OUTPUT
dir_mid <- "2_mid"
dir_output <- "3_output"


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# C H E C K   F O R   M I S S I N G   D I R E C T O R I E S 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Initialize a vector to store missing directories
# Create a named list of directories for clarity
directories <- list(
  input = dir_input, 
  datasheet = dir_datasheet, 
  pcr = dir_pcr, 
  platemap = dir_platemap, 
  scripts = dir_scripts, 
  mid = dir_mid, 
  output = dir_output
)

missing_dirs <- c()

# Check for missing directories and collect them
for (dir_name in names(directories)) {
  dir_path <- directories[[dir_name]]
  
  if (!dir.exists(dir_path)) {
    missing_dirs <- c(missing_dirs, dir_path)
  }
}

# If there are missing directories, stop the script and display them
if (length(missing_dirs) > 0) {
  stop(
    paste(
      "The following directories are missing:\n",
      paste(missing_dirs, collapse = "\n"),
      "\nPlease create them in your working directory or update the config R file."
    )
  )
} else {
  # If all directories exist, proceed with the script
  print("All required directories exist.")
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# -------- C H E C K   F O R   E M P T Y   I N P U T   S U B D I R  &   F I L E S
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Create a named list of directories to check
input_subdirs <- list(
  datasheet = dir_datasheet,
  pcr = dir_pcr,
  platemap = dir_platemap,
  scripts = dir_scripts
)

# Initialize a vector to store empty directories
empty_dirs <- c()

# Check each directory for emptiness
for (dir_name in names(input_subdirs)) {
  dir_path <- input_subdirs[[dir_name]]
  
  if (dir.exists(dir_path)) {
    # Check if the directory is empty
    if (length(list.files(dir_path)) == 0) {
      empty_dirs <- c(empty_dirs, dir_path)
    }
  } else {
    # If the directory doesn't exist, it's considered empty
    empty_dirs <- c(empty_dirs, dir_path)
  }
}

# If there are empty directories, stop the script and display them
if (length(empty_dirs) > 0) {
  stop(
    paste(
      "The following directories are empty or don't exist:",
      paste(empty_dirs, collapse = "\n"),
      "\nPlease populate them with the required files manually or with 0_check_load_input_data.R script."
    )
  )
} else {
  # If all directories contain files, proceed with the script
  print("All required directories are populated.")
}





#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ---------------- D E F I N E   G S H E E T   K E Y --------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
key_database_gsheet = "12Mf-w9I9NHTTDjzEPRoxUE08ka4WZ6RE-RM1s-FW7qA"
key_trap_gsheet = "1Jna3Bu47gjBWWz5vCoel4ksa-LBuo8R3zVfQYFl73wI"
key_trap_malfunction = "1dsTyvZoCN6NUJlTcDLINMfxuGZdJuP2ADpn8noQwL6Q"
key_standards_gsheet = "1bSMYQ4bZ9uBfrOQ6ylsegNmmGYdf9YFVbxB4qBhnFQo"
key_foco_trap = "1Jna3Bu47gjBWWz5vCoel4ksa-LBuo8R3zVfQYFl73wI"

fn_gdrive_database = file.path(dir_input,"wnv-s_database - data.csv") # <<<<<<<--------------------------------------------------- U S E R   I N P U T
fn_trap_malfunction <- file.path(dir_input, "trap_malfunction - data.csv")
fn_trap <- file.path(dir_input, "foco_trap - data.csv")


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-----U S E R   D E F I N E D   S T A T I C   D A T A   P A R A M E T E R S : -------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

fc_zones = c("NE", "SE", "NW", "SW")
non_fc_zones = c("LV", "BC", "BE")
all_zones = c("NE", "SE", "NW", "SW", "LV", "BC", "BE")

copy_threshold = 500 # <<<<<<<------------------------------------------------------------------------------------------------ U S E R   I N P U T
rn_threshold = 34000 # <<<<<<<------------------------------------------------------------------------------------------------ U S E R   I N P U T
vi_threshold = 0.75 # <<<<<<<------------------------------------------------------------------------------------------------- U S E R   I N P U T




#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------ U S E R   D E F  V A R I A B L E   D A T A   P A R A M E T E R S : -----------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#variables that will change from week to week
week_hardcode = 37  # <<<<<<<------------------------------------------------------------------------------------------------ U S E R   I N P U T
year_hardcode = 2024 # <<<<<<<----------------------------------------------------------------------------------------------- U S E R   I N P U T

# Create an argument parser
  parser <- ArgumentParser(description = "Script to handle config file data inputs")
  
  # Add arguments for fn_trap and fn_database_input
  parser$add_argument("--week", help = "week of report", type = "integer")
  parser$add_argument("--year", help = "year of the report", type = "integer")
  
  # Parse arguments
  args <- parser$parse_args()
  
  # Assign default values if arguments are not provided
  week_filter <- if (!is.null(args$week)) args$week else week_hardcode
  year_filter <- if (!is.null(args$year)) args$year else year_hardcode
  
  # Output the values
  cat("Week filter set to:", week_filter, "\n")
  cat("Year filter set to:", year_filter, "\n")

week_filter_yr= 23:week_filter
week_filter_hx = 23:37
year_filter_hx = seq(year_filter-12, year_filter-1, by = 1)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- O U T F I L E  N A M E  G E N E R A T I O N --------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

if(length(year_filter) > 1){ #if looking at multiple years then create YYYY-YYYY range
  fn_year = paste0(min(year_filter), "-", max(year_filter))
} else { #otherwise fn_year stays the same
  fn_year = year_filter
}

if(length(week_filter) > 1){ #if looking at multiple years then create YYYY-YYYY range
  fn_week = paste0(min(week_filter), "-", max(week_filter))
} else { #otherwise fn_year stays the same
  fn_week = week_filter
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------- F I L E  N A M E S  M I D ------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fn_database_update = file.path(dir_mid, "wnv-s_database_update.csv")
fn_datasheet_clean = paste0(dir_mid,"/y",fn_year, "_", "w",fn_week, "_datasheet.csv")
fn_datasheet_clean_test = paste0(dir_mid,"/y",fn_year, "_", "w",fn_week, "_datasheet_test.csv")
fn_weekly_input_format_mid = file.path(dir_mid, "weekly_data_input_format_mid.RData")
fn_cq_out = paste0(dir_mid,"/y",fn_year, "_", "w",fn_week, "_platemap.csv")
fn_abund_out = paste0(dir_mid,"/y",fn_year, "_", "w",fn_week, "_abundance")
fn_pools_mid = paste0(dir_mid,"/y",fn_year, "_", "w",fn_week, "_pools.csv")
fn_inactive_trap = file.path(dir_mid, "inactive_traps.csv")
fn_func_trap = file.path(dir_mid, "functional_traps.csv")
fn_max_trap_yr = file.path(dir_mid, "max_trap_zone_yr.csv")
fn_trap_p_wk = file.path(dir_mid, "trap_p_wk.csv")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- F I L E  N A M E S   O U T P U T ------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

fn_gdrive_archive = paste0("wnv-s_database_pre_y",year_filter, "_w", week_filter,".gsheet")
fn_data_output = paste0(dir_output,"/y",fn_year, "_", "w",fn_week, "_data_update.csv")
fn_weekly_input_format = file.path(dir_output, "weekly_data_input_format.csv")
fn_stds_ctrl_slev_bird = file.path(dir_output, "std_ctrl_slev_bird.csv")
fn_non_database_sample = file.path(dir_output, "non_database_samples(std-ctrl-bird-etc).csv")



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------------------- C O L U M N  S E L E C T I O N -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
col_trap = c("zone", "lat", "long")

col_input_database = c("csu_id", "trap_id", "year", "week", "trap_date", 
                   "county", "method", "spp", "total", "test_code", "zone")

col_datasheet = c("csu_id", "trap_id", "year", "week", "trap_date", 
             "county", "method", "genus", "spp", "sex", "no_gravid",
             "no_deplete", "total", "test_code", "seq", "cq", 
             "zone", "lat", "long")

col_database = c( "csu_id", "trap_id", "zone", "year", "week", "trap_date", "county", "method", "spp",
  "total", "test_code", "cq", "copies_WNV", "seq", "cq", "lat", "long")

col_pcr_check <- c("csu_id", "test_code", "ct_threshold", "plate", "copies_SLEV", "copies_WNV", "cq_SLEV", "cq")

col_class_database <- c("csu_id" = "character", 
            "trap_id" = "character", 
            "year" = "numeric", 
            "week" = "numeric", 
            "trap_date" = "character", 
            "county" = "character", 
            "method" = "character", 
            "genus" = "character", 
            "spp" = "character", 
            "sex" = "logical", 
            "no_gravid" = "numeric", 
            "no_deplete" = "numeric", 
            "total" = "numeric", 
            "test_code" = "numeric", 
            "seq" = "numeric", 
            "cq" = "numeric",
            "zone" = "character",
            "lat" = "numeric",
            "long" = "numeric")

col_rename_datasheet <- c("csu_id" = "CSU Pool Number (CMC Enters)" , 
               "trap_id" = "Collection Site       (Trap ID)", 
               "year" = "Year", 
               "week" = "Week", 
               "trap_date" = "Trap Date", 
               "county" = "County", 
               "method" = "L/G", 
               "genus" = "Genus", 
               "spp" = "Species", 
               "sex" = "Sex", 
               "no_gravid" = "No. Gravid", 
               "no_deplete" = "No. Deplete", 
               "total" = "Total", 
               "test_code" = "Test Code (CSU Enters)" , 
               "zone" = "Zone")



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------- G R O U P _ B Y  V A R S -------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
grp_vars = c("year", "week", "zone", "spp")
hx_grp_vars = c("week", "zone")
zone_lvls = c("NW", "NE", "SE","SW", "FC", "LV", "BE", "BC")
non_routine_zones = c("BC") # <<<<<<<--------------------------------------------------------------------- U S E R   I N P U T

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------- L O A D   G S H E E T _ P U L L . R -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fn_gsheet_pull = file.path(dir_scripts, "gsheet_pull.R")

# Download gsheet_pull.R
download.file("https://raw.githubusercontent.com/rtobiaskoch/wnv-s_data_tools/refs/heads/main/0_R/gsheet_pull.R", 
              fn_gsheet_pull, 
              mode = "wb")

if(!exists('gsheet_pull', mode = 'function')) {
  if(file.exists(fn_gsheet_pull)) {
    source(fn_gsheet_pull)
  } else { warning("No gsheet_pull.R script is available. Check github or scripts.")   }
}


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------------------- C O L O R  S E T T I N G S -------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
color_palette = wes_palette('Darjeeling1')

#https://coolors.co/palette/fb8b24-d90368-820263

pal_mozzy = c("hx_Tarsalis" = "grey50",
              "hx_Pipiens" = "grey30",
              "current_Tarsalis" = "#e9724c",
              "current_Pipiens" = "#820263")

pal_mozzy2 = c("Pipiens" = "#820263",
               "Tarsalis" = "#e9724c",
               "All" = "#faa916")


pal_mozzy3 = c("#ffc857", "#e9724c", "#c5283d")

curr_hx_pal = c("current" = "#e9724c",
                "hx"      = "grey50")


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------- E X P O R T   C O N F I G ----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
all_params <- ls(envir = .GlobalEnv)

# Create a list containing all the values of these objects
all_params_list <- mget(all_params, envir = .GlobalEnv)

saveRDS(all_params_list, config_params_file)


if(file.exists(config_params_file)) {
  cat("config file ", config_params_file, "save successfully.")
}


# save to a log file
if(!dir.exists("logs")){
  dir.create("logs")
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------------------- S A V E  L O G  F I L E  ------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
log_file = paste0("logs/params_y",year_filter,"_w",week_filter, "_", Sys.Date(), ".txt")
sink(log_file)
print(paste0(config_params_file, " Log"))
all_params_list
sink()




