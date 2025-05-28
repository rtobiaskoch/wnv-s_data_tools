# this script is to add the total mosquito numbers by trap that are provided by VDCI 
#in a spreadsheet called Culex that is separate from the pool level data. 

#this was brought to my attention by the fact that abundance numbers were so much lower in 2023 based off pooling data 
#compared to this trap data sheet because they were only sending a portion of the total. 


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------------- S E T   E N V I R O N M E N T -------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rm(list = setdiff(ls(), "config"))

#define packages to be used
pkg = c("dplyr", "lubridate", "purrr", "ggplot2")
cat("\n Loading required packages: ", pkg, "\n")
pacman::p_load(pkg, character.only = T)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------------  C O N F I G  ------------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#define inputs and paramters that can be updated by the user

if(!exists("config")) {
  source("config/config_combine_culex_sheet.R") #<--------------------------------------------------------USER INPUT
  cat("\n Config defined in config/config_combine_culex_sheet.R \n")
} else {
  cat("\nconfig has been defined in pipeline outside of this script.\n")
}

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------------  C O M P I L E  ------------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#run functions using user defined parameters from the config file

# R E A D  &  C O M B I N E
data0 = read_list(config$pattern)

# C L E A N
data = culex_sheet_clean(data0)
#REMOVE TRAPS THAT ARE NOT ONES WE TEST

# F I L T E R
cat("\n Reading in active foco traps to remove untested traps from", config$fn_foco_trap, "\n")
trap_keep_df = read.csv(config$fn_trap_keep)

data_filtered = filter_culex_sheet(df = data, 
                                  na_col = rlang::sym(config$na_col), 
                                  trap_keep_df = trap_keep_df)

cat("\n Saving culex sheet trap level data to", config$output, "\n")
write.csv(data_filtered, config$output)

# P L O T 
p <- plot_abundance_by_zone(data_filtered, filter_zone = config$filter_zone)

ggsave(filename = "3_output/culex_sheet_combined_abundance.png", plot = p,
       width = 5, height = 3, units = "in")

write.csv(data_filtered, config$output)





 