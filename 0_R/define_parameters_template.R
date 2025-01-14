#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------------- D E F I N E   P A R A M E T E R S ---------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#if config_params.RDS file exists read in the variables
#else define the necessary inputs yourself
if(file.exists("1_input/config_params.RDS")){
  list2env(readRDS("1_input/config_params.RDS"),          
           envir = .GlobalEnv)
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