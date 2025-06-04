#DESCRIPTION 
#removes traps that aren't in the list provided by fn_trap_keep


#USER INPUT
config = list(
  path = "..",
  pattern = "^(LC).*all mosquitoes.(xlsx|csv)$", #pattern to look for in fun_read_list 
  fn_trap_keep  = "../1_input/foco_trap.csv", #file name to be used for filtering traps if you don't want to filter by trap provide an empty dataframe
  fn_database = "../1_input/wnv-s_database.csv",
  fn_rename_col = "../1_input/database_column_rename.csv",
  na_col = "trap_id",
  output_culex_database = "../3_output/culex_sheet_database_augment.csv",
  output_culex = "../3_output/culex_sheet_database.csv", #output filename
  output_plot = "../3_output/culex_sheet_combined_abundance.png",
  filter_zone = c("BC", "BE"), #zone to remove for plotting abundance
  fc_zone = c("NE", "NW", "SE", "SW")
)

