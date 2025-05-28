#DESCRIPTION 
#removes traps that aren't in the list provided by fn_trap_keep


#USER INPUT
config = list(
  path = "..",
  pattern = "^(LC|Boulder).*Culex.(xlsx|csv)$", #pattern to look for in fun_read_list 
  fn_trap_keep  = "../1_input/foco_trap.csv", #file name to be used for filtering traps if you don't want to filter by trap provide an empty dataframe
  na_col = "trap_id",
  #output_all = "3_output/culex_sheet_combined_all.csv",
  output_df = "../3_output/culex_sheet_combined_surv_traps.csv", #output filename
  output_plot = "../3_output/culex_sheet_combined_abundance.png",
  filter_zone = "BC" #zone to remove for plotting abundance
)

