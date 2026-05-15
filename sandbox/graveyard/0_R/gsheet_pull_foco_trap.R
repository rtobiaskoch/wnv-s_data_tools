#run this to run the function gsheet_pull an wnv-s_database

#devtools::source_url("https://github.com/rtobiaskoch/TK_useful_functions/blob/main/gsheet_pull.R")
source("0_R/gsheet_pull.R")

gsheet_pull(key = "1Jna3Bu47gjBWWz5vCoel4ksa-LBuo8R3zVfQYFl73wI", 
            sheet = "data",
            out_fn = "1_input/foco_trap.csv")
