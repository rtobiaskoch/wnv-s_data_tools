#run this to run the function gsheet_pull an wnv-s_database

#devtools::source_url("https://github.com/rtobiaskoch/TK_useful_functions/blob/main/gsheet_pull.R")
source("0_R/gsheet_pull.R")

gsheet_pull(key = "12Mf-w9I9NHTTDjzEPRoxUE08ka4WZ6RE-RM1s-FW7qA", 
            sheet = "data", 
            out_fn = "1_input/wnv-s_database.csv",
            update = T)
