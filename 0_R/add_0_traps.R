
#get gsheet_pull off github
#source(https://raw.githubusercontent.com/rtobiaskoch/TK_useful_functions/refs/heads/main/gsheet_pull.R)

rm(list = ls())

list2env(readRDS("1_input/config_params.RDS"),          
         envir = .GlobalEnv)

pacman::p_load(tidyverse)
source("0_R/gsheet_read_fun.R")
# gsheet_pull(key = trap_gsheet_key,
#             sheet = "data",
#             out_fn = "1_input/foco_trap.csv")
# 
#  gsheet_pull(key = database_gsheet_key,
#             sheet = "data",
#             out_fn = "1_input/wnv-s_database_old.csv")

foco_traps0 = read.csv("1_input/foco_trap.csv")

foco_traps = foco_traps0 %>%
 # filter(active == 1) %>%
  unite("temp", trap_id, zone, method, sep = "|") #unite to avoid erroneous expansion of all zones for each trap_id



data_input = read.csv("1_input/wnv-s_database_old.csv") %>%
  select(-lat , -long)  %>% #remove to update with active trap list which is more accurate.
  mutate(csu_id = str_remove(csu_id, "-")) 

year = min(data_input$year):max(data_input$year)
week = min(data_input$week):max(data_input$week)
spp = unique(data_input$spp)
trap = foco_traps$temp

#generate all possible combinations of traps across the data set
trap_expand = expand_grid(year, 
                          week,
                          trap,
                          spp)  %>%
  separate_wider_delim(trap, delim = "|", names = c("trap_id", "zone", "method")) %>%
  #remove new BE traps which all start with BE before 2024 
  filter(!(grepl("^BE", trap_id) & year < 2024))
  

data_input_expand = trap_expand %>%
  left_join(data_input, by = c("year", "week", "zone", "trap_id", "method", "spp")) %>%
  #remove zones with no data for entire week for a zone
  group_by(year, week, zone, trap_id) %>% 
  filter(!all(is.na(csu_id))) %>% #remove weeks where there was no trapping in the particular zones
  #fill in the likely missing trap date and county from other traps in same zone
  arrange(year, week, zone, trap_id) %>%
  group_by(year, week, zone) %>% 
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
  
#check to make sure there are the same number of traps for every week for each zone
#sd should be 0 (except for BE which added traps)
check = data_input_expand %>%
  group_by(year, week, zone) %>% 
  distinct(trap_id) %>% 
  count() %>% 
  group_by(zone) %>% 
  summarize(mean = mean(n), sd = sd(n))

write.csv(data_input_expand, "1_input/wnv-s_database.csv", row.names = F)

googlesheets4::sheet_write(data_input_expand,
                           ss = database_gsheet_key,
                           sheet = "data"
)
