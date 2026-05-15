rm(list = ls())

list2env(readRDS("1_input/config_params.RDS"),          
         envir = .GlobalEnv)

pacman::p_load(tidyverse, googledrive)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------------- R E A D   D A T A  -------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
fn_new_data = "2_output/wnv-s_database_0_expand.csv"
fn_old_data = "1_input/wnv-s_database.csv"

#join cq (pcr) data and the trap location data to get new data to add to the database
#keep the standards controls and other samples that arent i 
new_data0 = read.csv(fn_new_data)  %>%
  mutate(csu_id = str_remove(csu_id, "-"))

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#---------- P U L L   &   U P D A T E    G D R I V E    D A T A B A S E  ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#pull database from googledrive location and read it as a csv
#gsheet_pull(database_gsheet_key, "data", fn_old_data)

database = read.csv(fn_old_data) %>%
  mutate(csu_id = str_remove(csu_id, "-"))

database_col = colnames(database)

#make a copy of the database before update
drive_cp(file = as_id(database_gsheet_key), 
         path = "database_archive",
         name = fn_gdrive_archive,
         overwrite = T)

#will update any csu_id that matches with the new_data and adds it if it doesn't
database_update = rquery::natural_join(new_data0, 
                                       database, 
                                       by = "csu_id",
                                       jointype = "FULL")

#csu_ids in database not in new data
missing_id = anti_join(database, new_data0, by = "csu_id")


database_update = database_update %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", .))) %>% #prevents NA from making non character variables in database throw errors
  select(all_of(database_col)) %>% #reorder them because natural_join fucked up the order
  arrange(year, week, zone, trap_id, desc(trap_date), spp) %>%
  distinct(csu_id, trap_id, trap_date, spp, .keep_all = T)

#save it to the working directory
#write.csv(database_update, fn_database_update, row.names = F)


#save it to gdrive
googlesheets4::sheet_write(database_update,
                           ss = database_gsheet_key,
                           sheet = "data"
)
# END OF ELSE FOR MISSING TRAP CHECK
