# this script is to add the total mosquito numbers by trap that are provided by VDCI 
#in a spreadsheet called Culex that is separate from the pool level data. 

#this was brought to my attention by the fact that abundance numbers were so much lower in 2023 based off pooling data 
#compared to this trap data sheet because they were only sending a portion of the total. 
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_unload()
pacman::p_load(tidyverse, rio, lubridate)


fc_zones = c("NE", "SE", "NW", "SW", "LV", "BC", "BE", "BC")
grp_vars = c("year", "week", "zone", "spp")

fun_read_list <- function(pattern) {
  list.files(pattern = pattern,
             recursive = TRUE,
             full.names = TRUE,
             ignore.case = TRUE) %>%
    map(~ rio::import(.x)) %>%
    map(~ mutate_all(., as.character)) %>%
    bind_rows()
}


#ALL ZONES OTHER THAN BC
data0 = fun_read_list("^LC.*Culex.csv")

data = data0 %>% 
  mutate_if(is.character, ~trimws(.x)) %>%
  mutate(trap_date = mdy(date_trap_set)) %>%
  transmute(trap_id = trap_name,
            year = year(trap_date),
            week = week(trap_date),
            zone = zone,
            spp = case_when(mosquito_species == "Culex tarsalis" ~ "Tarsalis",
                            mosquito_species == "Culex pipiens" ~ "Pipiens"),
            total = as.numeric(mosquito_count))

#BC 
data_bc0 = fun_read_list("^Boulder.*Culex.(xlsx|csv)$")

data_bc = data_bc0 %>%
  mutate_if(is.character, ~trimws(.x)) %>%
  mutate(date_trap_set = ifelse(grepl("\\d+/\\d+/\\d+", date_trap_set), 
                                format(mdy(date_trap_set), "%Y-%m-%d"), 
                                date_trap_set)) %>%
  transmute(trap_id = trap_name,
            year = year(date_trap_set),
            week = week(date_trap_set),
            zone = zone,
            spp = case_when(mosquito_species == "Culex tarsalis" ~ "Tarsalis",
                            mosquito_species == "Culex pipiens" ~ "Pipiens"), 
            total = as.numeric(mosquito_count))

#combine 
data = bind_rows(data, data_bc)

write.csv(data, "3_output/culex_sheet_combined.csv")

data_zone_wk_new0 = data %>%
  filter(!grepl("gr", trap_id)) %>% #remove gravid trap
  group_by(!!!syms(grp_vars)) %>%
  summarize(mosq = sum(total),
          n_trap = n_distinct(trap_id), #count the number of traps set for the week by spp
          .groups = "drop") %>%
  group_by(year, week, zone) %>%
  mutate(n_trap = max(n_trap)) %>% #get the max number of traps by zone for the week because it is not counting traps with 0 pipiens
  mutate(abund = round(mosq/n_trap,2)) %>%
  filter(zone %in% fc_zones)

data_zone_wk_new_all = data_zone_wk_new0 %>%
  group_by(year, week, zone, n_trap) %>%
  summarize(mosq = sum(mosq),
          .groups = "drop") %>%
  mutate(spp = "All") %>%
  mutate(abund = round(mosq/n_trap,2)) %>%
  filter(zone %in% fc_zones) 

data_zone_wk_new = rbind(data_zone_wk_new0, data_zone_wk_new_all) %>%
  arrange(!!!syms(grp_vars))

write.csv(data_zone_wk_new, "3_output/data_zone_wk_new_update.csv")
            
#update database
#source("0_R/gsheet_pull_wnv-s_database.R")
#read in database
database = read.csv("1_input/wnv-s_database.csv")

data_zone_wk = read.csv("1_input/data_zone_wk_stats.csv")

data_zone_wk_comb = data_zone_wk_new %>%
  #select(year, week, zone, spp, mosq, mosq_L, trap_L, abund) %>%
  rquery::natural_join(data_zone_wk, 
                       by = c("year", "week", "zone", "spp"), 
                       jointype = "FULL") # %>%

write.csv(data_zone_wk_comb, "3_output/data_zone_wk_new_update.csv", row.names = F)

#updated the data_zone_wk code to have natural join so code below doesn't work

# 
# data_zone_wk_comb %>%
#   select(year, week, zone, abund.x, abund.y) %>%
#   pivot_longer(cols = c( abund.x,  abund.y),
#                names_to = "source",
#                values_to = "abund") %>%
# ggplot(aes(x = week, y =  abund, fill = source)) +
#   geom_col(position = "dodge") +
#   facet_grid(zone ~ year)
# 
# 
# ggplot(data_zone_wk_comb, aes(x = week, y =  `abund trap/pool` )) +
#   geom_col() +
#   facet_grid(zone ~ year)
# 
# 
# data_zone_wk_comb %>%
#   filter(year > 2020) %>%
# ggplot(aes(x = week, y =  `abund trap/pool` )) +
#   geom_col() +
#   geom_hline(yintercept = 1, color = "red") +
#   facet_grid(zone ~ year)



 