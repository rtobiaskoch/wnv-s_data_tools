rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_unload()
pacman::p_load(tidyverse, rio, lubridate)


fc_zones = c("NE", "SE", "NW", "SW", "LV", "BC", "BE", "BC")
grp_vars = c("year", "week", "zone", "spp")


#get abundance by zone
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