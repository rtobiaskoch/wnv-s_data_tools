rm(list = ls())
list2env(readRDS("1_input/config_params.RDS"),          
         envir = .GlobalEnv)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, PooledInfRate)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#GET RAW DATA 
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#GET DATA
data_input = read.csv("1_input/wnv-s_database.csv") #24-10-28 updated with 0 traps

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------------T R A P S ----------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#get number of active traps. For the purposes of historical calculations not going to consider malfunctioning traps

  trap_data0 = data_input %>% 
    filter(method == "L") %>%
    distinct(year, week, zone, trap_id) %>%
    group_by(year, week, zone) %>%
    summarize(trap_L= n())
    
  trap_data_fc = data_input %>% 
    filter(method == "L") %>% #only keep active light traps
    filter(zone %in% fc_zones) %>% #keep zones in group
    distinct(year, week, zone, trap_id) %>%
    mutate(zone = "FC") %>% #change zones to FC
    group_by(year, week, zone) %>%
    summarize(trap_L = n())

trap_data = rbind(trap_data0, trap_data_fc)
rm(trap_data0, trap_data_fc)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------------M O Z Z I E S ----------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
suppressMessages({
  # Your code with group_by and summarize
  #get number of traps per night
  
  #get number of mosquitoes per zone per week per year
  m_p_wk0 = data_input %>%
    ungroup() %>%
    filter(method == "L") %>% #remove gravid traps for abundance calculation
    group_by(year,week,zone,spp) %>% #calc number of mosquitoes per week per zone
    summarize(mosq_L = sum(total)) %>%
    ungroup()
})



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#HX: GET MOSQUITOES FOR CITYWIDE FC
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


suppressMessages({ 
  fc_m_p_wk = m_p_wk0 %>%
    filter(zone %in% fc_zones) %>% # keep only FC zones
    group_by(year,week,spp) %>% #get number of mosquitoes per week per zone per species
    summarize(zone = "FC",
              mosq_L = sum(mosq_L)) %>% # changed from the mean which would be incorrect 
    #because I compare it to total number of traps per zone not the average
    ungroup() # %>%
  
  m_p_wk = rbind(m_p_wk0, fc_m_p_wk)
  rm(m_p_wk0, fc_m_p_wk)
})  


suppressMessages({ 
  #get abundance per trap 
  df_abund = left_join(m_p_wk, trap_data, by = c("year", "week", "zone")) %>%
    mutate(abund = round(mosq_L/trap_L,2)) %>%
    complete(zone, week) %>% #fill in missing weeks for the non-routine zones
    filter(!is.na(week)) #%>%
  #select(-mosq_L, -trap_L)
  # pivot_wider(names_from = zone, 
  #             values_from = abund,
  #             names_prefix = "abund_")
})
``

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------- C A L C  C I   F O R   ---------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

abund_ci_fun = function(df, grp, value) {
  df %>% 
    group_by(across(all_of(grp))) %>%
    summarise(
      lower_ci = t.test({{value}})$conf.int[1],
      upper_ci = t.test({{value}})$conf.int[2]
    )
}

abund_ci_fun(df_abund, grp_vars, abund) 



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------- C A L C   P I R:  A L L  --------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#create a grouping variable for mle
data_input = data_input %>%
  arrange(across(all_of(grp_vars))) %>% #dont split by method because PIR includes gravid traps
  mutate(grp = paste(year,week,zone,spp, sep ="-"))

#run pIR
mle = pIR(test_code ~ total|grp, data = data_input, pt.method = "firth")


#create pIR dataframe
df_pir0 = as.data.frame(mle) %>%
  separate(grp,
           into = c("year", "week", "zone", "spp"),
           sep = "-") %>%
  transmute(year = as.integer(year),
            week = as.integer(week),
            zone = zone,
            spp = spp,
            pir = round(P,4),
            pir_lci = round(Lower,4),
            pir_uci = round(Upper,4)
  )

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#----------------- C A L C   P I R:  F C -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#calculate FC row for sum
suppressMessages({
  fc_pir0 = data_input %>%
    filter(zone %in% fc_zones) %>% #keep only fc zones
    mutate(zone = "FC") %>% #change zone to be FC
    mutate(grp = paste(year,week,zone,spp, sep ="-"))
  
  mle = pIR(test_code ~ total|grp, data = fc_pir0, pt.method = "firth")
  
  fc_pir0 =  as.data.frame(mle) %>%
    separate(grp,
             into = c("year", "week", "zone", "spp"),
             sep = "-") %>%
    transmute(year = as.integer(year),
              week = as.integer(week),
              zone = zone,
              spp = spp,
              pir = round(P,4),
              pir_lci = round(Lower,4),
              pir_uci = round(Upper,4)
    )
})



df_pir = rbind(df_pir0, fc_pir0)
rm(df_pir0, fc_pir0)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-------------- C O M B I N E   D A T A -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_zone_wk0 = df_abund %>%
  left_join(df_pir, by = grp_vars) %>%
  mutate(vi = round(abund * pir,4),
         vi_lci = round(abund * pir_lci,4),
         vi_uci = round(abund * pir_uci,4)) %>%
  mutate(year = factor(year),
         week = factor(week),
         zone = factor(zone, levels = zone_lvls),
         spp = factor(spp, levels = c("Pipiens", "Tarsalis", "All"))) %>%
  arrange(zone, spp)

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------- S U M    A L L   S P P ----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
sum_col = c("mosq_L", 
            "abund", 
            "vi", "vi_lci", "vi_uci")

distinct_col = c("trap_L")

suppressMessages({
  data_zone_wk_spp_all0 = data_zone_wk0 %>%
    mutate(spp =  "All") %>%
    group_by(year, week, zone, spp) %>%
    summarise(spp = "All",
              across(all_of(sum_col), sum),
              across(all_of(distinct_col), ~max(.))
    )
  
})

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------- C A L C   P I R   A L L   S P P ---------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>å

suppressMessages({
  pir_all_spp = data_zone_wk0 %>%
    group_by(year, week, zone) %>%
    summarize(spp = "All",
              pir = sum(vi)/sum(abund),
              pir_lci = sum(vi_lci)/sum(abund),
              pir_uci = sum(vi_uci)/sum(abund))
  
})



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------- J O I N   A L L   S P P ---------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_zone_wk_spp_all = left_join(data_zone_wk_spp_all0, pir_all_spp, by = grp_vars)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------- C O M B I N E   A L L   S P P ---------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
data_zone_wk = rbind(data_zone_wk0, data_zone_wk_spp_all) %>% 
  arrange(year, week, zone, spp) %>%
  filter(zone == "BE" & !is.na(year))







#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#-----------------------H X :   V I / A L L ---------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

df_all_hx_long = data_zone_wk %>%
  select(-mosq_L, -trap_L) %>%
  pivot_longer(cols = -c(grp_vars),
               names_to = "est",
               values_to = "value") %>%
  mutate(type = "hx")

df_hx_wide = data_zone_wk %>%
  select(year, week, zone, spp, abund, pir, vi) %>%
  group_by(zone, week, spp) %>%
  summarise(abund = mean(abund), 
            pir = mean(pir), 
            vi = mean(vi)) %>%
  filter(spp == "All") %>%
  select(-spp) %>%
  pivot_wider(names_from = zone, 
              values_from = c(abund, pir, vi),
              names_prefix = "hx_")





#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------------- P L O T T I N G --------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
source("0_R/ci_line_plot_fun.R")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------------------- C I ---------------------------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
df_ci_hx = data_zone_wk %>%
  ungroup() %>%
  filter(week == week_filter) %>%
  select(zone, week, spp, vi, vi_lci, vi_uci) %>%
  group_by(zone, week, spp) %>%
  summarize_if(is.numeric, ~mean(.x)) %>%
  mutate(type = "hx")














df_ci_spp = rbind(df_ci_hx, df_ci_curr) %>%
  filter(type != "hx") %>%
  #filter(spp != "All") %>%
  mutate(vi_uci = if_else(vi == 0, 0, vi_uci)) #make uci 0 if vi 0 otherwise makes it look like there was a positive


df_ci_all = data_zone_wk %>%
  ungroup() %>%
  filter(week == week_filter) %>%
  select(year, zone, week, spp, vi, vi_lci, vi_uci)  %>%
  mutate(type = "hx")  %>%
  rbind(df_ci_curr) %>%
  mutate(year = as.integer(as.character(year))) %>%
  #filter(type != "hx") %>%
  filter(spp == "All") %>%
  mutate(vi_uci = if_else(vi == 0, 0, vi_uci))


pd = position_dodge(0.3)

p_df_ci_spp = ggplot(df_ci_spp, aes(x = week, y = vi, color = spp)) +
  geom_errorbar(aes(ymin = vi_lci, ymax = vi_uci), size = 0.8, width = .2, position = pd) +
  geom_point(size = 2, position = pd) +
  geom_hline(yintercept = 0) +
  facet_grid(zone~.) +
  theme_classic() +
  scale_color_manual(values = mozzy_pal2) +
  geom_hline(yintercept = vi_threshold, linetype = "dashed", color = "red") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
  coord_cartesian(ylim = c(0, 1)) +
  ggtitle("VI 95% CI") +
  labs(y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")


p_df_ci_all = ggplot(df_ci_all, aes(x = year, y = vi, color = type)) +
  geom_errorbar(aes(ymin = vi_lci, ymax = vi_uci), size = 0.8, width = .2, position = pd) +
  geom_point(size = 2, position = pd) +
  geom_hline(yintercept = 0) +
  facet_grid(zone~.) +
  theme_classic() +
  scale_color_manual(values = curr_hx_pal) +
  geom_hline(yintercept = vi_threshold, linetype = "dashed", color = "red") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
  scale_x_reverse(breaks = seq(min(df_ci_all$year, na.rm = T), max(df_ci_all$year, na.rm = T), by = 2)) +
  coord_cartesian(ylim = c(0, 1)) +
  ggtitle(paste("VI Week", week_filter, sep = " ")) +
  #labs(y = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.text.x = element_text(angle = 90))

p_df_ci_all

#generate description of time frames
yr_calc = data_zone_wk %>%
  filter(!zone %in% fc_zones) %>%
  group_by(zone) %>%
  summarize(min = min(as.numeric(as.character(year)), na.rm = T),
            max = max(as.numeric(as.character(year)), na.rm = T)) 

description = paste0(yr_calc$zone, " Historical Data was calculated from year ", 
                     yr_calc$min, 
                     " to ", 
                     yr_calc$max)

description = paste0(description, collapse = "\n")


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------------- P L O T : S P P ( A L L ) -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

df_all_long = rbind(current_year_long, df_all_hx_long) %>%
  mutate(type = factor(type, levels = c("hx", "current")),
         zone = factor(zone, levels = zone_lvls)) %>%
  pivot_wider(names_from = est, values_from = value) %>%
  group_by(zone, week, spp, type) %>%
  summarise(abund = mean(abund), 
            pir = mean(pir), 
            vi = mean(vi)) %>%
  filter(spp == "All")




p_df_all_fun = function(df, value, text) {
  
  ggplot(df, aes(x = week, y = {{value}}, 
                 color = type, fill = type, group = type)) +
    geom_hline(yintercept = 0) +
    geom_area(position = "dodge", alpha = 0.3) +
    facet_grid(zone ~ .) +
    theme_classic() +
    ggtitle(text) +
    scale_color_manual(values = curr_hx_pal) +
    scale_fill_manual(values = curr_hx_pal)
}   

p_abund = p_df_all_fun(df_all_long, abund, "Abundance")
p_abund 

p_pir = p_df_all_fun(df_all_long, pir, "Pooled Infection Rate")
p_pir 

p_vi = p_df_all_fun(df_all_long, vi, "Vector Index") + 
  geom_hline(yintercept = vi_threshold, linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25))
p_vi


p_hx_current0 = p_abund + p_pir + p_vi + 
  plot_annotation(caption = c(description)) +
  plot_layout(#widths =c(3,3,3,1), 
    guides = "collect") & theme(legend.position = 'bottom', 
                                legend.title = element_blank())



ggsave("data_output/plots/hx_plot_all.png", p_hx_current0, height = 8, width = 12, units = "in")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#--------------------------------- P L O T : S P P -----------------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
df_all_long = rbind(current_year_long, df_all_hx_long) %>%
  mutate(type = factor(type, levels = c("hx", "current")),
         zone = factor(zone, levels = zone_lvls)) %>%
  pivot_wider(names_from = est, values_from = value) %>%
  group_by(zone, week, spp, type) %>%
  summarise(abund = mean(abund), 
            pir = mean(pir), 
            vi = mean(vi)) %>%
  mutate(type2 = paste0(type,"_", spp)) %>%
  filter(spp != "All")

write.csv(df_all_long, "df_all_long_hx.csv", row.names = F)




p_df_all_fun2 = function(df, value, text) {
  
  ggplot(df, aes(x = week, y = {{value}}, group = interaction(type2, spp))) +
    # Separate geom_area for stacking
    geom_area(data = df %>% filter(type == "hx"), aes(fill = type2, color = type2), position = "stack", alpha = 0.5) +
    geom_area(data = df %>% filter(type == "current"), aes(fill = type2, color = type2), position = "stack", alpha = 0.5) +
    # geom_point(data = df %>% filter(week == week_filter & type = "current") %>% 
    #              group_by(zone, week)) %>% summarise(vi = sum(vi)), aes(fill = )
    geom_hline(yintercept = 0) +
    # Separate geom_area for dodging
    #  geom_area(data = df %>% filter(type %in% c("hx", "current")), aes(fill = type2), position = "dodge", alpha = 0.5) +
    facet_grid(zone ~ .) +
    theme_classic() +
    ggtitle(text) +
    scale_color_manual(values = mozzy_pal) +
    scale_fill_manual(values = mozzy_pal)
}


p_abund = p_df_all_fun2(df_all_long, abund, "Abundance")
p_abund 

p_pir = p_df_all_fun2(df_all_long, pir, "Pooled Infection Rate")
p_pir 

p_vi = p_df_all_fun2(df_all_long, vi, "Vector Index") + 
  geom_hline(yintercept = vi_threshold, linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25))
p_vi



p_hx_current = p_abund + p_pir + p_vi + p_df_ci_all + 
  plot_annotation(caption = c(description)) +
  plot_layout(widths =c(3,3,3,1), guides = "collect") & theme(legend.position = 'bottom', legend.title = element_blank())

p_hx_current

ggsave("data_output/plots/hx_plot.png", p_hx_current, height = 8, width = 12, units = "in")
