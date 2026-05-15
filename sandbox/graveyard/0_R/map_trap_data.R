#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#LOAD PACKAGES FOR PIPELINE
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
rm(list = ls())
#IF THE CONFIG PARAMS FILE EXISTS US THAT TO DEFINE INPUTS 
# ELSE DEFINE THEM HERE
config_params_file = "1_input/config_params.RDS" # <<<<<<<<<<----------------------------------  USER INPUT

if(file.exists(config_params_file)){
  list2env(readRDS(config_params_file), 
           envir = .GlobalEnv)
} else {
  
  
  #LOAD PACKAGES
  suppressMessages({
    if (!require("pacman")) install.packages("pacman")
    pacman::p_unload()
    pacman::p_load(tidyverse, readxl, purrr #manipulation
    )
  })
  source("0_R/gsheet_pull_prompt.R")
 
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #--------------------------- I N P U T S --------------------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #INPUTS FOR THIS SCRIPT IF CONFIG FILE DOESN'T EXIST
  dir_input = "1_input"
  
  #TIME FILTERS
  key_trap_gsheet = "1Jna3Bu47gjBWWz5vCoel4ksa-LBuo8R3zVfQYFl73wI"
  fn_trap <- file.path(dir_input, "foco_trap - data.csv")

} # end of if config_params_file exist statement
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
source("0_R/gsheet_pull_prompt.R")
library(leaflet)
library(RColorBrewer)
library(mapview)
library(htmlwidgets)

fun_gsheet_pull_prompt(filename = fn_trap, "data", key =  key_trap_gsheet)

trap_data = read.csv(fn_trap) %>%
  mutate(zone = factor(zone, levels = c("NW", "NE", "SW", "SE", "LV","BE", "BC")))

# Create a color palette based on the 'zone' column
pal <- colorFactor(palette = "Set2", domain = trap_data$zone)

# Create the Leaflet map
trap_map = leaflet(trap_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  #addProviderTiles("CartoDB.DarkMatter") %>%
  addCircleMarkers(
    ~long, ~lat,
    color = ~pal(zone),
    popup = ~paste0("Trap ID: ", trap_id, "<br>Zone: ", zone)
  ) %>%
  addLegend("bottomright", pal = pal, values = ~zone, title = "Zone")

trap_map

saveWidget(trap_map, "3_output/trap_map.html")


#NO ZONE 

pal <- colorFactor(palette = "#ff8661ff", domain = trap_data$zone)

# Create the Leaflet map
trap_map = leaflet(trap_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  #addProviderTiles("CartoDB.DarkMatter") %>%
  addCircleMarkers(
    ~long, ~lat,
    color = ~pal(zone),
    popup = ~paste0("Trap ID: ", trap_id, "<br>Zone: ", zone)
  ) 
trap_map

saveWidget(trap_map, "3_output/trap_map.html")



