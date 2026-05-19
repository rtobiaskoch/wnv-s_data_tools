library(tidyverse)
library(janitor)

d = read.csv("3_output/culex_sheet_database_augment.csv")


t = d %>%
  distinct(trap_id, zone, year, week, spp) %>%
  group_by(trap_id, zone) %>%
  count

get_dupes(t, trap_id)


fc049 = d %>%
  filter(trap_id == "FC-049")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# check trap map
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(leaflet)

trap = read.csv("1_input/foco_trap - data.csv")
source("sandbox/plot_map.R")
plot_map(df = trap, id = "trap_id", color = "zone")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#check zone count from culex clean
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
foco_traps0 = read.csv("1_input/foco_trap - data.csv")

check_zones = function(df, trap) {
  trap = trap %>%
    filter(active == 1)

  df = df %>%
    group_by(trap_id, zone) %>%
    count()
  check = anti_join(trap, df)
  check
}


culex_clean = read.csv("3_output/trap_database_all_spp.csv")

check_zones(culex_clean, foco_traps0)

database_reformat = read.csv("2_mid/database_reformat.csv")

check_zones(database_reformat, foco_traps0)
