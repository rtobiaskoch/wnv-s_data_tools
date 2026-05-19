library(tidyverse)
library(janitor)

d = read.csv("3_output/culex_sheet_database_augment.csv")


#get start year and weeks for zones ignoring variability by trap
m = d %>%
  group_by(zone) %>%
  summarize(
    start_year = min(year),
    start_week = min(week),
    end_week = max(week)
  )


write.csv(m, "sandbox/zone_trap_start_end.csv", row.names = FALSE)
