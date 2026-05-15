library(tidyverse)
library(janitor)

d = read.csv("3_output/culex_sheet_database_augment.csv")

# check for only 1 spp
spp1 = d %>%
  group_by(trap_id, year, week) %>%
  summarize(n = n_distinct(spp)) %>%
  filter(n == 1)


# get dupes by 
dedup = d %>% get_dupes(trap_id, year, week, spp)

dedup %>%
  group_by(year) %>%
  count

# dup example
t = d %>%
  filter(trap_id %in% c("FC-039", "FC-031") & year == 2023 & week %in% c(29, 27))

  #merging by trap_date problem seesm fixed when deduping by week instead


  #THIS ONE IS FUCKED MAY BE AN ISSUE OF MISMATCHING ZONES
  t = d %>%
  filter(trap_id %in% c("FC-049") & year == 2019 & week %in% c(36))
