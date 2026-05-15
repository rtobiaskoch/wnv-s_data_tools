#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#------------------ M E R G E  P C R  & P L A T E M A P --------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

merge_pcr_platemap = function(pcr, platemap, copy_threshold = 500, by = c("well_position", "year", "week", "plate")) {
  left_join(pcr, platemap,
            by = by)  %>%
    filter(!is.na(csu_id)) %>% # remove wells with no matches
    arrange(year, week, plate)
}