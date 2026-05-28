CFC = readxl::read_excel(
  "1_input/culex_sheet/CFC-2006-2017/CFC_Full_Raw_Data_2006-2017.xlsx",
  sheet = "Master Raw"
)

rename = read.csv("1_input/database_column_rename.csv")

t = CFC %>%
  key_rename(rename) %>%
  mutate(trap_id = str_to_upper(trap_id)) %>%
  group_by(trap_id) %>%
  count()

t %>% anti_join(trap_keep_df)


all_t = purrr::map_df(
  sources_prepped,
  ~ .x %>%
    group_by(trap_id) %>%
    count()
)


