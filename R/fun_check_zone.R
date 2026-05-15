check_zone = function(expected_n, calc_n) {

expected_zone_n = expected_n %>%
  filter(method == "L") %>%
  filter(zone %in% grp_zones) %>%
  group_by(zone) %>%
  count() %>%
  arrange(zone)

new_zone_n = calc_n %>%
  filter(method == "L") %>%
  filter(trap_status != "missing") %>%
  filter(zone %in% grp_zones) %>%
  distinct(zone, trap_id) %>%
  group_by(zone) %>%
  count() %>% 
  arrange(zone)


check = full_join(expected_zone_n, new_zone_n, by = "zone") %>% 
mutate(check = n.x == n.y)

if(any(check$check)) {
  cat("Warning some of your zones expected light trap numbers do not match your new data. check your check_zones data frame.")
}

return(check)

}

