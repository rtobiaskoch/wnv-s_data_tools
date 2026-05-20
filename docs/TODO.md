
## Architecture
### desc:
- [X] create unique key for the trap [trap_id, year, week, spp]
- [ ] make all code R functions


## combine all available data
- [ ] CMC
- [ ] Boulder
- [ ] VDCI
- [ ] supplemental wnv-s_database 


## expand trap
### desc: create an initial database set to fill in data with as an alternative to dealing with duplicates
- [ ] create initial database expanded by trap_id for active traps, year, week, and spp with are defined in the config file
    - [ ] year_start = 2006, year_end = year(Sys.date), week_start = 23, week_end = 37, spp = Tarsalis, Pipiens
- [ ] Boulder, ignore active traps and keep all traps because the pools that they submit are inconsistent

## fix trap_status
### desc: formalize and the trap_status so that the only needed spp are the ones specified in the config. Trap_status will provide context and determine 0 vs NA
    - [ ] each trap only has Tarsalis or Pipiens for spp
    - [ ] trap status provides further detail using spp0 which should be the original mosquito_species from the raw data sheet:
        - [ ] "culex" = within a trap spp0 Pipiens OR Tarsalis > 0
        - [ ] "no culex" =  within a trap ssp0 Pipiens OR Tarsalis == 0 & !spp0 %in% c("Pipiens", "Tarsalis") > 0     
        - [ ] "no mosquitoes" = within a trap spp0 Pipiens OR Tarsalis == 0 & !spp0 %in% c("Pipiens", "Tarsalis") == 0
        - [ ] "malfunction" trap didn't work totals should be NA. Malfunction determined in initial raw data mosquito_species
        - [ ] "no trap" = total for whole zone for that week == 0 then "no trap" and totals should be NA
    - [ ] drop spp0 after trap_status