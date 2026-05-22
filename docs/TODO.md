
## Architecture
### desc:
- [X] create unique key for the trap [trap_id, year, week, spp]
- [ ] make all code R functions
- [ ] incorporate all_mosquitoes_combine_pipeline

## All mosquitoes pipeline
- [ ] create a pipeline can combines the all spp pipeline. Use the git tagged commit all_mosq as a starting off point

## combine all available data
- [x] CMC
- [x] Boulder
- [x] VDCI
- [x] supplemental wnv-s_database 
- [ ] no culex status dropped
- [ ] culex_clean_all_spp_plot.png only showing tarsalis and pipiens should still be all species 
- [ ] ensure traps are uppercase for merging with list.
- [ ] CFC 2006 to 2014 is only pool data but looks like a lot of traps are missing.
- [ ] malfunction_trap_wks: 21 should be number of malfunctions and it looks low

## wnv-s_clean()
### desc: current summary isn't the most informative
- [ ] create new summary 


## expand trap
### desc: create an initial database set to fill in data with as an alternative to dealing with duplicates
- [X] create initial database expanded by trap_id for active traps, zone, zone2, method (not expanded--zone,zone2,method are unique to trap),  year, week, and spp with are defined in the config
    - [X] year_start = 2006, year_end = year(Sys.date), week_start = 23, week_end = 37, spp = Tarsalis, Pipiens
- [X] create key using fun_make_key
- [X] left_join on key to pull: "trap_date", "trap_status", "total", "source"   
- [X] use join_cx_sheets.R to complete and fill in the expanded data sheet
- [X] Boulder, ignore active traps and keep all traps because the pools that they submit are inconsistent

## fix trap_status
### desc: formalize and the trap_status so that the only needed spp are the ones specified in the config. Trap_status will provide context and determine 0 vs NA
    - [X] each trap only has Tarsalis or Pipiens for spp
    - [X] trap status provides further detail using spp0 which should be the original mosquito_species from the raw data sheet:
        - [X] "culex" = within a trap spp0 Pipiens OR Tarsalis > 0
        - [X] "other spp" (previously "no culex") =  within a trap ssp0 Pipiens OR Tarsalis == 0 & !spp0 %in% c("Pipiens", "Tarsalis") > 0     
        - [X] "no mosquitoes" = within a trap spp0 Pipiens OR Tarsalis == 0 & !spp0 %in% c("Pipiens", "Tarsalis") == 0
        - [X] "malfunction" trap didn't work totals should be NA. Malfunction determined in initial raw data mosquito_species
        - [X] "no trap" = total for whole zone for that week == 0 then "no trap" and totals should be NA
    - [X] drop spp0 after trap_status