update_gsheet = function(new, #new data to add
                         old = NULL, #old data that you want to update
                         type = "FULL", 
                         by, #joining keys
                         fn_save, #filename that you want to save locally
                         gkey,  #google sheet key of file you want to update
                         gfolder, #folder name in google drive save copy
                         gname, #name of file to cp before updating
                         sheet = "data",
                         col_database = c( "csu_id", "trap_id", "zone", "year", "week", "trap_date","method", "spp",
                                           "total", "test_code", "cq", "copies_WNV", "seq", "lat", "long")
                         ) {

#if old isn't supplied load from google
if(is.null(old)) {
  old = googlesheets4::read_sheet(ss = gkey, 
                            sheet = sheet)  
}
  
#update data 
update = rquery::natural_join(
  new, old, 
  jointype = type, 
  by = by) %>% #add any factors that can be joined otherwise they are converted to numbers becuase natural_join sucks
  select(all_of(col_database)) #reorder them because natural_join fucked up the order

#if trap_date is present convert it back to date after mutate everything changed it to a number
if("trap_date" %in% names(update)) {
  update = update %>%
  mutate(trap_date = as_date(as_datetime(trap_date))) %>%
  arrange(desc(trap_date))
}

#save local copy
write.csv(update, fn_save, row.names = F)
  
  #make a copy of old standards key 
  drive_cp(file = as_id(gkey), 
           path = gfolder,
           name = paste0(gname, Sys.time()),
           overwrite = T)
  
  #save it to gdrive
  options(googlesheets4.na = "")  # Write NA as blank cells
  googlesheets4::sheet_write(update,
                             ss = gkey,
                             sheet = sheet
  )
}

