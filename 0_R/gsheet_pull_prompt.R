fun_gsheet_pull_prompt = function(filename, sheet, key) {
  
  #first check if user has drive token. They don't have them provide one
  if(!drive_has_token()) {
    googledrive::drive_auth()
  }
  
  user_input <- readline(prompt = paste0("Would you like to download/update the file ", filename, " from Google Drive? (yes/no): "))
  
  # Check the user's input
  if (tolower(user_input) == "yes") {
    # Run the script if the user says "yes"
    
    require(googlesheets4)
    require(googledrive)
    
    
    #remove old file if it exists
    if(file.exists(filename)) { file.remove(filename) }
    
    #READ IN DATA
    mdata = read_sheet(key, sheet = sheet)
    mdata <- mdata %>%
      mutate(across(where(is.list), ~ sapply(., paste, collapse = ", ")))
    #mdata = apply(mdata, 2, as.character) #added because error   unimplemented type 'list' in 'EncodeElement' was occuring
    write.csv(mdata, filename, row.names = F, na = "")
    read.csv(filename)
    print("...Checking that gdrive download worked...")
    
    #if file exists process else stop function
    if(file.exists(filename)) {cat(filename, " gdrive downloaded successfully.\n") } else {
      stop(cat(filename, " file doesn't exist. Please rerun function or download manually. \n")) } #end if file check
    
  } #end user_input yes
  
  #If user_input is NO
  if (tolower(user_input) == "no") {
    cat("File", filename, " gdrive download skipped. \n")
    #if user_input no and file exists
    if(file.exists(filename)) {cat(paste0(filename, " modified ", as.Date.POSIXct(file.info(filename)$mtime), " will be used."))
      
    } else {     #if user_input no and file doesn't exist
      cat(filename, " file doesn't exist. Please rerun function or download manually. \n")
      stop()
    } 
  } #end user_input no
  
  #If user input is invalid
  if (!(tolower(user_input) %in% c("no", "yes"))) {
    cat("Please input a valid response.")
  }
  
  rm(user_input)
} #end of function
