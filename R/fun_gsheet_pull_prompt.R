#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# -------------------- D E F I N E   G S H E E T _ P U L L  ----------------------
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
require(googlesheets4)
require(googledrive)
require(dplyr)


gsheet_pull_prompt = function(filename, sheet = "Sheet1", key) {
  
  #check if filename ends in csv
  if (!grepl("\\.csv$", filename)) {
    stop("Filename must end in .csv")
  }
  
  # #check if input directory exists if not create it
  # if(!dir.exists(dir)) {
  #   cat("\n", dir, "directory doesn't exist. Creating directory...\n")
  #   dir.create(dir)
  # }

  
  
  #If file exists ask user if they want to replace it
  if(file.exists(filename)){
    user_input <- readline(prompt = paste0(filename, " exists. Would you like to replace it from Google Drive? (y/n): "))
  } else {
      user_input = "y"
    }

  
  # Check the user's input
  if (tolower(user_input) == "y") {
    # Run the script if the user says "yes"
    
    
    #if drive doesn't have a token then have user authorize
    if(!googledrive::drive_has_token()) {
      googledrive::drive_auth()
    }
    
    mdata = googlesheets4::read_sheet(key, sheet = sheet)
    
    cat("\nRead googlesheet with ", nrow(mdata), "rows and ", ncol(mdata), 
        "columns and following headers \n", paste(names(mdata), sep = ","), "\n")
    
    mdata <- mdata %>%
      mutate(across(where(is.list), ~ sapply(., paste, collapse = ", ")))
     #mdata = apply(mdata, 2, as.character) #added because error   unimplemented type 'list' in 'EncodeElement' was occuring
      write.csv(mdata, filename, row.names = F, na = "")
      
      #read.csv(filename)
    
    print("\n...Checking that gdrive download worked...\n")
    
  } else { #end of user_input yes update
    cat("\nFile", filename, " download skipped.\n")
  }
  
  if(!file.exists(filename)) {
    stop(cat(filename, " file doesn't exist. Please reauthenticate googledrive check inputs and rerun. \n"))
  } else {
    cat(filename, " exists.\n")
  }
  
  rm(user_input)
} #end of function
