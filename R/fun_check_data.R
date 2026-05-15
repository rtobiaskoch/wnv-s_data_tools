check_data <- function(df, trap, dir, 
                       zones = c("NE", "NW", "SE", "SW", "LV", "BE", "BC"),
                       year_filter = 1:10000,  #any number aka no filter
                       week_filter = 1:52, #any number aka no filter
                       copy_threshold = 500,
                       rn_threshold = 34000
                       ) {
  
  #this function is to be run on cleaned data using wnv_s_clean or some other 
  
  df_name <- deparse(substitute(df))
  
  cat("\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n",
      "\n Checking ", df_name, " with ", nrow(df), " rows and ", ncol(df), "columns.\n",
      "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
  
  #--------------------- C H E C K   C S U _ I D --------------------------------------
  
  if("csu_id" %in% names(df)) {
    if(nrow(get_dupes(df, csu_id)) >0) {
      cat("\n Alert! You have duplicate id's in ", df_name, 
          "\n Double check to ensure this was intentional. \n")
    } else {
      cat("\n No duplicate sample ids that start with CSU|BOU. \n")
    }
    
    # invisible(NULL)
  }#end if csu_id in names
  
  #--------------------- C H E C K  T R A P _ I D --------------------------------------
  
  # Check if trap_id is in the dataframe df 
  if("trap_id" %in% names(df)) {
    
    #check if all trap_ids are in the foco_trap list
    if(all(df$trap_id %in% trap$trap_id)) {
      cat("\nAll traps in df match trap data\n")
    } #end if all traps in df are in trap list 
    
    
    
    # Identify missing traps and count by zone
    missing_traps <- anti_join(trap, df, by = "trap_id") %>%
      mutate(trap_status = df_name)
    
    write.csv(missing_traps, paste0(dir, "/missing_traps_", df_name, ".csv"))
    
    if(nrow(missing_traps) == 0) {
      cat("\nAll active foco_traps are present in the data\n") } else {
        cat("\nThe following traps are not represented in the data:\n",
            paste(missing_traps$trap_id, sep = "\n"), "\nCheck data in missing traps.\n")
      }
    
    missing_traps %>%
      group_by(zone) %>%
      count()
  } #end if trap_id in names
  
  #--------------------- C H E C K  Z O N E --------------------------------------
  if("zone" %in% names(df)) {
    
    zones = paste(zones, collapse = "|")
    
    if(!any(str_detect(df$zone, zones))){
      stop(paste0("Column zone contains values that aren't ", zones, " in ", df_name))
    } else {
      cat("\nZone column in ", df_name, " matches ", zones, "\n")
    }# end if zones are missing 
    
  } #end if zone in names
  
  #--------------------- C H E C K    M E T H O D --------------------------------------
  if("method" %in% names(df)){
   
    if(any(is.na(df$method))){
      stop("\nthere are unknown trap methods in ", df_name, "\n")
    } else{
      cat("no NA method")
    }  #end if NA method
    
    if(!all(str_detect(df$method, "G|L"))) {
      
      stop(paste0("\n Some methods aren't L or G in ", df_name, "\n"))
    } else{
      cat("\nAll method G|L\n")
    } #end if L|G not detected
  } #end if method in names
  
  #-------------------- C H E C K   T O T A L   -------------------------
  
  if("total" %in% names(df)){
    
   if(!is.numeric(df$total)) {
    stop(paste0("\nThe total in ", df_name, " is not numeric\n"))
  } #end if total numeric
    
} #end if total in df
  
  #-------------------- C H E C K   D A T E   -------------------------
  
  if("trap_date" %in% names(df)){
    
    if(!is.Date(df$trap_date)) { 
      stop(paste0("\nThe trap_date in ", df_name, " is not a Date\n"))
    } #end if total numeric
    
  } #end if total in df
  
  
  #--------------------- C H E C K    Y E A R  --------------------------------------
  # Validate year if year is in your data frame
  if ("year" %in% names(df)) {
    if(!all(as.numeric(df$year) %in% year_filter)) {
      stop("The year in", df_name, " doesn't match the config year filter")
    } else { 
      cat("\nThe year in ", df_name, "matches the year filter. \n")
      
    } #end if year matches year_filter

  } #end if year in names
  
  
  #--------------------- C H E C K    W E E K  --------------------------------------
  # Validate week if week is in your data frame
  if ("week" %in% names(df)) {
    if(!all(as.numeric(df$week) %in% week_filter)) {
      stop("The week in", df_name, " doesn't match the config week filter")
    } else { 
      cat("\nThe week in ", df_name, "matches the week filter \n")
      
    } #end if week matches week_filter
    
  }#end if week in names
  
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  #PCR DATA 
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  #--------------------- C H E C K    P L A T E  --------------------------------------
  # Validate week if week is in your dataframe
  if ("plate" %in% names(df)) {
    if(any(is.na(df$plate))) {
      stop("\nThe plate number is missing in ", df_name, "\n")
    } #end is NA plate

  } #end check plate
  
  
  
  #------------------ C H E C K   C T (Rn)  T H R E S H O L D  ---------------------------
  # Validate week if week is in your dataframe
  if ("ct_threshold" %in% names(df)) {
    if(any(df$ct_threshold < rn_threshold)) {
      stop("\nThe Ct threshold was not set to ", rn_threshold, "\n")
    } else {
      cat("\nThe Ct Threshold (rn_threshold) is set to ", rn_threshold, 
           ". Nice.\n")} #end is ct_threshold < rn_threshold
    
  } #end check ct threshold
  
  
  #---------------- C H E C K   C O N T R O L S  ----------------------

  if("csu_id" %in% names(df) & "copies_WNV" %in% names(df)) {
    
    is_pcr = df %>% 
      filter(grepl("pos|neg", csu_id, ignore.case = T))
    
    #checking if this is even pcr data
    if(nrow(is_pcr) > 0) {
      
      pos = df %>% 
        filter(grepl("pos", csu_id, ignore.case = T))
      
      if(any(pos$copies_WNV < copy_threshold|pos$copies_SLEV < copy_threshold)) {
        warning(cat("\n One of your positive extraction controls have < ",copy_threshold, " copies. \n"))
      } else {
        cat("\nPositive controls are above threshold\n")
      } #end positive check
      
      neg = cq_data %>% 
        filter(grepl("neg", csu_id, ignore.case = T))
      
      if(any(neg$copies_WNV > copy_threshold|neg$copies_SLEV > copy_threshold)) {
        warning(cat("\n One of your negative extraction controls have < ",copy_threshold, " copies. \n"))
      } else {
        cat("\nNegative controls are below threshold\n")
      } #end negative check
      
    } #end if is_pcr
    

  } #end if csu_id is in names

  #---------------- C H E C K   S A M P L E _ T Y P E  ----------------------
  if("sample_type" %in% names(df)) {
    if(nrow(df %>% filter(sample_type == "undefined"))) {
      stop("you have undefined sample types in your data")
  } #end 
  }
} #end function
  

