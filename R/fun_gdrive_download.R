library(googledrive)
library(purrr)

gdrive_download <- function(key, dest_path = ".", overwrite = TRUE) {
  # Authenticate if needed
  if (!googledrive::drive_has_token()) {
    googledrive::drive_auth()
  }
  
  # List files in the folder
  files <- drive_ls(as_id(key))
  
  drive_ls(files[1,2])
  
  # Download each file
  purrr::walk2(
    files$name,
    files$id,
    ~ drive_download(
      as_id(.y),
      path = file.path(dest_path, .x),
      overwrite = overwrite
    )
  )
}

#usage
#gdrive_download("1dSPE3fUSk7yMVSYF4lsZSO7FjFqoxKpr")