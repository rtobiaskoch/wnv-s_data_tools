read_latest = function(path) {
  fn = list.files(path, full.names = TRUE) %>%
  file.info() %>%
  {.[order(.$mtime, decreasing = TRUE), ]} %>%
  rownames() %>%
  .[1]
  
  ext = tools::file_ext(fn)
  
  if(ext == "RData") {
    list2env(readRDS(fn), #fix config
             envir = .GlobalEnv)
  }

  if(ext == "csv|xlsx|xls") {
    rio::import(fn)
  }
  
}