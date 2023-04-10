
# function to load trained models parameters ----
load_model <- function(path = "./output/model/", dir = NA) {
  
  setwd(dir)
  
  models <- list()
  files <- list.files(path)
  for (pos in 1:length(files)) {
    model <- readRDS(paste0(path, files[pos]))
    models[[pos]] <- model
  }
  names(models) <- sapply(files, FUN = function(x) {substr(x, 1, nchar(x) - 4)})
  
  return(models)
}
