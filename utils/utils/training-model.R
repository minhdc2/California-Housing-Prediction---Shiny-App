

# function to fit linear regresion with fixed set of variables ----
fit_linear_model <- function(cluster_train) {
  model <- lm(median_house_value ~ longitude + latitude + 
                housing_median_age + log_total_rooms + log_total_bedrooms + 
                log_population + log_households + median_income, data=cluster_train)
  return(model)
}

# function to fit linear model to given clusters ----
fit <- function(train, col = "cluster",
                model_path = "./output/model/",
                dir = NA) {
  setwd(dir)
  clusters <- unique(train[[col]])
  files <- list.files(model_path)
  file.remove(paste0(model_path, files))
  for (pos in 1:length(clusters)) {
    cluster_train = train[train[[col]] == clusters[pos],]
    model <- fit_linear_model(cluster_train)
    file <- paste0(model_path, clusters[pos], ".rda")
    saveRDS(model, file = file)
  }
}
