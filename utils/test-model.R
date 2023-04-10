
library(psych)
source("./utils/load-model.R")

# function to save test set's MAPE results ----
evaluate <- function(models_path = "./output/model/", 
                     df_path = "./input/test/test.csv", 
                     result_path = "./output/evaluation/test_evaluation.csv",
                     dir = NA) {
  
  setwd(dir)
  
  models <- load_model(path = models_path, dir = dir)
  df <- read.csv(df_path)
  clusters <- unique(df$cluster)
  actual_values <- c()
  predicted.values <- c()
  mapes <- c()
  cluster_names <- c()
  for (pos in 1:length(clusters)) {
    cluster <- clusters[[pos]]
    model <- models[[cluster]]
    cluster_predicted <- predict(model, newdata = df[df$cluster == cluster,])
    cluster_mapes <- (cluster_predicted - 
                        df[df$cluster == cluster,]$median_house_value)/df[
                          df$cluster == cluster,]$median_house_value
    actual_values <- c(actual_values, df[df$cluster == cluster,]$median_house_value)
    predicted.values <- c(predicted.values, cluster_predicted)
    mapes <- c(mapes, cluster_mapes)
    cluster_names <- c(cluster_names, df[df$cluster == cluster,]$cluster)
  }
  result <- data.frame(
    actual = actual_values,
    predicted = predicted.values,
    mape = mapes,
    cluster = cluster_names
  )
  
  write.csv(result, result_path, row.names = FALSE)
}

# function to calculate overall model MAPE by combining cluster's MAPEs ----
avg.MAPE <- function(result_path = "./output/evaluation/test_evaluation.csv", 
                     dir = NA) {
  
  setwd(dir)
  
  test_result <- read.csv(result_path)
  avg_mape <- sum(abs(test_result$mape))/nrow(test_result)
  
  return(avg_mape)
}
