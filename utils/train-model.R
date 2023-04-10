
source("./utils/utils/load-data.R")
source("./utils/utils/data-cleansing.R")
source("./utils/utils/data-transformations.R")
source("./utils/utils/train-test-splits.R")
source("./utils/utils/clustering.R")
source("./utils/utils/training-model.R")
options(scipen=9999)

# function to train model using linear regression per cluster ----
train_model <- function(cluster_by = NA, no_clusters = NA, dir = NA) {
  
  set.seed(0762062)
  setwd(dir)
  
  # Step 1: Download Data
  raw_df <- read.csv("./input/raw/data.csv") # load_data(dir = dir)
  
  # Step 2: Data Cleansing
  # function from "./utils/utils/data-cleansing.R"
  cleaned_df <- cleansing(raw_df)
  
  # Step 3: Data Transformations
  # function from "./utils/utils/data-transformations.R"
  transformed_df <- log_transform(cleaned_df)
  
  # Step 4: Clustering Data
  # function from "./utils/utils/clustering.R"
  clustered_df <- cluster(transformed_df, cluster_by = cluster_by,
                          no_clusters = no_clusters,
                          dir = dir)
  
  # Step 3: Train, Test Splits
  # function from "./utils/utils/train-test-splits.R"
  train_test <- split(clustered_df, test_size = 0.3, dir = dir)
  train <- train_test[[1]]
  test <- train_test[[2]]
  
  # Step 6: Training Model
  # Note: trained models will be exported to 'model' folder at the same directory
  # function from "./utils/utils/training-model.R"
  fit(train, dir = dir)
}





