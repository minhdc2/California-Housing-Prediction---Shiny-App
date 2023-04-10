
# function to split train and test sets ----
split <- function(df, test_size = 0.3, 
                  train_path = "./input/train/train.csv",
                  test_path = "./input/test/test.csv",
                  dir = NA) {
  setwd(dir)
  # Train, test splits with test size = 0.3
  sample <- sample(c(TRUE, FALSE), nrow(df), 
                   replace=TRUE, prob=c(1-test_size, test_size))
  train  <- df[sample, ]
  test   <- df[!sample, ]
  write.csv(x = train, file = train_path, row.names = FALSE)
  write.csv(x = test, file = test_path, row.names = FALSE)
  
  return(list(train, test))
}