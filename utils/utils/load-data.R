
library(readr)

# function to download the raw data ----
load_data <- function(url = "https://github.com/ageron/handson-ml2/raw/master/datasets/housing/housing.csv",
                      path = "./input/raw/data.csv",
                      dir = NA) {
  setwd(dir)
  df <- read_csv(url(url), show_col_types = FALSE)
  write.csv(x = df, file = path, row.names = FALSE)
  
  return(df)
}
