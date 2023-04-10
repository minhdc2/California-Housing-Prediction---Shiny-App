
library(DT)

try_examples <- function(long, lat, size,
                         df_path = "./input/cluster/cluster.csv",
                         dir = NA) {
  setwd(dir)
  df <- read.csv(df_path)
  rows <- sample(row.names(unique(df[, c("longitude", "latitude")])), 
                 size = size)
  try_table <- df[rows, c("longitude", "latitude")]
  
  return(try_table)
}