
# function to log transform the data ----
log_transform <- function(df, 
                          cols = c("total_rooms", 
                                   "total_bedrooms", 
                                   "population", 
                                   "households")) {
  # There are 4 extreme skewed variables:
  # total_rooms, total_bedrooms, population, households
  for (col in cols) {
    new_col = paste0("log_", col)
    df[new_col] <- log(df[col])
  }
  
  # Add a column to split median house values in categorical ranges
  min_value <- round(min(df$median_house_value)/1e5, 1)
  max_value <- round(max(df$median_house_value)/1e5, 1)
  df["value_range"] <- as.factor(cut(round(df$median_house_value/1e5, 1), 
                                     include.lowest = TRUE, right = FALSE,
                                     seq(min_value, max_value + 0.5, 0.5)))
  
  return(df)
}