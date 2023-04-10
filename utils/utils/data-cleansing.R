
# function to cleanse the data ----
cleansing <- function(df) {
  
  df <- df[is.na(df$total_bedrooms) == F,]
  df <- df[df$ocean_proximity != 'ISLAND',]
  df <- df[df$median_house_value < 500001,]
  
  return(df)
}
