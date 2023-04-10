
library(psych)
library(dplyr)

# function to calculate Pearson correlation between independent and dependent variable ----
pearson_corr <- function(long, lat, 
                         df_path = "./input/cluster/cluster.csv",
                         dir = NA) {
  
  setwd(dir)
  
  f <- function(df) {
    cols <- c("median_house_value", "longitude", "latitude", 
              "housing_median_age", "log_total_rooms", 
              "log_total_bedrooms", "log_population", 
              "log_households", "median_income")
    corr_df <- round(as.data.frame(corr.test(df[, cols], 
                                             method = "pearson")$r)["median_house_value"] %>%
                       arrange(desc(abs(median_house_value))), 4)
    corr_df['variables'] <- rownames(corr_df)
    return(corr_df[, c("variables", "median_house_value")])
  }
  
  df <- read.csv(df_path)
  if ((long != "") & (lat != "")) {
    cluster_name <- unique(df[((df$longitude == as.numeric(long)) & (df$latitude == as.numeric(lat))),]$cluster)[1]
    corr_df <- f(df[(df$cluster == cluster_name),])
  }
  else {
    corr_df <- f(df)
  }
  
  return(corr_df)
}
