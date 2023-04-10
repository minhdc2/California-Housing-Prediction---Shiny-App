

# function to extract information summary of looked-up location ----
detailed_info <- function(long, lat, 
                          df_path = "./input/cluster/cluster.csv",
                          dir = NA) {
  
  setwd(dir)
  
  df <- read.csv(df_path)
  if ((long != "") & (lat != "")){
    cols <- c("median_house_value", "cluster", "median_income", 
              "housing_median_age", "total_rooms", "population", "households",
              "longitude", "latitude", "ocean_proximity")
    summary <- df[((df$longitude == as.numeric(long)) & (df$latitude == as.numeric(lat))), cols]
    summary <- as.data.frame(t(summary))
    block_names <- paste0("Block ", 1:ncol(summary))
    colnames(summary) <- block_names
    summary[["attribute"]] <- rownames(summary)
    summary <- summary[, c("attribute", block_names)]
  } else {
    summary <- data.frame(info = c("You must input longitude and latitude to show the table!"))
  }
  
  return(summary)
}