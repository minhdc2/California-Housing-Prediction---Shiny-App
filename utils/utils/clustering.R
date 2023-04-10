
library(factoextra)

# function to cluster the data ----
cluster <- function(df, cluster_by = NA,
                    no_clusters = NA,
                    dir = NA,
                    path = "./input/cluster/cluster.csv") {
  
  setwd(dir)
  
  f <- function(val, unq_val) {
    for (pos in 1:length(unq_val)) {
      if (val == unq_val[pos]) {
        cluster_name = paste0("area_", pos)
        break
      }
    }
    return(cluster_name)
  }
  
  df[['cluster']] <- NA
  
  if (cluster_by == "no cluster") {
    df[['cluster']] <- "area_0" 
  }
  
  if (cluster_by == "ocean_proximity") {
    unq_val <- unique(df[[cluster_by]])
    df['cluster'] <- sapply(df[[cluster_by]], FUN = f, unq_val = unq_val)
  }
  
  if (cluster_by == "longitude & latitude (using KMeans)") {
    set.seed(0762062)
    km.res <- kmeans(df[, c("longitude", "latitude")], 
                     centers = as.numeric(no_clusters), nstart = 25)
    df['cluster'] <- paste0("area_", km.res$cluster)
  }
  write.csv(x = df, file = path, row.names = FALSE)
  
  return(df)
}
