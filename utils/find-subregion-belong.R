
library(dplyr)

# function to find the region that looked-up location belongs to ----
belongedSubregion <- function(long, lat, ca_county, 
                              one_degr_long=1,
                              one_degr_lat=1) {
  
  ca_county["dist"] = sqrt(((ca_county$long - long)*one_degr_long)^2 + (
    (ca_county$lat - lat)*one_degr_lat)^2)
  sorted <- ca_county %>% arrange(dist)
  belong_subregion <- sorted$subregion[1]
  
  return(belong_subregion)
}

