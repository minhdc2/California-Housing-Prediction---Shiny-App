
source("./utils/load-model.R")
library(stringr)

# function to perform house prices prediction ----
do.Predict <- function(long, lat, source_path = "./input/cluster/cluster.csv", 
                       models_path = "./output/model/",
                       dir = NA) {
  setwd(dir)
  
  if (long == "") {cat("Longitude is missing! Please input.\n")}
  if (lat == "") {cat("Latitude is missing! Please input.\n")}
  if ((long != "") & (lat != "")) {
    long <- as.numeric(long)
    lat <- as.numeric(lat)
    source <- read.csv(source_path)
    models <- load_model(path = models_path, dir = dir)
    filtered <- source[source$longitude == long,]
    filtered <- filtered[filtered$latitude == lat,]
    if (nrow(filtered) == 0) {
      cat("Information was invalid. Possible reasons:\n
          (1) The information did not exist in my database.\n
          (2) Due to quality concerns, the information was excluded.")
    }
    else {
      model <- unique(filtered$cluster)[1]
      predicted.value <- predict(object = models[[model]], newdata = filtered)
      actual.value <- filtered$median_house_value
      if (length(predicted.value) > 1) {
        min_predicted_value <- round(min(predicted.value), 2)
        max_predicted_value <- round(max(predicted.value), 2)
        min_actual_value <- round(min(actual.value), 2)
        max_actual_value <- round(max(actual.value), 2)
        cat(paste0("The predicted median house value ranges from: (", 
                   as.character(min_predicted_value), ", ", 
                   as.character(max_predicted_value), ") USD."))
        cat(paste0("\nWhile, the recorded median house value ranges from: (", 
                   as.character(min_actual_value), ", ", 
                   as.character(max_actual_value), ") USD."))
        goods <- which(actual.value < predicted.value)
        bads <- which(actual.value > predicted.value)
        if (all(length(goods) > 0, length(bads) > 0)) {
          cat("\n=> Recommendations:\nBlocks",
              paste0(bads, collapse = ","),
              "are overvalued, please research your house carefully to save your money. However, Blocks",
              paste0(goods, collapse = ","),
              "are undervalued, you can find profitable deals here.")
        }
        else if (length(goods) == 0) {
          cat("\n=> Recommendations:\nAll Blocks are overvalued, please research your house carefully to save your money.")
        }
        else if (length(bads) == 0) {
          cat("\n=> Recommendations:\nAll Blocks are undervalued, you can find profitable deals here.")
        }
      }
      else {
        cat(paste0("The predicted median house value in this area is: ", 
                   as.character(round(predicted.value, 2)), " USD."))
        cat(paste0("\nWhile, the recorded median house value in this area is: ", 
                   as.character(round(actual.value, 2)), " USD."))
        if (actual.value < predicted.value) {
          cat("\n=> Recommendations:\nHouse prices in this area are undervalued, you can find profitable deals here.")
        } 
        else if (actual.value > predicted.value) {
          cat("\n=> Recommendations:\nHouse prices in this area are overvalued, please research your house carefully to save your money.")
        }
      }
      cat("\n\nOther information can be viewed in Summary tab.")
    }
  }
}
