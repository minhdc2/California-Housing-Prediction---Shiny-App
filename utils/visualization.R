
library(ggplot2)
library(gridExtra)
library(dplyr)
library(maps)

load.CA.counties <- function(reg="california") {
  counties <- map_data("county")
  ca_county <- subset(counties, region == reg)
  return(ca_county)
}

vizOnMap <- function(long,
                     lat,
                     source_df = "./input/cluster/cluster.csv",
                     country = "state",
                     reg = "california",
                     dir = NA) {
  
  setwd(dir)
  df <- read.csv(source_df)
  pop <- 0
  belong_subregion <- NA
  value_class <- NA
  ca_county <- load.CA.counties()
  ca_county['fill_color'] <- "all"
  if ((long != "") & (lat != "")) {
    belong_subregion <- belongedSubregion(as.numeric(long), as.numeric(lat), ca_county)
    ca_county['fill_color'] <- sapply(ca_county$subregion, FUN = function(x) {
      ifelse(x == belong_subregion, "target", "other")
    })
    pop <- sum(df[((df$longitude == as.numeric(long)) & (df$lat == as.numeric(lat))),]$population)
    value_class <- unique(df[((df$longitude == as.numeric(long)) & (df$lat == as.numeric(lat))),]$value_range)
  }
  
  states <- map_data(country)
  ca_df <- subset(states, region == reg)
  fills <- unique(ca_county$fill_color)
  ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) + 
    geom_polygon(color = "black", fill = NA) +
    geom_polygon(data = ca_county, aes(fill = fill_color), color = "white") +
    scale_fill_manual(values = c("snow3", "orange")) +
    geom_polygon(color = "snow3", fill = NA) +  # get the state border back on top
    
    geom_point(data = df, 
               mapping = aes(x = longitude, 
                             y = latitude, 
                             group = NA, 
                             color = cluster,
                             size = population), 
               alpha = 0.3) +
    ggtitle("Navigation of your searched location on map") +
    ylab("Latitude") +
    xlab("Longitude") +
    theme(plot.background=element_rect(fill=NA, color=NA),
          panel.background=element_rect(fill=NA, color=NA),
          panel.grid.major=element_line(color = "snow2"),
          panel.grid.minor=element_blank(),
          plot.title = element_text(color="grey30", size=16, face="bold", hjust = 0),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(color="grey60", size=14, face="bold"),
          axis.title.x = element_text(color="grey60", size=14, face="bold"),
          legend.title = element_blank(),
          legend.text=element_text(size=11, color = "black"),
          legend.position="right",
          plot.margin = margin(0,0,0,0, "cm")) +
    guides(colour = guide_legend(nrow = 10, ncol = 3),
           size = guide_legend(nrow = 3))
  
  agg_df <- df %>% group_by(value_range) %>% 
    summarise(cnt = n())
  p_bar <- ggplot(data = agg_df, aes(x = value_range, y = cnt)) +
    geom_bar(stat="identity", fill = "dodgerblue", alpha = 0.5) +
    ggtitle("Observed housing value range of your searched location") +
    ylab("Number of blocks") +
    xlab("Value Range (measured in 100,000 US Dollars)") +
    theme(plot.background=element_rect(fill=NA, color=NA),
          panel.background=element_rect(fill=NA, color=NA),
          panel.grid.major.y=element_line(color = "snow2"),
          panel.grid.minor=element_blank(),
          plot.title = element_text(color="grey30", size=16, face="bold", hjust = 0),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(color="grey60", size=14, face="bold"),
          axis.title.x = element_text(color="grey60", size=14, face="bold"),
          plot.margin = margin(2,.5,2,.5, "cm"))
  
  if ((long != "") & (lat != "") & (pop != 0) & (!is.na(belong_subregion))) {
    sub_df <- data.frame(
      longitude <- c(as.numeric(long)),
      latitude <- c(as.numeric(lat)),
      population <- c(pop),
      belong_subregion <- c(belong_subregion)
    )
    ca_base <- ca_base + geom_point(data = sub_df,
                                    aes(x = longitude,
                                        y = latitude,
                                        group = NA,
                                        size = population),
                                    color = "red3") +
      annotate("text", x = as.numeric(long), y = as.numeric(lat) + 0.3, label = belong_subregion,
                color='red4', size = 5, fontface = "bold")
    
    sub_agg_df <- agg_df[agg_df$value_range %in% value_class,]
    p_bar <- p_bar + geom_bar(data = sub_agg_df, 
                              aes(x = value_range, y = cnt),
                              stat="identity",
                              fill = "dodgerblue4", alpha = 0.5)
  }
  
  grid.arrange(ca_base, p_bar, nrow = 1, ncol = 2)
}


vizMAPEs <- function(result_path = "./output/evaluation/test_evaluation.csv",
                     dir = NA) {
  setwd(dir)
  test_result <- read.csv(result_path)
  avg_mape <- sum(abs(test_result$mape))/nrow(test_result)
  
  p1 <- ggplot(data = test_result, aes(x = actual, y = mape, color = cluster)) +
    geom_point(alpha = 0.3, size = 2) +
    geom_hline(yintercept = avg_mape, linetype = 'dashed', color = "brown") +
    geom_hline(yintercept = -1*avg_mape, linetype = 'dashed', color = "brown") +
    ylim(-6, 6) +
    ggtitle("Observation's MAPE per cluster") +
    ylab("MAPE") +
    xlab("Actual values") +
    theme(panel.background=element_rect(fill=NA, color=NA),
          panel.grid.major.x=element_line(color = "snow2"),
          panel.grid.major.y=element_line(color = "snow2"),
          legend.text=element_text(size=11, color = "black"),
          legend.position="top",
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_text(color="grey60", size=14, face="bold"),
          axis.title.x = element_text(color="grey60", size=14, face="bold"),
          plot.title = element_text(color="grey30", size=16, face="bold", hjust = 0),)
  
  grid.arrange(p1, nrow=1, ncol=1)
}


