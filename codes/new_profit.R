
library(tidyverse)

#Reading in Data for specific soil zoils
crop_revenue <- function(soil_zone, crops_selected = c("Corn", "Canola", "Flax", "Oats", "Soybean")) {
  
  if (soil_zone == "black") { 
    data <- read_csv("data/black.csv")
    data1 <- data %>% dplyr::filter(crop %in% crops_selected) %>%
      mutate(revenue = `yield(bu/acre)` * `price_fg ($/bu)`,
             cost = 2,
             net_profit = revenue - cost) %>% select(revenue, cost, net_profit) 
    return(data1 %>% View())
  } 
  else if (soil_zone == "brown") {
    data <- read_csv("data/brown.csv")
    data1 <- data %>% dplyr::filter(crop %in% crops_selected) %>%
      mutate(revenue = `yield(bu/acre)` * `price_fg ($/bu)`,
             cost = 2,
             net_profit = revenue - cost) %>% select(revenue, cost, net_profit)
    return(data1 %>% View())
  } 
  else if (soil_zone == "darkbrown") {
    data <- read_csv("data/darkbrown.csv")
    data1 <- data %>% dplyr::filter(crop %in% crops_selected) %>%
      mutate(revenue = `yield(bu/acre)` * `price_fg ($/bu)`,
             cost = 2,
             net_profit = revenue - cost) %>% select(revenue, cost, net_profit)
    return(data1 %>% View())
  }
  
}  


crop_revenue("brown", crops_selected = c("Corn", "Canola"))  
