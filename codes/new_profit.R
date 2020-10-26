
library(tidyverse)

data <- read_csv("data/black.csv")
data %>% dplyr::filter(crop %in% crops_selected) %>%
  mutate(revenue = `yield(bu/acre)` * `price_fg ($/bu)`) %>% select(revenue)


rev = data$`yield(bu/acre)`*data$`price_fg ($/bu)`

column_to_rownames(data, var = "crop") %>% head()

crops_selected <- c("Canola", "Corn")
out <- data %>% dplyr::filter(crop %in% crops_selected) %>% View()

data[[(crops), "yield(bu/acre)"]]

#Reading in Data for specific soil zoils

data <- read_csv("data/brown.csv")
data1 <- data %>% dplyr::filter(crop %in% c("Corn", "Canola", "Flax", "Oats", "Soybean")) %>%
  mutate(revenue = `yield(bu/acre)` * `price_fg ($/bu)`) %>% select(revenue)

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



crop_revenue("black", crops_selected = c("Corn", "Canola", "Flax"))  
