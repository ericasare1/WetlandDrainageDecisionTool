library(tidyverse)

blackzone <- read.csv("data/black.csv")

blackzone1 <- blackzone %>% select (-c(soil_zone)) 
blackzone2 <- blackzone %>% select (c(crop, yield.bu.acre., soil_zone ))

blackzone2[crop == "Corn", soil_zone == "Black"]

rownames(blackzone2) <- blackzone2$crop

blackzone2["Canola", "yield.bu.acre."]
