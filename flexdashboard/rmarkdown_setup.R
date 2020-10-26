crop_revenue <- function(soil_zone, crops_selected = c("Corn", "Canola", "Flax", "Oats", "Soybean")) {
  
  if (soil_zone == "black") { 
    data <- read_csv("data/black.csv")
    return(data %>% View())
  } 
  else if (soil_zone == "brown") {
    data <- read_csv("data/brown.csv")
    data <- read_csv("data/black.csv")
    return(data %>% View()) 
  } 
  else if (soil_zone == "darkbrown") {
    data <- read_csv("data/darkbrown.csv")
    data <- read_csv("data/black.csv")
    return(data %>% View()) 
  }
  
}  


crop_revenue("black")  
