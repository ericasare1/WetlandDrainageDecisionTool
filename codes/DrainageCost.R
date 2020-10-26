
ditch_length <- function(wetland_area, num_wetlands) {
  
  library(truncdist)  #package for drawing from truncated dist using rtrunc
  
  error_drainlength <- round(rnorm(1, 0, 1), 2)  # scaled by the standard deviation of the crop?
  
  length <- -0.0523*wetland_area + (-0.0179*num_wetlands) + 0.3204 + error_drainlength
  
  return(length)
}

drainage_cost <- function(ditch_length, construction_time, scraper_length,
                          scraper_ownership){
  if (scraper_length == 6.5 & scraper_ownership == 1 ) {
    cost <- ditch_length * 0.26 * 5.28
    return(cost)
  } else if (scraper_length == 8.5 & scraper_ownership == 1 ) {
    cost <- ditch_length * 0.2 * 7.55
    return(cost)
  } else if (scraper_length == 6.5 & scraper_ownership == 0 ) {
    cost <- ditch_length * 0.26 * 37.01
    return(cost)
  } else if (scraper_length == 8.5 & scraper_ownership == 0 ) {
    cost <- ditch_length * 0.2 * 52.93
    return(cost)
  }
}

Rehabilitation_costs <- function(wetlandacre){
  cost <- 200 * wetlandacre
  return(cost)
}
Maintenance_cost <- function(drainage_cost) {
  cost <- drainage_cost * 1.5
  return(cost)
}

NuisanceCost <- function(num_wland, farm_size, mach_op_cost){
  if (1 < num_wland < 3 & farm_size == 4) {
  cost <- mach_op_cost * 0.08
  return(cost)
  } else if ( 1 < num_wland < 3 & farm_size == 8) {
    cost <- mach_op_cost * 0.09
    return(cost) 
  } else if ( 1 < num_wland < 3 & farm_size == 12) {
    cost <- mach_op_cost * 0.11
    return(cost) 
  } else if ( 1 < num_wland < 3 & farm_size == 16) {
    cost <- mach_op_cost * 0.14
    return(cost)
  } else if (1 < num_wland < 3 & farm_size == 20) {
    cost <- mach_op_cost * 0.18
    return(cost)
  } else if ( 4 < num_wland < 6 & farm_size == 4) {
    cost <- mach_op_cost * 0.09
    return(cost) 
  } else if ( 4 < num_wland < 6 & farm_size == 8) {
    cost <- mach_op_cost * 0.10
    return(cost)
  } else if ( 4 < num_wland < 6 & farm_size == 12) {
    cost <- mach_op_cost * 0.12
    return(cost)
  } else if ( 4 < num_wland < 6 & farm_size == 16) {
    cost <- mach_op_cost * 0.15
    return(cost)
  } else if (4 < num_wland < 6 & farm_size == 20) {
    cost <- mach_op_cost * 0.19
    return(cost)
  } else if ( 7 < num_wland < 9 & farm_size == 4) {
    cost <- mach_op_cost * 0.11
    return(cost)
  } else if ( 7 < num_wland < 9 & farm_size == 8) {
    cost <- mach_op_cost * 0.12
    return(cost)
  } else if ( 7 < num_wland < 9 & farm_size == 12) {
    cost <- mach_op_cost * 0.14
    return(cost)
  } else if ( 7 < num_wland < 9 & farm_size == 16) {
    cost <- mach_op_cost * 0.17
    return(cost)
  } else if (7 < num_wland < 9 & farm_size == 20) {
    cost <- mach_op_cost * 0.21
    return(cost)
  } else if ( num_wland > 9 & farm_size == 4) {
    cost <- mach_op_cost * 0.14
    return(cost)
  } else if ( num_wland > 9 & farm_size == 8) {
    cost <- mach_op_cost * 0.15
    return(cost)
  } else if (num_wland > 9 & farm_size == 12) {
    cost <- mach_op_cost * 0.17
    return(cost)
  } else if (num_wland > 9 & farm_size == 16) {
    cost <- mach_op_cost * 0.20
    return(cost)
  } else if (num_wland > 9 & farm_size == 20) {
    cost <- mach_op_cost * 0.24
    return(cost)}
} 
InputWasteCost <- function(num_wland, farm_size, mach_op_cost){
  if (1 < num_wland < 3 & farm_size == 4) {
    cost <- mach_op_cost * 0.08*0.1
    return(cost)
  } else if ( 1 < num_wland < 3 & farm_size == 8) {
    cost <- mach_op_cost * 0.09*0.1
    return(cost) 
  } else if ( 1 < num_wland < 3 & farm_size == 12) {
    cost <- mach_op_cost * 0.11*0.1
    return(cost) 
  } else if ( 1 < num_wland < 3 & farm_size == 16) {
    cost <- mach_op_cost * 0.14*0.1
    return(cost)
  } else if (1 < num_wland < 3 & farm_size == 20) {
    cost <- mach_op_cost * 0.18*0.1
    return(cost)
  } else if ( 4 < num_wland < 6 & farm_size == 4) {
    cost <- mach_op_cost * 0.09*0.1
    return(cost) 
  } else if ( 4 < num_wland < 6 & farm_size == 8) {
    cost <- mach_op_cost * 0.10*0.1
    return(cost)
  } else if ( 4 < num_wland < 6 & farm_size == 12) {
    cost <- mach_op_cost * 0.12*0.1
    return(cost)
  } else if ( 4 < num_wland < 6 & farm_size == 16) {
    cost <- mach_op_cost * 0.15*0.1
    return(cost)
  } else if (4 < num_wland < 6 & farm_size == 20) {
    cost <- mach_op_cost * 0.19*0.1
    return(cost)
  } else if ( 7 < num_wland < 9 & farm_size == 4) {
    cost <- mach_op_cost * 0.11*0.1
    return(cost)
  } else if ( 7 < num_wland < 9 & farm_size == 8) {
    cost <- mach_op_cost * 0.12*0.1
    return(cost)
  } else if ( 7 < num_wland < 9 & farm_size == 12) {
    cost <- mach_op_cost * 0.14*0.1
    return(cost)
  } else if ( 7 < num_wland < 9 & farm_size == 16) {
    cost <- mach_op_cost * 0.17*0.1
    return(cost)
  } else if (7 < num_wland < 9 & farm_size == 20) {
    cost <- mach_op_cost * 0.21*0.1
    return(cost)
  } else if ( num_wland > 9 & farm_size == 4) {
    cost <- mach_op_cost * 0.14*0.1
    return(cost)
  } else if ( num_wland > 9 & farm_size == 8) {
    cost <- mach_op_cost * 0.15*0.1
    return(cost)
  } else if (num_wland > 9 & farm_size == 12) {
    cost <- mach_op_cost * 0.17*0.1
    return(cost)
  } else if (num_wland > 9 & farm_size == 16) {
    cost <- mach_op_cost * 0.20*0.1
    return(cost)
  } else if (num_wland > 9 & farm_size == 20) {
    cost <- mach_op_cost * 0.24*0.1
    return(cost)}
} 

  




