###############################Drawn Powered Cost Functions###################################
TractorCost <- function(tractor_power, UseTractor,
                        hours_use_55h.p =200, cost_per_hr_55h.p = 9.63,
                        hours_use_150h.p =200, cost_per_hr_150h.p = 37.23,
                        hours_use_170h.p =200, cost_per_hr_170h.p= 43.65,
                        hours_use_275h.p =200, cost_per_hr_275h.p = 41.20,
                        hours_use_400h.p =200, cost_per_hr_400h.p = 57.4) {
  
  if (tractor_power == 55) {
    cost_tractor_55h.p <- UseTractor * (hours_use_55h.p*cost_per_hr_55h.p)
    return(cost_tractor_55h.p)
  } else if (tractor_power == 150) {
    cost_tractor_150h.p <- UseTractor * (hours_use_150h.p*cost_per_hr_150h.p)
    return(cost_tractor_150h.p)
  } else if (tractor_power == 170) {
    cost_tractor_170h.p <- UseTractor * (hours_use_170h.p*cost_per_hr_170h.p)
    return(cost_tractor_170h.p)
  } else if (tractor_power == 275) {
    cost_tractor_275h.p <- UseTractor * (hours_use_275h.p*cost_per_hr_275h.p)
    return(cost_tractor_275h.p)
  } else if (tractor_power == 400) {
    cost_tractor_400h.p <- UseTractor * (hours_use_400h.p*cost_per_hr_400h.p)
    return(cost_tractor_400h.p)
  } 
  
}

TractorCost(150,0)

SwathCost_powered <- function(swath_width, UseSwath_powered, 
                              hours_use_20ft =60, cost_per_hr_20ft = 10.08,
                              hours_use_24ft=100, cost_per_hr_24ft=5.71,
                              hours_use_30ft=100, cost_per_hr_30ft=5.08,
                              hours_use_36ft=100, cost_per_hr_36ft=4.31) {
  
  if (swath_width == 20) {
    cost_swath_powered_20ft <- UseSwath_powered * (hours_use_20ft + cost_per_hr_20ft)
    return(cost_swath_powered_20ft)
  } else if (swath_width == 24) {
    cost_swath_powered_24ft <- UseSwath_powered * (hours_use_24ft + cost_per_hr_24ft)
    return(cost_swath_powered_24ft)
  } else if (swath_width == 40) {
    cost_swath_powered_30ft <- UseSwath_powered * (hours_use_30ft + cost_per_hr_30ft)
    return(cost_swath_powered_30ft)
  } else if (swath_width == 36) {
    cost_swath_powered_36ft <- UseSwath_powered * (hours_use_36ft + cost_per_hr_36ft)
    return(cost_swath_powered_36ft)
  } 
}

CombineCost <- function(combine_class, UseCombine, 
                              hours_use_cl5 =120, cost_per_hr_cl5 = 16.07,
                              hours_use_cl6=150, cost_per_hr_cl6=13.18,
                              hours_use_cl7=180, cost_per_hr_cl7=11.15,
                              hours_use_cl8=180, cost_per_hr_cl8=4.31) {
  
  if (combine_class == 5) {
    cost_comnibe_cl5 <- UseCombine * (hours_use_cl5 + cost_per_hr_cl5)
    return(cost_comnibe_cl5)
  } else if (combine_class == 6) {
    cost_comnibe_cl6 <- UseCombine * (hours_use_cl6 + cost_per_hr_cl6)
    return(cost_comnibe_cl6)
  } else if (combine_class == 7) {
    cost_comnibe_cl7 <- UseCombine * (hours_use_cl5 + cost_per_hr_cl7)
    return(cost_comnibe_cl7) 
  } else if (combine_class == 8) {
    cost_comnibe_cl8 <- UseCombine * (hours_use_cl8 + cost_per_hr_cl8)
    return(cost_comnibe_cl8)
  } 
}
###############################End Powered Equipment###################################

###############################Drawn Equipment Cost Functions###################################
SeederCost <- function(seeder_width, UseSeeder, hours_use_20ft =100, cost_per_hr_20ft = 4.9,
                        hours_use_24ft=150, cost_per_hr_24ft=3.7, hours_use_30ft=150, cost_per_hr_30ft=4.49,
                        hours_use_40ft=150, cost_per_hr_40ft=3.1, hours_use_50ft=150, cost_per_hr_50ft=2.79) {
  
  if (seeder_width == 20) {
    cost_seeder_20ft <- UseSeeder * (hours_use_20ft*cost_per_hr_20ft)
    return(cost_seeder_20ft)
  } else if (seeder_width == 24) {
    cost_seeder_24ft <- UseSeeder * (hours_use_24ft*cost_per_hr_24ft)
    return(cost_seeder_24ft)
  } else if (seeder_width == 30) {
    cost_seeder_30ft <- UseSeeder * (hours_use_30ft*cost_per_hr_30ft)
    return(cost_seeder_30ft)
  } else if (seeder_width == 40) {
    cost_seeder_40ft <- UseSeeder * (hours_use_40ft*cost_per_hr_40ft)
    return(cost_seeder_40ft)
  } else if (seeder_width == 50) {
      cost_seeder_50ft <- UseSeeder * (hours_use_50ft*cost_per_hr_50ft)
      return(cost_seeder_50ft)
  } 
    
}
  
SeederCost(20, 1)
  

SprayerCost <- function(sprayer_width, UseSprayer, hours_use_60ft =60, cost_per_hr_60ft = 0.79,
                       hours_use_95ft=60, cost_per_hr_95ft=0.89, hours_use_110ft_60hrs=60, cost_per_hr_110ft_60hrs=0.82,
                       hours_use_110ft_120hrs=120, cost_per_hr_110ft_120hrs=0.41) {
  
  if (sprayer_width == 60) {
    cost_sprayer_60ft <- UseSprayer * (hours_use_60ft + cost_per_hr_60ft)
    return(cost_sprayer_60ft)
  } else if (sprayer_width == 95) {
    cost_sprayer_95ft <- UseSprayer * (hours_use_95ft + cost_per_hr_95ft)
    return(cost_sprayer_95ft)
  } else if (sprayer_width == 110) {
    cost_sprayer_110ft <- UseSprayer * (hours_use_110ft_60hrs + cost_per_hr_110ft_60hrs)
    return(cost_sprayer_110ft)
  } else if (sprayer_width == 110) {
    cost_sprayer_110ft <- UseSprayer * (hours_use_110ft_120hrs + cost_per_hr_110ft_120hrs)
    return(cost_sprayer_110ft)
  }
  
}

SwathCost <- function(swath_width, UseSwath, hours_use_24ft =100, cost_per_hr_24ft = 1.35,
                        hours_use_30ft=100, cost_per_hr_30ft=1.80,
                        hours_use_36ft=100, cost_per_hr_36ft=0.82) {
  
  if (swath_width == 24) {
    cost_swath_24ft <- UseSwath * (hours_use_24ft + cost_per_hr_24ft)
    return(cost_swath_24ft)
  } else if (swath_width == 30) {
    cost_swath_30ft <- UseSwath * (hours_use_30ft + cost_per_hr_30ft)
    return(cost_swath_30ft)
  } else if (swath_width == 36) {
    cost_swath_36ft <- UseSwath * (hours_use_36ft + cost_per_hr_36ft)
    return(cost_swath_36ft)
  } 
}
###############################End Drawn Equipment###################################