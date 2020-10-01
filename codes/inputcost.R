
Input_cost <- function(acre, can_seed=50.26, bar_seed=18.63, flax_seed=19.89, wht_seed=26.87,
                       can_pest=78.08, bar_pest=63.75, flax_pest=63.75, wht_pest=63.75,
                       can_fert=53.93, bar_fert=50.43, flax_fert=61,  wht_fert=58.73, fallow_pest=29.9) {
  
  seed_cost <- acre*(can_seed + bar_seed + flax_seed + wht_seed)
  pesticide_cost <- acre*(can_pest + bar_pest + flax_pest + wht_pest)
  fertilizer_cost <- acre*(can_fert + bar_fert + flax_fert + wht_fert)
  Total_inputcost <- seed_cost + pesticide_cost + fertilizer_cost + fallow_pest
  
  return(Total_inputcost)
}

Input_cost(20)