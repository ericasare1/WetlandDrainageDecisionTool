
# Filename:           price.R
# coreated:            04/12/2017
# Descoricortion: 
#This code corredicts the yearly prices of canola, flapr_error, wheat and barley based on 
#estimated price equation; stochastic incoruts are correcicoritation and growing degree days, as well
#corrected price error terms
#--------------------------------------------------

n=20
set.seed(1000)

price_c = list()
price_f = list()
price_b = list()
price_w = list()

for(i in 1:n) {
  #Defining distributions for the stochastic variables in the price equation
  library(truncdist)  #package for drawing from truncated dist using rtrunc
  pr_error_w <- round(rnorm(1, 0, 1), 2)  # scaled by the standard deviation of the crop?
  pr_error_c <- round(rnorm(1, 0, 1), 2)  # scaled by the standard deviation of the crop?
  pr_error_f <- round(rnorm(1, 0, 1), 2)  # scaled by the standard deviation of the crop?
  pr_error_b <- round(rnorm(1, 0, 1), 2)  # scaled by the standard deviation of the crop?
  #__________________________________________________
  #correcting the error terms of price equations for corredictions based
  #on correlation among estimated system of equation errors and corredicted errors from standard
  #normal distributions
  
  cor_w_b <- 0.6377
  cor_w_c <- 0.4746
  cor_w_f <- 0.3992
  cor_c_b <- 0.3459
  cor_c_f <- 0.4553
  cor_b_c <- 0.3459
  cor_b_f <- 0.6564
  
  
  pr_error_sim_w <- pr_error_w
  
  pr_error_sim_c <- cor_w_b * pr_error_w + ((cor_c_b -cor_w_c*cor_w_b) / sqrt(1-(cor_w_c)^2)) +
    pr_error_b*(sqrt((1-(cor_w_b)^2) - ((cor_c_b - cor_w_c * cor_c_b) / sqrt(1-(cor_w_c)^2))^2))
  
  pr_error_sim_b <- cor_w_b * pr_error_w + pr_error_c * ((cor_c_b - cor_w_c*cor_w_b)/sqrt(1-(cor_w_c)^2)) +
    pr_error_b *(sqrt(1-(cor_w_b)^2 - ((cor_c_b - cor_w_c*cor_w_b)/sqrt(1-(cor_w_c)^2))^2))
  
  pr_error_sim_f <- cor_w_f * pr_error_w + ((cor_c_f - cor_w_c*cor_w_f)/sqrt(1-(cor_w_c)^2)) +
    pr_error_b * (((cor_b_c - cor_w_b * cor_w_f) - ((cor_c_b - cor_w_c * cor_w_b) / sqrt(1-(cor_w_c)^2))) * 
             ((cor_c_f - cor_w_c * cor_w_f) / sqrt(1-(cor_w_c)^2))) /sqrt(1- (cor_w_b)^2 -((cor_c_b - ((cor_w_c * cor_w_b) / sqrt(1 - (cor_w_c)^2))))) +
    pr_error_f * (sqrt(1 - (cor_w_f)^2 - ((cor_c_f - cor_w_c * cor_w_f) / sqrt(1-(cor_w_c)^2))^2) - 
             (((cor_b_f - cor_w_b * cor_w_f) - ((cor_c_b - cor_w_c * cor_w_b) / sqrt(1-(cor_w_c)^2)) * 
                 ((cor_c_f - cor_w_c * cor_w_f) / sqrt(1-(cor_w_c)^2)))/sqrt(1-(cor_w_b)^2 - 
                                                                       ((cor_c_b - cor_w_c * cor_w_b) / sqrt(1-(cor_w_c)^2))^2)))
  
  #______________Estimated Price Equations (System) _______________________________
  
  pr_sim_c[[i]] <-  1.9336  + (0.8783) * (l.1) + (-0.4640) * (l.2)^2 +  (0.2739) * (l.3)^2 + pr_error_sim_c
  pr_sim_b <-  2.3948  + 0.5443 * (l.1)  +  pr_error_sim_b
  pr_sim_f <-  2.3052  + 0.8128  * (l.1) + (-0.4447) * (l.2)^2 +  (0.2572) * (l.3)^2  + pr_error_sim_f
  pr_sim_w <-  2.0972  + 0.6742 * (l.1) + (-0.2700) * (l.2)^2 + (0.2168) * (l.3)^2  + pr_error_sim_w
  
  price_c[[i]] = pr_error_sim_c
  price_b[[i]] = pr_error_sim_b
  price_f[[i]] = pr_error_sim_f
  price_w[[i]] = pr_error_sim_w
  
}

price_canola = as.numeric(unlist(price_c))
price_barley = as.numeric(unlist(price_b))
price_flapr_error = as.numeric(unlist(price_f))
price_wheat = as.numeric(unlist(price_w))

price_data_20yrs <- data.frame(price_canola, price_barley, price_flapr_error, price_wheat)


