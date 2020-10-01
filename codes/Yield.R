
# Filename:           yield.R
# Created:            04/12/2017
# Description: 
#This code predicts the yearly yields of canola, flax, wheat and barley based on 
#estimated yield equation; stochastic inputs are precipitation and growing degree days, as well
#corrected yield error terms
#--------------------------------------------------
set.seed(1000)

yield_c <- list()
yield_f <- list()
yield_b <- list()
yield_w <- list()

for (i in 1:20) { #Start of Simulation:
  
#Defining distributions for the stochastic variables in the yield equation
library(truncdist)  #package for drawing from truncated dist using rtrunc
gs <- round(rtrunc(1, spec = "logis", a = 30, b = 400), 2) #trucncated draws from a logis dist between 30 & 400
#gdd <- round(rtrunc(1, spec = "logis", a = 0), 2) #trucncated draws from a logistic dist between 30 & 400
gdd <- round(rlogis(1), 2)  #draws from logistic distribution with no draws
#x_w <- round(rtrunc(1, spec = "norm", a = 0), 2)
#x_c <- round(rtrunc(1, spec = "norm", a = 0), 2)
#x_f <- round(rtrunc(1, spec = "norm", a = 0), 2)
#x_b <- round(rtrunc(1, spec = "norm", a = 0), 2)
x_w <- round(rnorm(1, 0, 1), 2)  # scaled by the standard deviation of the crop?
x_c <- round(rnorm(1, 0, 1), 2) 
x_f <- round(rnorm(1, 0, 1), 2) 
x_b <- round(rnorm(1, 0, 1), 2)
#__________________________________________________
#correcting the error terms of yield equations for predictions based
#on correlation among estimated system of equation errors and predicted errors from standard
#normal distributions

#_____Estimated Correlation Coefficients Among Estimated Yield Error Terms_____________
p_w_b <- 0.6377
p_w_c <- 0.4746
p_w_f <- 0.3992
p_c_b <- 0.3459
p_c_f <- 0.4553
p_b_c <- 0.3459
p_b_f <- 0.6564
#________________Corrected Errors for Simulation_______________________________________
e_w <- x_w
e_c <- p_w_b * x_w + ((p_c_b -p_w_c*p_w_b) / sqrt(1-(p_w_c)^2)) +
       x_b*(sqrt((1-(p_w_b)^2) - ((p_c_b - p_w_c * p_c_b) / sqrt(1-(p_w_c)^2))^2))
e_b <- p_w_b * x_w + x_c * ((p_c_b - p_w_c*p_w_b)/sqrt(1-(p_w_c)^2)) +
       x_b *(sqrt(1-(p_w_b)^2 - ((p_c_b - p_w_c*p_w_b)/sqrt(1-(p_w_c)^2))^2))
e_f <- p_w_f * x_w + ((p_c_f - p_w_c*p_w_f)/sqrt(1-(p_w_c)^2)) +
       x_b * (((p_b_c - p_w_b * p_w_f) - ((p_c_b - p_w_c * p_w_b) / sqrt(1-(p_w_c)^2))) * 
       ((p_c_f - p_w_c * p_w_f) / sqrt(1-(p_w_c)^2))) /sqrt(1- (p_w_b)^2 -((p_c_b - ((p_w_c * p_w_b) / sqrt(1 - (p_w_c)^2))))) +
        x_f * (sqrt(1 - (p_w_f)^2 - ((p_c_f - p_w_c * p_w_f) / sqrt(1-(p_w_c)^2))^2) - 
        (((p_b_f - p_w_b * p_w_f) - ((p_c_b - p_w_c * p_w_b) / sqrt(1-(p_w_c)^2)) * 
        ((p_c_f - p_w_c * p_w_f) / sqrt(1-(p_w_c)^2)))/sqrt(1-(p_w_b)^2 - 
        ((p_c_b - p_w_c * p_w_b) / sqrt(1-(p_w_c)^2))^2)))
#______________Estimated Yield Equations (System) _________________________________
y_c <-  0.3574  + 8.7401  * (gs/gdd) + (-21.2970) * (gs/gdd)^2 + e_c
y_b <- -0.1010  + 22.8300 * (gs/gdd) + (-47.4440) * (gs/gdd)^2 + e_b
y_f <-  0.1561  + 8.5425  * (gs/gdd) + (-17.7670) * (gs/gdd)^2 + e_f
y_w <-  0.1329  + 18.0630 * (gs/gdd) + (-42.9690) * (gs/gdd)^2 + e_w
#____Collecting Simulated Yields Per Each Iteration________________________________
yield_c[[i]] = y_c
yield_b[[i]] = y_b
yield_f[[i]] = y_f
yield_w[[i]] = y_w

} #End of Simulation
#____________Post Simulation________________________________________________________
#Unpacking simulated yield data into numeric data
yield_canola = as.numeric(unlist(yield_c))
yield_barley = as.numeric(unlist(yield_b))
yield_flax = as.numeric(unlist(yield_f))
yield_wheat = as.numeric(unlist(yield_w))
#______Putting Yield Data into a Data.Frame_________________________________________
yield_data_20yrs <- data.frame(yield_canola, yield_barley, yield_flax, yield_wheat)
######################################################################################
