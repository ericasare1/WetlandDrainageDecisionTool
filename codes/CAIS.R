#reference margin: The average level of returns for the CAIS program is an Olympic 5-year average
#margin for allowable income and expense items. An Olympic 5-year average margin is
#calculated by removing the high and the low margins during the 5-year period. This
#margin is called the farm operator’s reference margin. Within the simulation this was
#calculated using crop revenues and crop insurance payments less all costs used in this
#research study except for drainage costs.
RM <- 50000 # Reference margin
#program margin for the year i think is the margin for the year in question not historical
#like the reference margin
PM <- 20000 #program margin


#The CAIS deposit was treated as a reduction in cash flow, since farm operators do not
#have access to these funds unless a program payment is triggered. At the 85% level of
#protection, the farm operator was required to deposit a dollar amount equal to 20% of the
#0%-70% range of their reference margin plus 30% of the 70%-85% range of their
#reference margin. For example, if a farm operator’s reference margin was $50,000, then
#the amount that they would require to have on deposit would equal

CAIS_deposit <- function(RM){
  dep <- (RM*0.70*0.20) + (RM*0.15*0.30) 
  return(dep)
}

CAIS_payment <- function(RM, PM){
  payment <- (RM*0.7 - PM)*0.8 + (RM*0.85 - RM*0.7)*0.7  #payment by government tier 2
  return(payment)
}

CAIS_withdrawal <- function(RM, PM){
  withdrawal <- (RM*0.7 - PM)*0.2 + (RM*0.85 - RM*0.7)*0.3 #withdrawal of account
  return(withdrawal)
}


crop_insurance <- function(yd, farmyd) {
  farmyd <- yd * 0.1 + farmyd(i -1)*0.9  #refine it well
 if (yd < 0.7*farmyd) {
 payment <- 0
   return(payment)
 } else {
   return(0)
 }

}









