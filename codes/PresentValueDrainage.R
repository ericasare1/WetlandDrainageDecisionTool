
PresentValueDrainage <- function(data, p_1, p_2, p_3, p_4, p_5, y_1, y_2, y_3, y_4, y_5, 
                                 ci_1, ci_2, ci_3, ci_4, ci_5, cais_1, cais_2, cais_3, cais_4, cais_5,
                                 ic_1, ic_2, ic_3, ic_4, ic_5, mc_1, mc_2, mc_3, mc_4, mc_5, n=5) {
  source("") #all the individual functions for the elements of the FarmProgram function
  ...
  ...
  #ci_1 = cost of inputs in first year
  yearly_return <- 0.4*(((((p_1*y_1 + ci_1 + cais_1) - (ci_1 - mc_1)) + ((p_2*y_2 + ci_2 + cais_2) - (ic_2 - mc_2)) + 
    ((p_3*y_3 + ci_3 + cais_3) - (ic_3- mc_3)) + ((p_4*y_4 + ci_4 + cais_4) - ( ic_4 - mc_4))                       +
    ((p_1*y_5 + ci_5 + cais_5) - (ic_5 - mc_5)))/17)/n)
  
  ((yearly_return/(1+r)^4) + (yearly_return/(1+r)^5) +(yearly_return/(1+r)^6) +(yearly_return/(1+r)^7) +(yearly_return/(1+r)^8)     +
   (yearly_return/(1+r)^9) + (yearly_return/(1+r)^10) +(yearly_return/(1+r)^11) +(yearly_return/(1+r)^12) +(yearly_return/(1+r)^13) +
   (yearly_return/(1+r)^14) + (yearly_return/(1+r)^15) +(yearly_return/(1+r)^16) +(yearly_return/(1+r)^17) +(yearly_return/(1+r)^18) +
   (yearly_return/(1+r)^19) + (yearly_return/(1+r)^20)) - dc
    
}
