#P(5W) = P(W)* P(W |W) : probability of 5 consecutive days
##################################
sum_workdays <- function(n, unc_wd_prob, cond_wd_prob) {
   out <- list()
for (i in 1:1) {  # 2. sequence
  library(truncdist)  #package for drawing from truncated dist using rtrunc
  pb <- round(runif(1, min = 0, max = 1), 2)
  pb
  out[[i]] <- ifelse(pb < unc_wd_prob, 1,0) #unconditional prob in august
}
  out1 <- as.numeric(unlist(out))
  out1
  out2 <- append(out1, rep(0, n - 1))
################################# 
  out3 <- list()
for (i in 2:n) {
  if (out2[i - 1] == 1 & pb < unc_wd_prob*cond_wd_prob) {
    out3[[i]] <- 1
  } else if (pb < unc_wd_prob) {
    out3[[i]] <- 1
  } else {
    out3[[i]] <- 0 
  }
}
  out4 <- as.numeric(unlist(out3))
  out5 <- append(out1, out4)
 return(sum(out5))
}
###################################
sum_workdays(n = 31, unc_wd_prob = 0.68, cond_wd_prob = 0.8)
