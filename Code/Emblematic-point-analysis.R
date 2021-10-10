########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 19, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Bandt-Pompe.R")

# HC calculation function ------------------------------------------------------------------------------
hc.series <- function(ts, D, tau){
  
  i = 0
  h.values = c()
  c.values = c()
  match.median = FALSE
  
  for(i in 1:length(ts)){
    i = i + 1
    cat("Test: ", i, "\n")
    
    probs = bandt.pompe(ts[,i], D, tau)
    h = shannon.entropy.normalized(probs)
    c = Ccomplexity(probs)
    
    h.values = c(h.values, h)
    c.values = c(c.values, c)
    cat("H: ", h, " C: ", c, "\n")
    HC.values = data.frame(H = h.values, C = c.values)
    write.csv(HC.values, "../Data/HC-D3-N1000.csv")
  }
}

# Median search function --------------------------------------------------------------------------------
emblematic.series <- function(D, tau, N, dx = 4, dy = 4){
  
  i = 0
  match.median = FALSE
  HC = read.csv("../Data/HC-D3-N1000.csv")[2:3]
  
  while(i < dim(HC)[1]){
    i = i + 1
    index = i
    
    h = round(HC$H[i], digits = dx)
    c = round(HC$C[i], digits = dy)
    
    cat("i: ", i , " h: ", h, " c: ", c, "\n")
    
    hc.points = read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
    ax = round(hc.points$H[17], digits = dx)
    ay = round(hc.points$C[17], digits = dy)
    
    if(!is.na(h) && !is.na(c)){
      if(h == ax && c == ay){
        match.median = TRUE
        break
      }
    }
  }
  
  if(match.median)
    return(index)
  else
    return(-1)
}