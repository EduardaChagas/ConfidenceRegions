########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 22, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Bandt-Pompe.R")
if(!require(doParallel)){
  install.packages("doParallel")
  require(doParallel)
}

# Analysis functions -----------------------------------------------------------------------------------
fBm.HC.generator <- function(){
  # N = 50000 -------------------
  a = -100
  N = 50000
  D = c(3, 4, 5, 6)
  tau = c(1, 10, 30, 50)
  hc.50k = data.frame(H = numeric(1600), C = numeric(1600), D = numeric(1600), tau = numeric(1600))
  i = 0
  for(t in tau){
    for(d in D){
      a = a + 100
      for(j in 1:100){
        i = i + 1
        cat("fBm: ", i, '\n')
        ts = read.csv("../../fBm.csv")[j, 2:50001]
        probs = bandt.pompe(random.series[j,], d, t)
        hc.50k$H[j + a] = shannon.entropy.normalized(probs)
        hc.50k$C[j + a] = Ccomplexity(probs)
        hc.50k$D[j + a] = d
        hc.50k$tau[j + a] = t
      }
    }
  }
  write.csv(hc.50k, file = "../Data/PRNG/HC-fBm-50k.csv")
}

fGn.HC.generator <- function(){
  # N = 50000 -------------------
  a = -100
  N = 50000
  D = c(3, 4, 5, 6)
  tau = c(1, 10, 30, 50)
  hc.50k = data.frame(H = numeric(1600), C = numeric(1600), D = numeric(1600), tau = numeric(1600))
  i = 0
  for(t in tau){
    for(d in D){
      a = a + 100
      for(j in 1:100){
        i = i + 1
        cat("fGn: ", i, '\n')
        ts = read.csv("../../fGn.csv")[j, 2:50001]
        probs = bandt.pompe(random.series[j,], d, t)
        hc.50k$H[j + a] = shannon.entropy.normalized(probs)
        hc.50k$C[j + a] = Ccomplexity(probs)
        hc.50k$D[j + a] = d
        hc.50k$tau[j + a] = t
      }
    }
  }
  write.csv(hc.50k, file = "../Data/PRNG/HC-fBm-50k.csv")
}

registerDoParallel(cores = 2)
foreach(i = 1:2) %dopar% {
  if(i == 1)
    fBm.HC.generator()
  else
    fGn.HC.generator()
}