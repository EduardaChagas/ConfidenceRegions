########################################################################################################
# Author: Eduarda Chagas
# Date : Jul 17 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Bandt-Pompe.R")
if(!require(fftw)){
  install.packages("fftw")
  require(fftw)
}
if(!require(reshape2)){
  install.packages("reshape2")
  require(reshape2)
}
if(!require(plot3D)){
  install.packages("plot3D")
  require(plot3D)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(ggthemes)){
  install.packages("ggthemes")
  require(ggthemes)
}
if(!require(doParallel)){
  install.packages("doParallel")
  require(doParallel)
}

# Generator of noise with power spectrum f^-k --------------------------------------------------------
set.seed(seed = 1234567890, kind = "Mersenne-Twister")

series.generator.fk <- function(pp, y, n, k){
  Series = vector(mode="numeric")
  filtro = (1:n)^-(k/2)
  filtro = filtro / sum(filtro)
  y1 = y * filtro    
  x1 = IFFT(y1, plan = pp)  
  Series = c(Re(x1)) 
  return(Series)
}

# f^-k Analysis function -----------------------------------------------------------------------------
fk0.generate.point <- function(){
  k = 0
  D = c(3, 4, 5, 6)
  Tau = c(1, 10, 30, 50)
  N = 50000
  number.series = 100
  
  HC.values = data.frame(H = numeric(1600), 
                         C = numeric(1600),  
                         Tau = numeric(1600), 
                         D = numeric(1600))
  j = 0
  for(d in D){
    for(t in Tau){
      for(i in 1:number.series){
        j = j + 1
        x = rnorm(N)
        x = x - mean(x)
        pp = planFFT(N)
        y = FFT(x, plan = pp)
        fk = series.generator.fk(pp, y, N, k)
        probs = bandt.pompe(fk, d, t)
        HC.values$H[j] = shannon.entropy.normalized(probs)
        HC.values$C[j] = Ccomplexity(probs)
        HC.values$D[j] = d
        HC.values$Tau[j] = t
        cat("Series: ", j, "\n")
      }
    }
  }
  write.csv(HC.values, "HC-fk-50k.csv")
}


fk.test.confidence.regions <- function(D = 3, N = 50000, generator, table.code){
  hc.data = read.csv("../Data/PRNG/HC-fk-50k.csv")[2:5]
  index = which(hc.data['D'] == D)
  hc.D = hc.data[index,]
  result.95 = p.value.set.point(hc.D, D, N, 95)
  result.99 = p.value.set.point(hc.D, D, N, 99)
  table.code = paste0(table.code, generator, " & ", D, " & ", result.95, " & ", result.99, "\\ ")
  return(table.code)
}


test.all.fk <- function(){
  table.code = ""
  table.code = fk.test.confidence.regions(D = 3, N = 50000, generator = 'f-k', table.code)
  table.code = fk.test.confidence.regions(D = 4, N = 50000, generator = 'f-k', table.code)
  table.code = fk.test.confidence.regions(D = 5, N = 50000, generator = 'f-k', table.code)
  table.code = fk.test.confidence.regions(D = 6, N = 50000, generator = 'f-k', table.code)
  return(table.code)
}

test.all.fk()