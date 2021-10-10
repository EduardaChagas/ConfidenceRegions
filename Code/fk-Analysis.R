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

# Analysis functions -----------------------------------------------------------------------------------
calculate.p.value.samples <- function(samples, N, D){
  if(N == 1000){
    HC = read.csv("../Data/HC/HC_1000.csv")[,2:4]
  }else if(N == 50000){
    HC = read.csv("../Data/HC/HC_50k.csv")[,2:4]
  }
  names(HC) = c("H", "C", "n")
  HC$n = as.factor(HC$n)
  
  HC.D = subset(HC, n == D)[,-3]
  pca_HC = prcomp(x = HC.D[,-3], retx = TRUE, 
                  center=TRUE, scale. = TRUE)
  PC = data.frame(PC1 = pca_HC$x[,1], PC2 = pca_HC$x[,2])
  
  p.values = rep(0, dim(samples)[1])
  
  
  attach(PC)
  for(i in 1:dim(samples)[1]){
    print(i)
    
    test = TRUE
    step.loop = 0
    region.test = 1.001
    while(test){
      step.loop = step.loop + 0.001
      region.test = region.test - 0.001
      
      (qpc1 = quantile(PC1, probs = c((step.loop/2), 0.5, 1 - (step.loop/2))))
      (x.pc.1 = PC1[which(min(abs(PC1- qpc1[1])) == abs(PC1- qpc1[1]))])
      (x.pc.2 = PC1[which(min(abs(PC1- qpc1[3])) == abs(PC1- qpc1[3]))])
      (y.pc.1 = min(PC2[PC1 > x.pc.1[1] & PC1 < x.pc.2[1]]))
      (y.pc.2 = max(PC2[PC1 > x.pc.1[1] & PC1 < x.pc.2[1]]))
      
      rect = data.frame(xmin = x.pc.1, xmax = x.pc.2, ymin = y.pc.1, ymax = y.pc.2)
      rect = matrix(unlist(rect), nrow = 2, ncol = 2)
      
      my.order = c(1,2,4,3)
      M = mesh(rect[,1], rect[,2])
      rect = matrix(nrow = 4, ncol = 2)
      rect[,1] = M$x
      rect[,2] = M$y
      rect = t(t(rect %*% t(pca_HC$rotation)) * pca_HC$scale + pca_HC$center)
      rect = data.frame("H" = rect[my.order, 1], 
                        "C" = rect[my.order, 2],
                        stringsAsFactors=FALSE)
      rect$H[rect$H < 0] = 0
      rect$C[rect$C < 0] = 0
      rect$H[rect$H > 1] = 1
      rect$C[rect$C > 1] = 1
      
      test = point.in.polygon(samples$H[i], samples$C[i], rect$H, rect$C)
    }
    p.values[i] = 1 - region.test
  }
  detach(PC)
  return(mean(p.values))
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

fk.test.p.values <- function(D = 3, N = 50000, table.code){
  filePath = read.csv("../Data/PRNG/HC-fk-50k.csv")[2:5]
  index = which(filePath['D'] == D)
  hc.data = filePath[index,]
  result = calculate.p.value.samples(hc.data, N, D)
  result.95 = p.value.set.point(hc.data, D, N, 95)
  result.99 = p.value.set.point(hc.data, D, N, 99)
  table.code = paste0(table.code,"True-Random & ", N," & ", D, " & ", round(result.95, 4), " & ", round(result.99, 4), " & ", result,"\\ ")
  return(table.code)
}


test.all.fk <- function(){
  table.code = ""
  table.code = fk.test.p.values(D = 3, N = 50000, table.code)
  table.code = fk.test.p.values(D = 4, N = 50000, table.code)
  table.code = fk.test.p.values(D = 5, N = 50000, table.code)
  table.code = fk.test.p.values(D = 6, N = 50000, table.code)
  return(table.code)
}

test.all.fk()