########################################################################################################
# Author: Eduarda Chagas
# Date : Sep 15, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Test-point.R")
if(!require(plot3D)){
  install.packages("plot3D")
  require(plot3D)
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
    #print(i)
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

PNRG.test.p.values <- function(D = 3, N = 50000, generator = 1, table.code){
  
  GNR.models = c('Wichmann-Hill', 'Marsaglia-Multicarry', 'Super-Duper', 
                 'Knuth-TAOCP-2002', 'Knuth-TAOCP', 'LEcuyer-CMRG',
                 'pcg64', 'Threefry', 'Xoroshiro128+', 'Xoshiro256+', 
                 'mersenne', 'randu', 'lcg', 'Lehmer', 'mwc', 'mother', 'combo', 'borland', 'matlab')
  
  generator = GNR.models[generator]
  
  if(generator == 'mersenne'){
    hc.data = read.csv("../Data/PRNG/HC-mersenne-50k.csv")[2:5]
  }else if(generator == 'randu'){
    hc.data = read.csv("../Data/PRNG/HC-randu-50k.csv")[2:5]
  } else if(generator == 'lcg'){
    hc.data = read.csv("../Data/PRNG/HC-lcg-50k.csv")[2:5]
  }else if(generator == 'Wichmann-Hill'){
    hc.data = read.csv("../Data/PRNG/HC-Wichmann-Hill-50k.csv")[2:5]
  }else if(generator == 'Marsaglia-Multicarry'){
    hc.data = read.csv("../Data/PRNG/HC-Marsaglia-Multicarry-50k.csv")[2:5]
  }else if(generator == 'Super-Duper'){
    hc.data = read.csv("../Data/PRNG/HC-Super-Duper-50k.csv")[2:5]
  }else if(generator == 'Knuth-TAOCP-2002'){
    hc.data = read.csv("../Data/PRNG/HC-Knuth-TAOCP-2002-50k.csv")[2:5]
  }else if(generator == 'Knuth-TAOCP'){
    hc.data = read.csv("../Data/PRNG/HC-Knuth-TAOCP-50k.csv")[2:5]
  }else if(generator == 'LEcuyer-CMRG'){
    hc.data = read.csv("../Data/PRNG/HC-LEcuyer-CMRG-50k.csv")[2:5]
  }else if(generator == 'pcg64'){
    hc.data = read.csv("../Data/PRNG/HC-pcg64-50k.csv")[2:5]
  }else if(generator == 'Xoroshiro128+'){
    hc.data = read.csv("../Data/PRNG/HC-Xoroshiro128+-50k.csv")[2:5]
  }else if(generator == 'Xoshiro256+'){
    hc.data = read.csv("../Data/PRNG/HC-Xoshiro256+-50k.csv")[2:5]
  }else if(generator == 'Threefry'){
    hc.data = read.csv("../Data/PRNG/HC-Threefry-50k.csv")[2:5]
  }else if(generator == 'Lehmer'){
    hc.data = read.csv("../Data/PRNG/HC-Lehmer-50k.csv")[2:5]
  }else if(generator == 'fBm'){
    hc.data = read.csv("../Data/PRNG/HC-fBm-50k.csv")[2:5]
  }else if(generator == 'fGn'){
    hc.data = read.csv("../Data/PRNG/HC-fGn-50k.csv")[2:5]
  }else if(generator == 'mwc'){
    hc.data = read.csv("../Data/PRNG/HC-mwc-50k.csv")[2:5]
  }else if(generator == 'mother'){
    hc.data = read.csv("../Data/PRNG/HC-mother-50k.csv")[2:5]
  }else if(generator == 'combo'){
    hc.data = read.csv("../Data/PRNG/HC-combo-50k.csv")[2:5]
  }else if(generator == 'borland'){
    hc.data = read.csv("../Data/PRNG/HC-borland-50k.csv")[2:5]
  }else if(generator == 'matlab'){
    hc.data = read.csv("../Data/PRNG/HC-matlab-50k.csv")[2:5]
  }
  index = which(hc.data['D'] == D)
  hc.data = hc.data[index,]
  result = calculate.p.value.samples(hc.data, N, D)
  result.95 = p.value.set.point(hc.data, D, N, 95)
  result.99 = p.value.set.point(hc.data, D, N, 99)
  table.code = paste0(table.code, generator, " & ", D, " & ", round(result.95, 4), " & ", round(result.99, 4), " & ", result,"\\ ")
  return(table.code)
}

RNG.test.p.values <- function(D = 3, N = 50000, table.code){
  if(N == 1000){
    filePath = read.table(paste0("../../Random_/Random_", 1, "k_D", D, "-T1.dat"), header=TRUE)
  }else{
    filePath = read.table(paste0("../../Random_/Random_", 50, "k_D", D, "-T1.dat"), header=TRUE)
  }
  
  HC = matrix(data = filePath$x, nrow = 100, ncol = 3, byrow = TRUE, dimnames = NULL)
  
  HC = data.frame(H = HC[,1], 
                  C = HC[,2], 
                  D = factor(c(rep(D, 100))))
  
  result = calculate.p.value.samples(HC, N, D)
  result.95 = p.value.set.point(HC, D, N, 95)
  result.99 = p.value.set.point(HC, D, N, 99)
  table.code = paste0(table.code,"True-Random & ", N," & ", D, " & ", round(result.95, 4), " & ", round(result.99, 4), " & ", result,"\\ ")
  return(table.code)
}

p.value.all.prng <- function(){
  #registerDoParallel(cores = 6)
  table.code = ""
  for(i in 1:19){
    table.code = PNRG.test.p.values(D = 3, N = 50000, generator = i, table.code)
    table.code = PNRG.test.p.values(D = 4, N = 50000, generator = i, table.code)
    table.code = PNRG.test.p.values(D = 5, N = 50000, generator = i, table.code)
    table.code = PNRG.test.p.values(D = 6, N = 50000, generator = i, table.code)
  }
  return(table.code)
}

p.value.all.rng <- function(){
  table.code = ""
  table.code = RNG.test.p.values(D = 3, N = 1000, table.code)
  table.code = RNG.test.p.values(D = 4, N = 1000, table.code)
  table.code = RNG.test.p.values(D = 5, N = 1000, table.code)
  table.code = RNG.test.p.values(D = 6, N = 1000, table.code)
  table.code = RNG.test.p.values(D = 3, N = 50000, table.code)
  table.code = RNG.test.p.values(D = 4, N = 50000, table.code)
  table.code = RNG.test.p.values(D = 5, N = 50000, table.code)
  table.code = RNG.test.p.values(D = 6, N = 50000, table.code)
  return(table.code)
}

p.value.all.prng()
