########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 22, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Bandt-Pompe.R")
source("Test-point.R")
if(!require(dqrng)){
  install.packages("dqrng")
  require(dqrng)
}
if(!require(gtools)){
  install.packages("gtools")
  require(gtools)
}
if(!require(latex2exp)){
  install.packages("latex2exp")
  require(latex2exp)
}
if(!require(doParallel)){
  install.packages("doParallel")
  require(doParallel)
}
if(!require(ggthemes)){
  install.packages("ggthemes")
  require(ggthemes)
}
if(!require(ggpubr)){
  install.packages("ggpubr")
  require(ggpubr)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}

# Number generating functions --------------------------------------------------------------------------

seed <- as.double(1)
RANDU <- function(){
  set.seed(123, kind = "Mersenne-Twister")
  seed <<- ((1103515245 * seed ) + 12345 ) %% (2^31)
  return(seed)
}

lehmer.rng <- function(n) {
  
  rng <- vector(length = n)
  
  m <- 2147483647
  a <- 48271
  q <- 44488
  r <- 3399
  
  # Set the seed using the current system time in microseconds. 
  # The initial seed value must be coprime to the modulus m, 
  # which we are not really concerning ourselves with for this example.
  d <- as.numeric(Sys.time())
  
  for (i in 1:n) {
    h <- d / q
    l <- d %% q
    t <- a * l - r * h
    if (t > 0) {
      d <- t
    }
    else {
      d <- t + m
    }
    
    rng[i] <- d / m
  }
  return(rng)
}

MWC.rng <- function(n) {
  
  rng <- vector(length = n)
  
  a <- 2131995753
  x <- 1947362876
  m <- 4294967296
  
  for (i in 1:n) {
    c <- floor(x/m)
    x <- (a * x) + c
    x <- x %% m
    rng[i] <- x
  }
  
  return(rng)
}

Mother.rng <- function(n) {
  
  rng <- vector(length = n)
  
  m <- 4294967296
  
  a <- 2111111111
  b <- 1492
  c <- 1776
  d <- 5115
  
  x.n_1 <- 1143897285
  x.n_2 <- 1549345678
  x.n_3 <- 205987485
  x.n_4 <- 164987491
  
  for (i in 1:n) {
    c <- floor(x.n_1/m)
    x <- (a * x.n_4) + (b * x.n_3) + (c * x.n_2) + (d * x.n_1) + c
    x <- x %% m
    rng[i] <- x
    x.n_4 <- x.n_3
    x.n_3 <- x.n_2
    x.n_2 <- x.n_1
    x.n_1 <- x
  }
  
  return(rng)
}

Combo.rng <- function(n) {
  
  rng <- vector(length = n)
  
  m1 <- 4294967296
  m2 <- 65536
  a <- 30903
  
  x <- 356819112
  x.n_1 <- 455997113
  y.n_1 <- 158644912
  
  for (i in 1:n) {
    c <- floor(x/m2)
    
    x <- x * x.n_1
    x <- x %% m1
    
    y <- (a * y.n_1) + c
    y <- y %% m2
    
    z <- x + y
    z <- z %% m2
    
    rng[i] <- z
    
    x.n_1 <- x
    y.n_1 <- y
  }
  
  return(rng)
}

# The parameters we will use for our implementation of the linear congruential generator are the same as 
# the ANSI C implementation (Saucier, 2000.)
lcg.rand <- function(n) {
  
  rng = vector(length = n)
  
  m = 2 ** 32
  a = 1103515245
  c = 12345
  
  # Set the seed using the current system time in microseconds
  d = as.numeric(Sys.time()) * 1000
  
  for(i in 1:n) {
    d = (a * d + c) %% m
    rng[i] = d / m
  }
  return(rng)
}

PNRG <- function(n.series, size.series, generator = 'randu'){
  
  random.vector = matrix(nrow = n.series, ncol = size.series)
  
  if(generator == 'randu'){#Randu
    for(i in 1:n.series){
      for(j in 1:size.series){
        random.vector[i, j] = c(RANDU())
      }
    }
  }else if(generator == 'mersenne'){#Mersenne-Twister
    set.seed(1234567890, kind = "Mersenne-Twister")
    for(i in 1:n.series){
      random.vector[i,] = runif(size.series)
    }
  }else if(generator == 'lcg'){#Linear Congruential Generator
    for(i in 1:n.series){
      random.vector[i,] = lcg.rand(size.series)
    }
  }else if(generator == 'Wichmann-Hill'){
    RNGkind("Wichmann-Hill")
    set.seed(1234567890, kind = "Wichmann-Hill")
    for(i in 1:n.series){
      random.vector[i,] = runif(size.series)
    }
  }else if(generator == 'Marsaglia-Multicarry'){
    RNGkind("Marsaglia-Multicarry")
    set.seed(1234567890, kind = "Marsaglia-Multicarry")
    for(i in 1:n.series){
      random.vector[i,] = runif(size.series)
    }
  }else if(generator == 'Super-Duper'){
    RNGkind("Super-Duper")
    set.seed(1234567890, kind = "Super-Duper")
    for(i in 1:n.series){
      random.vector[i,] = runif(size.series)
    }
  }else if(generator == 'Knuth-TAOCP-2002'){
    RNGkind("Knuth-TAOCP-2002")
    set.seed(1234567890, kind = "Knuth-TAOCP-2002")
    for(i in 1:n.series){
      random.vector[i,] = runif(size.series)
    }
  }else if(generator == 'Knuth-TAOCP'){
    RNGkind("Knuth-TAOCP")
    set.seed(1234567890, kind = "Knuth-TAOCP")
    for(i in 1:n.series){
      random.vector[i,] = runif(size.series)
    }
  }else if(generator == 'LEcuyer-CMRG'){
    RNGkind("L'Ecuyer-CMRG")
    set.seed(1234567890, kind = "L'Ecuyer-CMRG")
    for(i in 1:n.series){
      random.vector[i,] = runif(size.series)
    }
  }else if(generator == 'Xoroshiro128+'){
    dqRNGkind("Xoroshiro128+")
    dqset.seed(1234567890)
    for(i in 1:n.series){
      random.vector[i,] = dqrunif(size.series, min = 0, max = 1)
    }
  }else if(generator == 'Xoshiro256+'){
    dqRNGkind("Xoshiro256+")
    dqset.seed(1234567890)
    for(i in 1:n.series){
      random.vector[i,] = dqrunif(size.series, min = 0, max = 1)
    }
  }else if(generator == 'pcg64'){
    dqRNGkind("pcg64")
    dqset.seed(1234567890)
    for(i in 1:n.series){
      random.vector[i,] = dqrunif(size.series, min = 0, max = 1)
    }
  }else if(generator == 'Threefry'){
    dqRNGkind("Threefry")
    dqset.seed(1234567890)
    for(i in 1:n.series){
      random.vector[i,] = dqrunif(size.series, min = 0, max = 1)
    }
  }else if(generator == 'Lehmer'){
    for(i in 1:n.series){
      random.vector[i,] = lehmer.rng(size.series)
    }
  }else if(generator == 'mwc'){
    for(i in 1:n.series){
      random.vector[i,] = MWC.rng(size.series)
    }
  }else if(generator == 'mother'){
    for(i in 1:n.series){
      random.vector[i,] = Mother.rng(size.series)
    }
  }else if(generator == 'combo'){
    for(i in 1:n.series){
      random.vector[i,] = Combo.rng(size.series)
    }
  }else if(generator == 'borland'){
    rand.borland = array(unlist(read.csv("../Data/PRNG/borland-50k.csv")))
    for(i in 1:n.series){
      random.vector[i,] = rand.borland[(((i-1)*n.series) + 1):(n.series*i)]
      print(i)
    }
  }
  return(random.vector)
}

# Analysis functions -----------------------------------------------------------------------------------

PNRG.HC.generator <- function(i){
  
  GNR.models = c('Wichmann-Hill', 'Marsaglia-Multicarry', 'Super-Duper', 
                 'Knuth-TAOCP-2002', 'Knuth-TAOCP', 'LEcuyer-CMRG',
                 'pcg64', 'Threefry', 'Xoroshiro128+', 'Xoshiro256+', 
                 'mersenne', 'randu', 'lcg', 'Lehmer', 'mwc', 'mother', 'combo', 'borland')
  
  i = GNR.models[i]
  
  # N = 50000 -------------------
  a = -100
  N = 50000
  D = c(3, 4, 5, 6)
  tau = c(1, 10, 30, 50)
  random.series = PNRG(100, N, i)
  hc.50k = data.frame(H = numeric(1600), C = numeric(1600), D = numeric(1600), tau = numeric(1600))
  
  for(t in tau){
    for(d in D){
      a = a + 100
      for(j in 1:100){
        cat("N: ", N, " D: ", d, " tau: ", t, " series: ", j, '\n')
        probs = bandt.pompe(random.series[j,], d, t)
        hc.50k$H[j + a] = shannon.entropy.normalized(probs)
        hc.50k$C[j + a] = Ccomplexity(probs)
        hc.50k$D[j + a] = d
        hc.50k$tau[j + a] = t
      }
    }
  }
  write.csv(hc.50k, file = paste0("../Data/PRNG/HC-", i, "-50k.csv"))
}

PNRG.test.confidence.regions <- function(D = 3, N = 50000, generator = 1, table.code){
  
  GNR.models = c('Wichmann-Hill', 'Marsaglia-Multicarry', 'Super-Duper', 
                 'Knuth-TAOCP-2002', 'Knuth-TAOCP', 'LEcuyer-CMRG',
                 'pcg64', 'Threefry', 'Xoroshiro128+', 'Xoshiro256+', 
                 'mersenne', 'randu', 'lcg', 'Lehmer', 'mwc', 'mother', 'combo', 'borland')
  
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
  }
  index = which(hc.data['D'] == D)
  hc.D = hc.data[index,]
  result.95 = p.value.set.point(hc.D, D, N, 95)
  result.99 = p.value.set.point(hc.D, D, N, 99)
  table.code = paste0(table.code, generator, " & ", D, " & ", round(result.95, 4), " & ", round(result.99, 4), "\\ ")
  return(table.code)
}

PNRG.HC.confidence.regions <- function(D = 3, tau = 1, N = 50000, generator = 'lcg', horizontal = 0, vertical = 0){
  
  if(generator == 'mersenne'){
    if(N == 1000)
      hc.data = read.csv("../Data/PRNG/HC-mersenne-1000.csv")[2:5]
    else
      hc.data = read.csv("../Data/PRNG/HC-mersenne-50k.csv")[2:5]
  }else if(generator == 'randu'){
    if(N == 1000)
      hc.data = read.csv("../Data/PRNG/HC-randu-1000.csv")[2:5]
    else
      hc.data = read.csv("../Data/PRNG/HC-randu-50k.csv")[2:5]
  } else if(generator == 'lcg'){
    if(N == 1000)
      hc.data = read.csv("../Data/PRNG/HC-lcg-1000.csv")[2:5]
    else
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
  }
  
  hc.confidence.regions = read.csv(paste0("../Data/Regions-HC/regions-hc-D", D,"-N", N, ".csv"))[2:4]
  rect95 = data.frame(H = hc.confidence.regions$H[5:8], C = hc.confidence.regions$C[5:8])
  rect99 = data.frame(H = hc.confidence.regions$H[9:12], C = hc.confidence.regions$C[9:12])
  
  index = which(hc.data['D'] == D)
  hc.D = hc.data[index,]
  result.95 = test.set.point(hc.D, D, N, 95)
  result.99 = test.set.point(hc.D, D, N, 99)
  
  p = ggplot(data = hc.D, aes(x = H, y = C)) +
    ggtitle("") +
    geom_point(size = 1.5, alpha = .5) + 
    geom_polygon(data=rect95, aes(x = H, y = C), fill="red", alpha=0.5, inherit.aes = FALSE) +
    geom_polygon(data=rect99, aes(x = H, y = C), fill="green", alpha=0.3, inherit.aes = FALSE) +
    geom_point(aes(x = hc.confidence.regions$H[17], y = hc.confidence.regions$C[17]), colour="red") +
    theme_few(base_size = 18, base_family = "serif")  +
    theme(plot.title = element_text(hjust=0.5))
  
  xlab = ylab = ""
  if(horizontal == 1)
    ylab = paste("D = ", D)
  if(vertical == 1)
    xlab = bquote(tau==.(tau))
  
  p = p + labs(x = xlab, y = ylab, parse = TRUE)
  return(p)
}

plot.PNRG.analysis <- function(N = 50000, generator = 'lcg'){
  
  D = c(3, 4, 5, 6) #Dimension parameter
  tau = c(1, 10, 30, 50) #Delay parameter
  plots = array(list(), 16)
  horizontal = vertical = 0
  
  if(generator == 'mersenne'){
    if(N == 1000)
      hc.data = read.csv("../Data/PRNG/HC-mersenne-1000.csv")[2:5]
    else
      hc.data = read.csv("../Data/PRNG/HC-mersenne-50k.csv")[2:5]
  }else if(generator == 'randu'){
    if(N == 1000)
      hc.data = read.csv("../Data/PRNG/HC-randu-1000.csv")[2:5]
    else
      hc.data = read.csv("../Data/PRNG/HC-randu-50k.csv")[2:5]
  }else if(generator == 'lcg'){
    if(N == 1000)
      hc.data = read.csv("../Data/PRNG/HC-lcg-1000.csv")[2:5]
    else
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
  }
  
  i = 0
  for(d in D){
    for(t in tau){
      i = i + 1
      
      if(d == 6)
        vertical = 1
      else
        vertical = 0
      
      if(t == 1)
        horizontal = 1
      else 
        horizontal = 0
      
      cat("\n D:", d, " tau: ", t, "\n")
      plots[[i]] = PNRG.HC.confidence.regions(D = d, tau = t, N, generator, horizontal, vertical)
    }
  }
  
  p = ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]],
                plots[[5]], plots[[6]], plots[[7]], plots[[8]],
                plots[[9]], plots[[10]], plots[[11]], plots[[12]],
                plots[[13]], plots[[14]], plots[[15]], plots[[16]],
                ncol=4, nrow=4, common.legend = TRUE, legend = "right") + 
    ggtitle(TeX("\\textit{H} $ \\times $ \\textit{C Plane}")) +
    xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
    labs(colour=expression(italic(Regions))) +
    theme_minimal() + theme(text=element_text(size=18, family="Times"), 
                            axis.text.x=element_blank(), axis.text.y=element_blank(),
                            plot.title = element_text(hjust=0.5)) + 
    guides(colour = guide_legend(override.aes = list(size=3)))
  return(p)
}

generator.all <- function(){
  #registerDoParallel(cores = 3)
  foreach(i = 18) %dopar% {
    PNRG.HC.generator(i)
    cat("Generating PRNG ", i, "\n")
  }
}

test.all <- function(){
  #registerDoParallel(cores = 6)
  table.code = ""
  for(i in 18){
    table.code = PNRG.test.confidence.regions(D = 3, N = 50000, generator = i, table.code)
    table.code = PNRG.test.confidence.regions(D = 4, N = 50000, generator = i, table.code)
    table.code = PNRG.test.confidence.regions(D = 5, N = 50000, generator = i, table.code)
    table.code = PNRG.test.confidence.regions(D = 6, N = 50000, generator = i, table.code)
  }
  return(table.code)
}

test.all()

#pdf("pcg64-50000.pdf", width = 24, height = 16)
#p = plot.PNRG.analysis(N = 50000, generator = 'pcg64')
#print(p)
#dev.off() 
