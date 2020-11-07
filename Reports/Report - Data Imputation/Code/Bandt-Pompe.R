########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 18, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################
# Packages and sources ---------------------------------------------------------------------------------
if(!require(gtools)){
  install.packages("gtools")
  require(gtools)
} 

# Auxiliar Function -------------------------------------------------------------------------------------

define.symbols <- function(D){
  d = c(1:D)
  symbol = matrix(unlist(permutations(n = D, r = D, v = d)),nrow = factorial(D), ncol = D, byrow = FALSE)
  symbol = symbol - 1
  symbol
}

FP <- function(n, dimension, delay){
  dyn.load("FormationPatterns.so")
  p <- .Call("FormationPatterns", n, dimension, delay)
  p = t(p) + 1
  return(p)
}

formationPattern <- function(series, D, tau, option){
  
  i = 1
  n = length(series)
  p_patterns = elements = matrix(nrow = n, ncol = D)
  index = c(0:(D-1))
  
  for(s in seq(1, length(series)-(D-1)*tau, by = 1)){
    # the indices for the subsequence
    ind = seq(s, s+(D-1)*tau, by = tau)
    elements[i,] = series[ind]
    p_patterns[i,] = index[order(elements[i,])]
    i = i + 1
  }
  
  if(option == 0){
    p_patterns = na.omit(p_patterns)
    return(p_patterns[1:(i-1),])
  }else if(option == 1){
    elements = na.omit(elements)
    return(elements[1:(i-1),])    
  }
}

# Bandt-Pompe function ---------------------------------------------------------------------------------

bandt.pompe <- function(series, dimension, delay){
  dyn.load("BandtPompe.so")
  elements = formationPattern(series, dimension, delay, 1)
  element.size = dim(elements)[1]
  probability <- .Call("BandtPompe", elements, dimension, element.size)
  return(probability)
}


# Shannon Entropy function -------------------------------------------------------------------------------
shannon.entropy <- function(prob){
  entropy = prob * log(prob)
  entropy[is.nan(entropy)] = 0
  return(-sum(entropy))
}

shannon.entropy.normalized <- function(prob){
  entropy = (shannon.entropy(prob)/log(length(prob)))
  entropy[is.nan(entropy)] = 0
  return(entropy)
}

# Jensen Divergence function ------------------------------------------------------------------------------
jensen.divergence <- function(prob){
  cc = rep(1/length(prob),length(prob))
  s_p = shannon.entropy(prob)
  s_q = shannon.entropy(cc)
  s_pq = shannon.entropy((prob + cc)/2)
  divergence = sum(s_pq - (s_p/2) - (s_q/2))
  return(divergence)
}

# Statistical Complexity function ---------------------------------------------------------------------------

constant <- function(prob){
  k = (0.5)/length(prob)
  a1 = (0.5 + k) * log(0.5 + k)
  a2 = (length(prob) - 1) * k * log(k)
  a3 = (1 - 0.5) * log(length(prob))
  b = -1/(a1 + a2 + a3)
  return(b)
}

Ccomplexity<-function(prob){
  cc = jensen.divergence(prob) * constant(prob) * shannon.entropy.normalized(prob)
  return(cc)
}

# Trozos functions ---------------------------------------------------------------------------

cotas <- function(dimension, option = 0){
  
  if(option == 0){
    cx = readingMPR(dimension,1)
    cy = readingMPR(dimension,2)
    
  }else{
    cx = readingMPR(dimension,3)
    cy = readingMPR(dimension,4)
    
  }
  cotas.data = data.frame("cx" = cx, "cy" = cy)
  return(cotas.data)
}


readingMPR<-function(dimension, option=0){
  if(dimension == 3){ 
    continua = "../Data/trozos/continuaN6.txt"
    trozo = "../Data/trozos/trozosN6.txt"
  }
  if(dimension == 4){ 
    continua = "../Data/trozos/continuaN24.txt"
    trozo = "../Data/trozos/trozosN24.txt"
  }
  if(dimension == 5){ 
    continua = "../Data/trozos/continuaN120.txt"
    trozo = "../Data/trozos/trozosN120.txt"
  }
  if(dimension == 6){ 
    continua = "../Data/trozos/continuaN720.txt"
    trozo = "../Data/trozos/trozosN720.txt"
  }
  if(dimension == 36){ 
    continua = "../Data/trozos/continuaN36.txt"
    trozo = "../Data/trozos/trozosN36.txt"
  }
  if(dimension == 576){ 
    continua = "../Data/trozos/continuaN576.txt"
    trozo = "../Data/trozos/trozosN576.txt"
  }
  if(dimension == 14400){ 
    continua = "../Data/trozos/continuaN14400.txt"
    trozo = "../Data/trozos/trozosN14400.txt"
  }
  if(dimension == 518400){ 
    continua = "../Data/trozos/continuaN518400.txt"
    trozo = "../Data/trozos/trozosN518400.txt"
  }
  curva1x = read.table(continua, stringsAsFactors=FALSE, fileEncoding="latin1")[,1]
  if(option==1) return(curva1x)
  curva1y = read.table(continua, stringsAsFactors=FALSE, fileEncoding="latin1")[,2]
  if(option==2) return(curva1y)
  curva2x = read.table(trozo, stringsAsFactors=FALSE, fileEncoding="latin1")[,1]
  if(option==3) return(curva2x)
  curva2y = read.table(trozo, stringsAsFactors=FALSE, fileEncoding="latin1")[,2]
  if(option==4) return(curva2y)
}

histogram <- function(series, D, tau = 1){
    fat = factorial(D)
    p.patterns = formationPattern(series, D, tau, 0)
    n.symbols = dim(p.patterns)[1]
    symbol = define.symbols(D)
    index.rep = array(0, n.symbols)
    for(i in 1:n.symbols){
      for(j in 1:fat){
        if(all(p.patterns[i,] == symbol[j, ])){
          index.rep[i]=j
          break
        }
      }
    }
    index.rep = index.rep[1:n.symbols]
    index.rep = data.frame(i = index.rep)
    p = ggplot(index.rep) +
      geom_histogram(aes(x = i, y = ..density..),
                     binwidth = 1, fill = "grey", color = "black")+ 
      labs(x="Patterns", y="Probability") +
      theme_few(base_size = 14, base_family = "serif") 
  return(p)
}
