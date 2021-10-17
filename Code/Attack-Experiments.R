################################################################################
# Author: Eduarda Chagas
# Date : Out 7, 2021
# Contact: eduarda.chagas@dcc.ufmg.br
################################################################################

# Packages and sources ---------------------------------------------------------
setwd("/home/eduarda/Desktop/Codes/Confidence Regions/ConfidenceRegions-master/Code")
source("Bandt-Pompe.R")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")

# Define attack ----------------------------------------------------------------
set.seed(123)
AttackElement <- function(e, d, p){
  if(runif(1) <= p){
    i = round(runif(1, max = d-1, min = 1), digits = 0)
    e[i] = e[i] - 0.002
    e[i+1] = e[i+1] + 0.002
  }
  return(e)
}

AttackPattern <- function(elements_ts, d, tau, p) {
  n_elements = dim(elements_ts)[1]
  attacked_elements = matrix(nrow = n_elements, ncol = d)
  for(i in 1:n_elements){
    elements = formationPattern(elements_ts[i,], d, tau, 1)
    attacked_elements[i,] = AttackElement(elements, d, p)
  }
  return(attacked_elements)
}

AttackTimeSeries <- function(time_series, d, tau, p) {
  ts_attacked = time_series
  n.series = length(time_series)
  for(i in 1:(n.series-1)){
    if(runif(1) <= p){
      ts_attacked[i+1] = ts_attacked[i] 
    }
  }
  return(ts_attacked)
}

bandt.pompe.by.elements <- function(elements, dimension, delay){
  dyn.load("BandtPompe.so")
  element.size = dim(elements)[1]
  probability <- .Call("BandtPompe", elements, dimension, element.size)
  return(probability)
}


#Global variables --------------------------------------------------------------
D = 3
N = 50000
tau = 1
P = seq(from = 0, to = 0.7, length.out = 20)

h = rep(0, length(P) + 1)
c = rep(0, length(P) + 1)

ts = read.csv("../Data/Minimum-serie-D3-N50000.csv")[,2]
elements = formationPattern(ts, D, tau, 1)
probs.ts = bandt.pompe(ts, D, tau)
h[1] = shannon.entropy.normalized(probs.ts)
c[1] = Ccomplexity(probs.ts)

for(i in 1:length(P)) {
  cat("i:" , i," P: ", P[i], "\n")
  #x_attack = AttackTimeSeries(ts, D, tau, P[i])
  x_attack = AttackPattern(elements, D, tau, P[i])
  #probs_attacks = bandt.pompe(x_attack, D, tau)
  probs_attacks = bandt.pompe.by.elements(x_attack, D, tau)
  h[i+1] = round(shannon.entropy.normalized(probs_attacks), 10)
  c[i+1] = round(Ccomplexity(probs_attacks), 10)
}

P = c(0, P)

hc_df = data.frame(H = h, C = c, P = P)
                   
legend.names = as.character(P)

cotas.sup = data.frame("c1x" = readingMPR(D, 1), "c1y" = readingMPR(D, 2))
cotas.inf = data.frame("c2x" = readingMPR(D, 3), "c2y" = readingMPR(D, 4))

#Confidence regions at the 95% and 99%
hc.points = read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
rect95 = data.frame(H = hc.points$H[5:8], C = hc.points$C[5:8])
rect99 = data.frame(H = hc.points$H[9:12], C = hc.points$C[9:12])

p = ggplot(data = round(hc_df, 6), aes(x = H, y = C)) +
  geom_polygon(data = rect95, aes(x = H, y = C), fill = "red", alpha=0.9, inherit.aes = FALSE) +
  geom_polygon(data = rect99, aes(x = H, y = C), fill = "green", alpha=0.2, inherit.aes = FALSE) +
  #geom_line(data = cotas.sup, aes(x = c1x, y = c1y), color="gray") +               
  #geom_line(data = cotas.inf, aes(x = c2x, y = c2y), color="gray") + 
  geom_label_repel(aes(label = paste("italic(p) ==", legend.names)),parse = TRUE, segment.size = 0.5, min.segment.length = 0, force = 18) +
  #geom_line(linetype = "dotted") +
  geom_point(size = 1.5, alpha = .4) + 
  #ggtitle(expression(italic('Time Series Attacked'))) +
  ggtitle(expression(italic('Patterns Attacked (delta permutation)'))) +
  #xlim(limits = c(min(hc_df$H), max(hc_df$H))) + 
  #ylim(limits = c(min(hc_df$C), max(hc_df$C))) + 
  xlim(limits = c(min(rect99$H), max(rect99$H))) + 
  ylim(limits = c(min(rect99$C), max(rect99$C))) + 
  xlab(expression(italic(H))) +
  ylab(expression(italic(C))) +
  theme_few(base_size = 20, base_family = "serif")  + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none")                                                             
print(p)
