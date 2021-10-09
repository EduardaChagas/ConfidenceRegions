################################################################################
# Author: Eduarda Chagas
# Date : Out 7, 2021
# Contact: eduarda.chagas@dcc.ufmg.br
################################################################################

# Packages and sources ---------------------------------------------------------
source("Bandt-Pompe.R")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")

# Define attack ----------------------------------------------------------------
set.seed(123)
AttackElement <- function(e, d, p){
  if(runif(1) <= p){
    i = round(runif(1, max = d-1, min = 1), digits = 0)
    e[i+1] = e[i] 
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
tau = 1
P = c(0.001, 0.005, 0.01, 0.015, 0.02, 0.025, 0.03) 

h = rep(0, length(P) + 1)
c = rep(0, length(P) + 1)

ts = read.csv("../Data/Emblematic-serie-D3-N50000.csv")[,2]
elements = formationPattern(ts, D, tau, 1)
probs.ts = bandt.pompe(ts, D, tau)
h[1] = shannon.entropy.normalized(probs.ts)
c[1] = Ccomplexity(probs.ts)

for(i in 1:length(P)) {
  cat("i:" , i," P: ", P[i], "\n")
  x_attack = AttackTimeSeries(ts, D, tau, P[i])
  #x_attack = AttackPattern(elements, D, tau, P[i])
  probs_attacks = bandt.pompe(x_attack, D, tau)
  #probs_attacks = bandt.pompe.by.elements(x_attack, D, tau)
  h[i+1] = round(shannon.entropy.normalized(probs_attacks), 10)
  c[i+1] = round(Ccomplexity(probs_attacks), 10)
}

P = c(0, P)

hc_df = data.frame(H = h, C = c, P = P)
                   
legend.names = as.character(P)

cotas.sup = data.frame("c1x" = readingMPR(D, 1), "c1y" = readingMPR(D, 2))
cotas.inf = data.frame("c2x" = readingMPR(D, 3), "c2y" = readingMPR(D, 4))

p = ggplot(data = hc_df, aes(x = H, y = C)) +
  geom_line(data = cotas.sup, aes(x = c1x, y = c1y), color="gray") +
  geom_line(data = cotas.inf, aes(x = c2x, y = c2y), color="gray") + 
  geom_label_repel(aes(label = paste("italic(p) ==", legend.names)),parse = TRUE, segment.size = 0.5, min.segment.length = 0, force = 18) +
  geom_line(linetype="dotted") +
  geom_point(size = 1.5, alpha = .4) + 
  ggtitle(expression(italic('Time Series Attacked'))) +
  #ggtitle(expression(italic('Patterns Attacked'))) +
  xlim(limits = c(min(hc_df$H), max(hc_df$H))) + 
  ylim(limits=c(min(hc_df$C), max(hc_df$C))) + 
  xlab(expression(italic(H))) +
  ylab(expression(italic(C))) +
  theme_few(base_size = 20, base_family = "serif")  + 
  theme(plot.title = element_text(hjust=0.5), legend.position="none")
print(p)
