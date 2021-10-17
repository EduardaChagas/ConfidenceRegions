################################################################################
# Author: Eduarda Chagas
# Date : Out 9, 2021
# Contact: eduarda.chagas@dcc.ufmg.br
################################################################################

# Packages and sources ---------------------------------------------------------
source("Bandt-Pompe.R")
source("Test-point.R")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(nonlinearTseries)) install.packages("nonlinearTseries")


logistic.map.generator <- function(r){
  index = 1
  time_series = vector(mode = "numeric", length = 50000)
  
  time_series[index] <- 0.1
  for(i in 2:(10000 + 50000)){
    time_series[index] = r * time_series[index] * (1 - time_series[index])
    if(i > 10001){
      index = index + 1
      time_series[index] = r * time_series[index-1] * (1 - time_series[index-1])
    }
  }
  return(time_series)
}

quadratic.map.generator <- function(c){
  index = 1
  time_series = vector(mode = "numeric", length = 50000)
  
  time_series[index] <- 0.1
  for(i in 2:(10000 + 50000)){
    time_series[index] = (time_series[index]^2) + c
    if(i > 10001){
      index = index + 1
      time_series[index] = (time_series[index - 1]^2) + c
    }
  }
  return(time_series)
}

r = 3.56995
c = -2
D = 3
N = 50000
tau = 1

ts = read.csv("../Data/Minimum-serie-D3-N50000.csv")[,2]
ts = round(ts, 6)
logistic_map = logistic.map.generator(r)

P = seq(from = 0.99, to = 1, length.out = 20)
n_p = length(P)
h_logistic = rep(0, n_p)
c_logistic  = rep(0, n_p)

for(i in 1:n_p){
  ts_mixture_logistic = (P[i] * ts) + ((1 - P[i]) * logistic_map)
  probs_mixture_logistic = bandt.pompe(ts_mixture_logistic, D, tau)
  h_logistic[i] = round(shannon.entropy.normalized(probs_mixture_logistic), 5)
  c_logistic[i] = round(Ccomplexity(probs_mixture_logistic), 5)
  #cat("i: ", i, 
  #    " 90%: ", test.point(h[i], c[i], D, N, 90), 
  #    " 95%: ", test.point(h[i], c[i], D, N, 95), 
  #    " 99%: ", test.point(h[i], c[i], D, N, 99), "\n")
}

hc_df_logistic = data.frame(H = h_logistic, C = c_logistic, P = P)

legend.names = as.character(P)

cotas.sup = data.frame("c1x" = readingMPR(D, 1), "c1y" = readingMPR(D, 2))
cotas.inf = data.frame("c2x" = readingMPR(D, 3), "c2y" = readingMPR(D, 4))

#Confidence regions at the 95% and 99%
hc.points = read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
rect95 = data.frame(H = hc.points$H[5:8], C = hc.points$C[5:8])
rect99 = data.frame(H = hc.points$H[9:12], C = hc.points$C[9:12])

p = ggplot(data = round(hc_df_logistic, 6), aes(x = H, y = C)) +
  geom_polygon(data = rect95, aes(x = H, y = C), fill = "red", alpha=0.9, inherit.aes = FALSE) +
  geom_polygon(data = rect99, aes(x = H, y = C), fill = "green", alpha=0.2, inherit.aes = FALSE) +
  geom_label_repel(aes(label = paste("italic(p) ==", legend.names)),parse = TRUE, segment.size = 0.5, min.segment.length = 0, force = 18) +
  #geom_line(linetype = "dotted") +
  geom_point(size = 1.5, alpha = .4, color = "pink") +  
  ggtitle(expression(italic('Time Series Mixture'))) +
  xlim(limits = c(min(rect99$H), max(rect99$H))) + 
  ylim(limits = c(min(rect99$C), max(rect99$C))) + 
  xlab(expression(italic(H))) +
  ylab(expression(italic(C))) +
  theme_few(base_size = 20, base_family = "serif")  + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
print(p)

#write.csv(hc_df, "../Data/mixture_caos_emblematic_D3.csv")
