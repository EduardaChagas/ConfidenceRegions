################################################################################
# Author: Eduarda Chagas
# Date : Out 9, 2021
# Contact: eduarda.chagas@dcc.ufmg.br
################################################################################

# Packages and sources ---------------------------------------------------------
source("Bandt-Pompe.R")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")

series.generator.map <- function(r){
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

r = 3.56995
D = 3
tau = 1

ts = read.csv("../Data/Emblematic-serie-D3-N50000.csv")[,2]
logistic_map = series.generator.map(r)

P = seq(from = 0.999, to = 1, length.out = 10)
n_p = length(P)
h = rep(0, n_p)
c = rep(0, n_p)

for(i in 1:n_p){
  ts_mixture = (P[i] * ts) + ((1 - P[i]) * logistic_map)
  probs_mixture = bandt.pompe(ts_mixture, D, tau)
  h[i] = round(shannon.entropy.normalized(probs_mixture), 10)
  c[i] = round(Ccomplexity(probs_mixture), 10)
}

hc_df = data.frame(H = h, C = c, P = P)
legend.names = as.character(P)

cotas.sup = data.frame("c1x" = readingMPR(D, 1), "c1y" = readingMPR(D, 2))
cotas.inf = data.frame("c2x" = readingMPR(D, 3), "c2y" = readingMPR(D, 4))

p = ggplot(data = hc_df, aes(x = H, y = C)) +
  geom_line(data = cotas.sup, aes(x = c1x, y = c1y), color="gray") +
  geom_line(data = cotas.inf, aes(x = c2x, y = c2y), color="gray") + 
  geom_label_repel(aes(label = paste("italic(p) ==", legend.names)),parse = TRUE, segment.size = 0.5, min.segment.length = 0, force = 18) +
  geom_line(linetype = "dotted") +
  geom_point(size = 1.5, alpha = .4) + 
  ggtitle(expression(italic('Time Series Mixture'))) +
  xlim(limits = c(min(hc_df$H), max(hc_df$H))) + 
  ylim(limits = c(min(hc_df$C), max(hc_df$C))) + 
  xlab(expression(italic(H))) +
  ylab(expression(italic(C))) +
  theme_few(base_size = 20, base_family = "serif")  + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
print(p)
