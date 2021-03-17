########################################################################################################
# Author: Eduarda Chagas
# Date : Jul 12, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Test-point.R")
source("Bandt-Pompe.R")
source("Level-confidence-region.R")
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
} 
if(!require(ggrepel)){
  install.packages("ggrepel")
  require(ggrepel)
} 
if(!require(ggthemes)){
  install.packages("ggthemes")
  require(ggthemes)
} 

theme_set(theme_clean())

# Trozos plot ---------------------------------------------------------------------------------------
plot.cotas <- function(D){
  c1x = readingMPR(D,1)
  c1y = readingMPR(D,2)
  c2x = readingMPR(D,3)
  c2y = readingMPR(D,4)
  
  cotas.1xy = data.frame("c1x" = c1x, "c1y" = c1y)
  cotas.2xy = data.frame("c2x" = c2x, "c2y" = c2y)
  
  p = ggplot(cotas.1xy, aes(c1x, c1y)) + 
    geom_line(size = 0.5, color = "gray") +
    geom_line(aes(x = c2x, y = c2y), cotas.2xy, size = 0.5, color = "gray") +
    theme(plot.title = element_text(hjust = 0.5)) 
  return(p)
}

# Ramp process -----------------------------------------------------------------------------------------
ramp.function <- function(N, n) {
  x <- 1:N
  y <- (x %% n ) / n
  return(y)
}

ramp.contamination.plot <- function(N, noise, ramp){
  df = data.frame(x = c(1:N), noise = noise, ramp = ramp)
  
  pdf("Ramp-contamination-D6-N50k.pdf", width = 7, height = 7)
  p = ggplot(df, aes(x = x, y = ramp)) +
      geom_line(col = "gray") +
      #geom_line(aes(x = x, y = ramp+noise), col = "red") +
      #geom_point(aes(x = x, y = ramp+noise), col = "red") +
      geom_line(aes(x = x, y = .5*ramp+.5*noise), col = "orange") +
      geom_point(aes(x = x, y = .5*ramp+.5*noise), col = "orange") +
      geom_line(aes(x = x, y = noise), col = "black") +
      geom_point(aes(x = x, y = noise), col = "black")
  print(p)
  dev.off()
}

# Deterministic Structure Analysis function ------------------------------------------------------------
Deterministic.analysis.point <- function(save.plot = FALSE){
  
  D = 6
  N = 50000
  ts = read.csv("../Data/Emblematic-series-D6-N50000.csv")[,2]
  
  x = seq(1, N, 1)
  y = ramp.function(N, 100)
  a = seq(0, 0.4, length.out = 11)
  legend.names = paste0(round(a, 3))
  df = data.frame(x = x, noise = ts, ramp = y)
  
  #ramp.contamination.plot(100, ts[1:100], y[1:100])
  #histogram(ts, 6, 1)
  
  HC.values = data.frame(H = length(a), C = length(a), A = as.factor(a))
  
  for(i in 1:length(a)){
    result.ts = (a[i] * df$ramp) + ((1-a[i]) * df$noise)
    probs = bandt.pompe(result.ts, D, 1)
    h = shannon.entropy.normalized(probs)
    c = Ccomplexity(probs)
    HC.values$H[i] = h
    HC.values$C[i] = c
  }
  
  #Confidence regions at the 95% and 99%
  hc.points = read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
  rect95 = data.frame(H = hc.points$H[5:8], C = hc.points$C[5:8])
  rect99 = data.frame(H = hc.points$H[9:12], C = hc.points$C[9:12])
  
  #p = plot.cotas(D)
  #p = p +
  p = ggplot(data = HC.values, aes(x = H, y = C, color = A)) +
    ggtitle("Deterministic Structure Analysis") +
    geom_label_repel(aes(label = legend.names), box.padding = 0.3, point.padding = 0.45, 
                     segment.color = 'grey50', size = 2.5, parse = TRUE) +
    geom_point(data = HC.values, aes(x = H, y = C), shape = 16, size = 2.5, alpha = .4) +
    geom_polygon(data=rect95, aes(x = H, y = C), fill="red", alpha=0.4, inherit.aes = FALSE) +
    geom_polygon(data=rect99, aes(x = H, y = C), fill="green", alpha=0.2, inherit.aes = FALSE) +
    geom_point(data = HC.values, aes(x = H, y = C, color = A), size = 2, alpha = I(0.8)) + 
    xlim(limits = c(min(HC.values$H, rect95$H, rect99$H), max(HC.values$H, rect95$H, rect99$H))) + ylim(limits=c(0.002, max(HC.values$C, rect95$C, rect99$C))) + 
    #xlim(limits=c(0, 1)) + ylim(limits=c(0, 0.18)) + 
    xlab("H") +
    ylab("C") +
    theme_few(base_size = 12, base_family = "serif")  +
    theme(plot.title = element_text(hjust=0.5))
  print(p)
  
  if(save.plot == TRUE){
    pdf("Deterministic-Analysis.pdf", width = 6, height = 4)
    print(p)
    dev.off()
  }
  return(p)
}

Deterministic.analysis.point(save.plot = TRUE)
