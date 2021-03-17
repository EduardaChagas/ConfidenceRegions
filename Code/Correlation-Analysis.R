########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 20, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Test-point.R")
source("Bandt-Pompe.R")
source("Level-confidence-region.R")
if(!require(fftw)) install.packages("fftw")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(latex2exp)) install.packages("latex2exp")

# Generator of noise with power spectrum f^-k --------------------------------------------------------
set.seed(seed = 123, kind = "Mersenne-Twister")

series.generator.fk <- function(pp, y, n, k){
  Series = vector(mode="numeric")
  filtro = (1:n)^-(k/2)
  filtro = filtro / sum(filtro)
  y1 = y * filtro    
  x1 = IFFT(y1, plan = pp)  
  Series = c(Re(x1)) 
  return(Series)
}

# Correlation Structure Analysis function --------------------------------------------------------------
correlation.analysis.point <- function(){
  
  rainbow_colors = palette(c("#fca311",#amarelo
                             "#000000", #preto
                             "#ff006e", #rosa
                             "#40916c", #verde
                             "#6a040f", #vermelho
                             "#eb5e28", #laranja
                             "#6a4c93", #roxo
                             "#50514f", #cinza
                             "#5e3023", #marrom
                             "#3f88c5", #azul
                             "#60d394"))
  D = 6
  N = 50000
  k = c(0, 0.1, 0.2, 0.3)
  
  ts = read.csv("../Data/Emblematic-series-D6-N50000.csv")[,2]
  probs.ts = bandt.pompe(ts, 6, 1)
  h.ts = shannon.entropy.normalized(probs.ts)
  c.ts = Ccomplexity(probs.ts)
  HC.ts = data.frame(H = h.ts, C = c.ts)
  
  HC.values = data.frame(H = length(k), C = length(k), K = as.factor(k), names = as.factor(k))
  
  pp = planFFT(50000)
  y = FFT(ts, plan = pp)
  for(i in 1:length(k)){
    #f^-k 
    fk = series.generator.fk(pp, y, 50000, k[i])
    probs = bandt.pompe(fk, 6, 1)
    h = shannon.entropy.normalized(probs)
    c = Ccomplexity(probs)
    
    HC.values$H[i] = h
    HC.values$C[i] = c
  }
  HC.values = HC.values[order(k),]
  legend.names = k[order(k)]
  HC.values$names <- legend.names
  
  #Confidence regions at the 95% and 99%
  hc.points = read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
  rect95 = data.frame(H = hc.points$H[5:8], C = hc.points$C[5:8])
  rect99 = data.frame(H = hc.points$H[9:12], C = hc.points$C[9:12])
  
  cotas.sup = data.frame("c1x" = readingMPR(D, 1), "c1y" = readingMPR(D, 2))
  cotas.inf = data.frame("c2x" = readingMPR(D, 3), "c2y" = readingMPR(D, 4))
  
  pdf("../Reports/JOURNAL - Confidence Regions/Figures/Correlation-Analysis-point.pdf", width = 6, height = 4)
  p = ggplot(data = HC.values, aes(x = H, y = C)) +
    geom_polygon(data=rect95, aes(x = H, y = C), fill="red", alpha=0.4, inherit.aes = FALSE) +
    geom_polygon(data=rect99, aes(x = H, y = C), fill="green", alpha=0.2, inherit.aes = FALSE) +
    geom_line(data = cotas.sup, aes(x = c1x, y = c1y), color="gray") +
    geom_line(data = cotas.inf, aes(x = c2x, y = c2y), color="gray") + 
    geom_point(size = 1.5, alpha = .4) + 
    geom_point(data = HC.ts, aes(x = H, y = C), size = 1.5, alpha = .4, color = "red") + 
    geom_label_repel(aes(label = paste("italic(k) ==", legend.names)),parse = TRUE, segment.size = 0.5, min.segment.length = 0, force = 18) +
    scale_color_manual(values = rainbow_colors) +
    xlab(expression(italic(H))) +
    ylab(expression(italic(C))) +
    xlim(limits = c(min(HC.values$H, rect95$H, rect99$H), max(HC.values$H, rect95$H, rect99$H))) + 
    ylim(limits=c(0.002, max(HC.values$C, rect95$C, rect99$C))) + 
    theme_few(base_size = 20, base_family = "sans")  + 
    theme(plot.title = element_text(hjust=0.5), legend.position="none")
  print(p)
  dev.off()
}


correlation.analysis.dotted <- function(){
  
  rainbow_colors = palette(c("#fca311",#amarelo
                             "#000000", #preto
                             "#ff006e", #rosa
                             "#40916c", #verde
                             "#6a040f", #vermelho
                             "#eb5e28", #laranja
                             "#6a4c93", #roxo
                             "#50514f", #cinza
                             "#5e3023", #marrom
                             "#3f88c5", #azul
                             "#60d394"))
  D = 6
  N = 50000
  k = c(3, 2.5, 2, 1.5, 1, 0.5, 0)
  ts = read.csv("../Data/Emblematic-series-D6-N50000.csv")[,2]
  
  HC.values = data.frame(H = length(k), C = length(k), K = as.factor(k), names = as.factor(k))
  
  pp = planFFT(50000)
  y = FFT(ts, plan = pp)
  for(i in 1:length(k)){
    #f^-k 
    fk = series.generator.fk(pp, y, 50000, k[i])
    probs = bandt.pompe(fk, 6, 1)
    h = shannon.entropy.normalized(probs)
    c = Ccomplexity(probs)
    
    HC.values$H[i] = h
    HC.values$C[i] = c
  }
  HC.values = HC.values[order(k),]
  legend.names = k[order(k)]
  HC.values$names <- legend.names
  
  #Confidence regions at the 95% and 99%
  hc.points = read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
  rect95 = data.frame(H = hc.points$H[5:8], C = hc.points$C[5:8])
  rect99 = data.frame(H = hc.points$H[9:12], C = hc.points$C[9:12])
  
  cotas.sup = data.frame("c1x" = readingMPR(D, 1), "c1y" = readingMPR(D, 2))
  cotas.inf = data.frame("c2x" = readingMPR(D, 3), "c2y" = readingMPR(D, 4))
  
  pdf("../Reports/JOURNAL - Confidence Regions/Figures/Correlation-Analysis-dotted.pdf", width = 6, height = 4)
  p = ggplot(data = HC.values, aes(x = H, y = C)) +
    geom_polygon(data=rect95, aes(x = H, y = C), fill="red", alpha=0.4, inherit.aes = FALSE) +
    geom_polygon(data=rect99, aes(x = H, y = C), fill="green", alpha=0.2, inherit.aes = FALSE) +
    geom_line(data = cotas.sup, aes(x = c1x, y = c1y), color="gray") +
    geom_line(data = cotas.inf, aes(x = c2x, y = c2y), color="gray") + 
    geom_label_repel(aes(label = paste("italic(k) ==", legend.names)),parse = TRUE, segment.size = 0.5, min.segment.length = 0, force = 18) +
    geom_line(linetype="dotted") +
    geom_point(size = 1.5, alpha = .4) + 
    scale_color_manual(values = rainbow_colors) +
    xlim(limits = c(min(HC.values$H, rect95$H, rect99$H), max(HC.values$H, rect95$H, rect99$H))) + 
    ylim(limits=c(0.002, max(HC.values$C, rect95$C, rect99$C))) + 
    xlab(expression(italic(H))) +
    ylab(expression(italic(C))) +
    theme_few(base_size = 20, base_family = "sans")  + 
    theme(plot.title = element_text(hjust=0.5), legend.position="none")
  print(p)
  dev.off()
}

correlation.analysis.point()
correlation.analysis.dotted()