########################################################################################################
# Author: Eduarda Chagas
# Date : Jul 24, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Bandt-Pompe.R")
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(ggthemes)){
  install.packages("ggthemes")
  require(ggthemes)
}

generate.cross.validation.plots <- function(region, D, N, k){
  k = k + 1
  
  cotas.sup = data.frame("c1x" = readingMPR(D, 1), "c1y" = readingMPR(D, 2))
  cotas.inf = data.frame("c2x" = readingMPR(D, 3), "c2y" = readingMPR(D, 4))
  
  rect90 = rect95 = rect99 = rect999 = median = hc = array(list(), k)
  hc[[1]] = read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
  for(i in 2:k){
    if(N == 50000)
      hc[[i]] = read.csv(paste0("../Data/Regions-HC/N", 50, "kD", D, "/hc", (i-1), ".csv"))[2:4]
    else
      hc[[i]] = read.csv(paste0("../Data/Regions-HC/N", N, "D", D, "/hc", (i-1), ".csv"))[2:4]
  }
  for(i in 1:k){
    rect90[[i]] = data.frame(H = hc[[i]]$H[1:4], C = hc[[i]]$C[1:4])
    rect95[[i]] = data.frame(H = hc[[i]]$H[5:8], C = hc[[i]]$C[5:8])
    rect99[[i]] = data.frame(H = hc[[i]]$H[9:12], C = hc[[i]]$C[9:12])
    rect999[[i]] = data.frame(H = hc[[i]]$H[13:16], C = hc[[i]]$C[13:16])
    median[[i]] = data.frame(H = hc[[i]]$H[17], C = hc[[i]]$C[17])
  }
  
  p = ggplot() +
    ggtitle(paste0(region, "% of confidence")) +
    geom_point(size = .3, alpha = .4) 
  
  if(region == 90)
    rect = rect90
  else if(region == 95)
    rect = rect95
  else if(region == 99)
    rect = rect99
  else if(region == 999)
    rect = rect999
  
  for(i in 2:k){
    p = p + 
      geom_polygon(data = rect[[i]], aes(x = H, y = C), 
                   fill = "red", alpha = 0.1, inherit.aes = FALSE) 
  }
  p = p + 
    geom_polygon(data = rect[[1]], aes(x = H, y = C), 
                 fill = "blue", alpha = 0.7, inherit.aes = FALSE) 
  
  lim.x.min = lim.y.min = 1
  lim.x.max = lim.y.max = 0
  for(i in 1:k){
    lim.x.min = min(lim.x.min, rect[[i]]$H)
    lim.y.min = min(lim.y.min, rect[[i]]$C)
    lim.x.max = max(lim.x.max, rect[[i]]$H)
    lim.y.max = max(lim.y.max, rect[[i]]$C)
  }
  p = p +
    #geom_line(data = cotas.sup, aes(x = c1x, y = c1y), color="gray") +
    #geom_line(data = cotas.inf, aes(x = c2x, y = c2y), color="gray") + 
    xlim(limits = c(lim.x.min, lim.x.max)) + 
    ylim(limits = c(lim.y.min, lim.y.max)) + 
    xlab("H") + 
    ylab("C") +
    theme_few() +
    theme(plot.title = element_text(hjust=0.5)) 
  
  pdf(paste0(region, "-confidence-HC.pdf"), width = 8, height = 6)
  print(p)
  dev.off()
}

generate.cross.validation.plots(90, 6, 50000, 10)
generate.cross.validation.plots(95, 6, 50000, 10)
generate.cross.validation.plots(99, 6, 50000, 10)
generate.cross.validation.plots(999, 6, 50000, 10)