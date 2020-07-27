########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 30, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Confidence-regions-functions.R")
source("Bandt-Pompe.R")
if(!require(ggpubr)){
  install.packages("ggpubr")
  require(ggpubr)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(ggthemes)){
  install.packages("ggthemes")
  require(ggthemes)
}
if(!require(reshape2)){
  install.packages("reshape2")
  require(reshape2)
}
if(!require(latex2exp)){
  install.packages("latex2exp")
  require(latex2exp)
}

plot.HC.PCA.trozos <- function(N, dim, lengend ,x.lim = NULL, y.lim = NULL){
  if(N == 1000){
    HC = read.csv("../Data/HC/HC_1000.csv")[,2:4]
  }else if(N == 50000){
    HC = read.csv("../Data/HC/HC_50k.csv")[,2:4]
  }
  
  names(HC) <- c("H", "C", "D")
  HC$D <- as.factor(HC$D)
  HC.D <- subset(HC, D == dim)[,-3]
  HC.D <- HC.D[,-3]
  PCA.HC.D <- prcomp(x = HC.D[,-3], 
                     retx = TRUE, 
                     center = TRUE, 
                     scale. = TRUE)
  
  # Trozos data ----------------------------
  cotas.sup <- cotas(dim, 0)
  cotas.sup <- data.frame("H" = cotas.sup$cx, 'C' = cotas.sup$cy)
  cotas.inf <- cotas(dim, 1)
  cotas.inf <- data.frame("H" = cotas.inf$cx, 'C' = cotas.inf$cy)
  
  index.i = which(cotas.sup$H >= min(HC.D$H) & cotas.sup$H <= 1)
  sup = cotas.sup[index.i,]
  sup = data.frame(H = sup$H, C = predict(lm(data = sup, formula = C ~ poly(H, 3))))
  cotas.sup = data.frame(H = c(cotas.sup[-index.i,]$H, sup$H), 
                         C = c(cotas.sup[-index.i,]$C, sup$C))
  
  index.i = which(cotas.inf$H >= min(HC.D$H) & cotas.inf$H <= 1)
  inf = cotas.inf[index.i,]
  inf = data.frame(H = inf$H, C = predict(lm(data = inf, formula = C ~ poly(H, 3))))
  cotas.inf = data.frame(H = c(cotas.inf[-index.i,]$H, inf$H), 
                         C = c(cotas.inf[-index.i,]$C, inf$C))
  
  cotas.sup <- predict(PCA.HC.D, newdata = cotas.sup)
  cotas.sup <- data.frame(H = (cotas.sup[,1]), C = (cotas.sup[,2]))
  
  cotas.inf <- predict(PCA.HC.D, newdata = cotas.inf)
  cotas.inf <- data.frame(H = (cotas.inf[,1]), C = (cotas.inf[,2]))
  
  # PCA data -------------------------------
  PCA.HC.D <- data.frame(PC1 = PCA.HC.D$x[,1], PC2 = PCA.HC.D$x[,2],
                         D = factor(rep(dim, length(PCA.HC.D$x[,2]))))
  #Confidence regions data -----------------
  regions.D.N <- read.csv(paste0("../Data/Regions-PCA/regions-pca-D", dim, "-N", N, ".csv"))
  rect.90 <- data.frame(xmin = regions.D.N$xmin[1], 
                        xmax = regions.D.N$xmax[1], 
                        ymin = regions.D.N$ymin[1], 
                        ymax = regions.D.N$ymax[1],
                        D = factor(dim))
  rect.95 <- data.frame(xmin = regions.D.N$xmin[2], 
                        xmax = regions.D.N$xmax[2], 
                        ymin = regions.D.N$ymin[2], 
                        ymax = regions.D.N$ymax[2],
                        D = factor(dim))
  rect.99 <- data.frame(xmin = regions.D.N$xmin[3], 
                        xmax = regions.D.N$xmax[3], 
                        ymin = regions.D.N$ymin[3], 
                        ymax = regions.D.N$ymax[3],
                        D = factor(dim))
  rect.999 <- data.frame(xmin = regions.D.N$xmin[4], 
                         xmax = regions.D.N$xmax[4], 
                         ymin = regions.D.N$ymin[4], 
                         ymax = regions.D.N$ymax[4],
                         D = factor(dim))
  median <- data.frame(PC1 = regions.D.N$xmin[5], 
                       PC2 = regions.D.N$ymax[5], 
                       D = factor(dim))
  
  ggplot(data = PCA.HC.D, aes(x = PC1, y = PC2)) +
    geom_point(size = .8, alpha = .4, color = "#757171") +
    geom_point(data = median, aes(x = PC1, y = PC2), colour = "red", size = 1) +
    geom_line(data = cotas.inf, aes(x = H, y = C), color = "gray", size = .8) +
    geom_line(data = cotas.sup, aes(x = H, y = C), color = "gray", size = .8) + 
    geom_rect(data = rect.90, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.2, inherit.aes = FALSE) +
    geom_rect(data = rect.95, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              fill = "blue", alpha = 0.2, inherit.aes = FALSE) +
    geom_rect(data = rect.99, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              fill = "green", alpha = 0.2, inherit.aes = FALSE) +
    geom_rect(data = rect.999, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              fill = "yellow", alpha = 0.3, inherit.aes = FALSE) +
    #scale_color_discrete(name = expression(italic(D))) +
    #ggtitle(expression(italic(legend))) +
    xlab(expression(italic(PC1))) + ylab(expression(italic(PC2))) + 
    theme_few(base_size = 18, base_family = "serif") +  
    theme(plot.title = element_text(hjust=0.5)) -> p
  
  if(is.null(x.lim)){
    p <- p + xlim(limits = c(min(min(PCA.HC.D$PC1), min(rect.90$xmin, rect.95$xmin, rect.99$xmin, rect.999$xmin)), 
                             max(max(PCA.HC.D$PC1), max(rect.90$xmax, rect.95$xmax, rect.99$xmax, rect.999$xmax)))) + 
      ylim(limits = c(min(min(PCA.HC.D$PC2), min(rect.90$ymin, rect.95$ymin, rect.99$ymin, rect.999$ymin)), 
                      max(max(PCA.HC.D$PC2), max(rect.90$ymax, rect.95$ymax, rect.99$ymax, rect.999$ymax)))) 
  }else{
    p <- p + xlim(limits = x.lim) + 
      ylim(limits = y.lim) 
  }
  return(p)
}

png("p1.png", width = 800, height = 500)
p1 = plot.HC.PCA.trozos(1000, 3)
p1 = p1 + ggtitle(expression(italic("N = 1000, D = 3")))
print(p1)
dev.off() 

png("p2.png", width = 800, height = 500)
p2 = plot.HC.PCA.trozos(1000, 6)
p2 = p2 + ggtitle(expression(italic("N = 1000, D = 6")))
print(p2)
dev.off() 

png("HC-PCA-Trozos-D3N50k.png", width = 800, height = 500)
p3 = plot.HC.PCA.trozos(50000, 3)
p3 = p3 + ggtitle(expression(italic("N = 50000, D = 3")))
print(p3)
dev.off() 

png("HC-PCA-Trozos-D6N50k.png", width = 800, height = 500)
p4 = plot.HC.PCA.trozos(50000, 6)
p4 = p4 + ggtitle(expression(italic("N = 50000, D = 6")))
print(p4)
dev.off() 

png("HC-PCA.png", width = 800, height = 1000)
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, label.x = expression(italic(PC1)), label.y = expression(italic(PC2))) + 
  ggtitle(TeX("\\textit{PCA Plane}")) +
  xlab(expression(italic(PC1))) + 
  ylab(expression(italic(PC2))) + 
  labs(colour=expression(italic(D))) +
  theme_few() + 
  theme(text=element_text(size=14, family="Times"),plot.title = element_text(hjust=0.5)) 
dev.off() 

pdf("HC-PCA-Trozos.pdf", width = 16, height = 10)
ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2, label.x = expression(italic(PC1)), label.y = expression(italic(PC2))) + 
  ggtitle(TeX("\\textit{PCA Plane}")) +
  xlab(expression(italic(PC1))) + 
  ylab(expression(italic(PC2))) + 
  labs(colour=expression(italic(D))) +
  theme_few() + 
  theme(text=element_text(size=14, family="Times"),plot.title = element_text(hjust=0.5)) 
dev.off() 
