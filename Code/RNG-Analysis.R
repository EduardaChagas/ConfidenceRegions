########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 30, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Test-point.R")
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

# Analysis functions -----------------------------------------------------------------------------------
RNG.HC.confidence.regions <- function(D = 3, N = 1000, horizontal = 1){
  
  if(N == 1000){
    filePath <- read.table(paste0("../../Random_/Random_", 1, "k_D", D, "-T1.dat"), header=TRUE)
  }else{
    filePath <- read.table(paste0("../../Random_/Random_", 50, "k_D", D, "-T1.dat"), header=TRUE)
  }
  
  HC <- matrix(data = filePath$x, nrow = 100, ncol = 3, byrow = TRUE, dimnames = NULL)
  
  HC <- data.frame(H = HC[,1], 
                   C = HC[,2], 
                   D = factor(c(rep(D, 100))))
  
  result.95 = test.set.point(HC, D, N, 95)
  result.99 = test.set.point(HC, D, N, 99)
  
  HC <- melt(data = HC, id.vars = c(1, 2), variable.name = "D")
  
  regions <- read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))
  
  rect.95 <- data.frame(H = c(regions$H[5:8]), 
                        C = c(regions$C[5:8]), 
                        D = factor(c(rep(D, 4))))
  rect.95 <- melt(data = rect.95, id.vars = c(1, 2), variable.name = "D")
  
  rect.99 <- data.frame(H = c(regions$H[9:12]), 
                        C = c(regions$C[9:12]), 
                        D = factor(c(rep(D, 4))))
  rect.99 <- melt(data = rect.99, id.vars = c(1, 2), variable.name = "D")
  
  median <- data.frame(H = c(regions$H[17]), 
                       C = c(regions$C[17]), 
                       D = factor(D))
  median <- melt(data = median, id.vars = c(1, 2), variable.name = "D")
  
  ggplot(data = HC, aes(x = H, y = C)) +
    geom_point(size = 1, alpha = .9) +
    geom_polygon(data=rect.95, aes(x = H, y = C), fill="red", alpha=0.5, inherit.aes = FALSE) +
    geom_polygon(data=rect.99, aes(x = H, y = C), fill="green", alpha=0.3, inherit.aes = FALSE) +
    geom_point(data = median, aes(x = H, y = C), colour = "red", size = 1) +
    xlab("") + ylab("") + 
    theme_few(base_size = 25, base_family = "serif") +  
    theme(plot.title = element_text(hjust=0.5)) -> p
  
  xlab = ""
  if(horizontal == 1)
    xlab = bquote(D==.(D))
  
  p = p + ggtitle(xlab)
  return(p)
}

# Analysis with N = 1k ---------------------------------------------------------------------------------
p1 = RNG.HC.confidence.regions(D = 3, N = 1000)
print(p1)

p2 = RNG.HC.confidence.regions(D = 4, N = 1000)
print(p2)

p3 = RNG.HC.confidence.regions(D = 5, N = 1000)
print(p3)

p4 = RNG.HC.confidence.regions(D = 6, N = 1000)
print(p4)

pdf("RNG-1000.pdf", width = 32, height = 10)
ggarrange(p1, p2, p3, p4, ncol = 4, nrow = 1, common.legend = TRUE, legend = "right") + 
  #ggtitle(TeX("\\textit{H} $ \\times $ \\textit{C Plane}")) +
  ggtitle(TeX("$ \\textit{N} = 1000 $")) +
  xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
  theme_few() + theme(text=element_text(size=25, family="Times"), plot.title = element_text(hjust=0.5)) + 
  guides(colour = guide_legend(override.aes = list(size=3)))
dev.off()

# Analysis with N = 50k --------------------------------------------------------------------------------
p1 = RNG.HC.confidence.regions(D = 3, N = 50000)
print(p1)

p2 = RNG.HC.confidence.regions(D = 4, N = 50000)
print(p2)

p3 = RNG.HC.confidence.regions(D = 5, N = 50000)
print(p3)

p4 = RNG.HC.confidence.regions(D = 6, N = 50000)
print(p4)

pdf("RNG-50000.pdf", width = 32, height = 10)
ggarrange(p1, p2, p3, p4, ncol = 4, nrow = 1, common.legend = TRUE, legend = "right") + 
  #ggtitle(TeX("\\textit{H} $ \\times $ \\textit{C Plane}")) +
  ggtitle(TeX("$ \\textit{N} = 50000 $")) +
  xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
  theme_few() + theme(text=element_text(size=25, family="Times"), plot.title = element_text(hjust=0.5)) + 
  guides(colour = guide_legend(override.aes = list(size=3)))
dev.off()