########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 26, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Confidence-regions-functions.R")
source("PCA-Histogram.R")
source("Bandt-Pompe.R")
if(!require(reshape2)){
  install.packages("reshape2")
  require(reshape2)
}
if(!require(plot3D)){
  install.packages("plot3D")
  require(plot3D)
}
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

# Analysis with N = 1000 --------------------------------------------------------------------------------
HC = read.csv("../Data/HC/HC_1000.csv")[,2:4]
names(HC) = c("H", "C", "D")
HC$D = as.factor(HC$D)

HC.1000 <- melt(data = HC, id.vars = c(1,2), variable.name = "D")

cotas.sup = data.frame("c1x" = numeric(37814), "c1y" = numeric(37814), "D" = as.factor(c(rep(3, 8332), rep(4, 9582), rep(5, 9915), rep(6, 9985))))
cotas.sup$c1x = c(readingMPR(3, 1), readingMPR(4, 1), readingMPR(5, 1), readingMPR(6, 1))
cotas.sup$c1y = c(readingMPR(3, 2), readingMPR(4, 2), readingMPR(5, 2), readingMPR(6, 2))
cotas.sup = melt(data = cotas.sup, id.vars = c(1,2), variable.name = "D")

cotas.inf = data.frame("c2x" = numeric(147122), "c2y" = numeric(147122), "D" = as.factor(c(rep(3, 14499), rep(4, 27753), rep(5, 43641), rep(6, 61229))))
cotas.inf$c2x = c(readingMPR(3, 3), readingMPR(4, 3), readingMPR(5, 3), readingMPR(6, 3))
cotas.inf$c2y = c(readingMPR(3, 4), readingMPR(4, 4), readingMPR(5, 4), readingMPR(6, 4))
cotas.inf = melt(data = cotas.inf, id.vars = c(1,2), variable.name = "D")

ggplot(data = HC.1000, aes(x = H, y = C, color = value, group = value)) +
  geom_point(size = 1.5, alpha = .8) + facet_grid(. ~ value) +
  geom_line(data = cotas.sup, aes(x = c1x, y = c1y), color="gray") +
  geom_line(data = cotas.inf, aes(x = c2x, y = c2y), color="gray") + 
  xlim(limits = c(min(HC.1000$H), 1)) + 
  ylim(limits = c(0, max(HC.1000$C))) + 
  coord_fixed() +
  scale_color_discrete(name = expression(italic(D))) +
  ggtitle(expression(italic(N == 1000))) +
  xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
  theme_few(base_size = 12, base_family = "serif") +  
  theme(plot.title = element_text(hjust=0.5)) -> plot1000

# Analysis with N = 50k --------------------------------------------------------------------------------
HC = read.csv("../Data/HC/HC_50k.csv")[,2:4]
names(HC) = c("H", "C", "D")
HC$D = as.factor(HC$D)

HC.50k <- melt(data = HC, id.vars = c(1,2), variable.name = "D")

cotas.sup = data.frame("c1x" = numeric(37814), "c1y" = numeric(37814), "D" = as.factor(c(rep(3, 8332), rep(4, 9582), rep(5, 9915), rep(6, 9985))))
cotas.sup$c1x = c(readingMPR(3, 1), readingMPR(4, 1), readingMPR(5, 1), readingMPR(6, 1))
cotas.sup$c1y = c(readingMPR(3, 2), readingMPR(4, 2), readingMPR(5, 2), readingMPR(6, 2))
cotas.sup = melt(data = cotas.sup, id.vars = c(1,2), variable.name = "D")

cotas.inf = data.frame("c2x" = numeric(147122), "c2y" = numeric(147122), "D" = as.factor(c(rep(3, 14499), rep(4, 27753), rep(5, 43641), rep(6, 61229))))
cotas.inf$c2x = c(readingMPR(3, 3), readingMPR(4, 3), readingMPR(5, 3), readingMPR(6, 3))
cotas.inf$c2y = c(readingMPR(3, 4), readingMPR(4, 4), readingMPR(5, 4), readingMPR(6, 4))
cotas.inf = melt(data = cotas.inf, id.vars = c(1,2), variable.name = "D")

ggplot(data = HC.50k, aes(x = H, y = C, color = value, group = value)) +
   geom_point(size = 1.5, alpha = .8) + facet_grid(. ~ value) +
   geom_line(data = cotas.sup, aes(x = c1x, y = c1y), color="gray") +
   geom_line(data = cotas.inf, aes(x = c2x, y = c2y), color="gray") + 
   xlim(limits = c(min(HC.50k$H), 1)) + 
   ylim(limits = c(0, max(HC.50k$C))) + 
   coord_fixed() +
   scale_color_discrete(name = expression(italic(D))) +
   ggtitle(expression(italic(N == 50000))) +
   xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
   theme_few(base_size = 12, base_family = "serif") +  
   theme(plot.title = element_text(hjust=0.5)) -> plot50k

pdf("AllPlots.pdf", width = 18, height = 10)
ggarrange(plot1000, plot50k, nrow = 2, common.legend = TRUE, legend = "bottom") 
dev.off() 

png("AllPlots.png", width = 600, height = 900)
ggarrange(plot1000, plot50k, nrow = 2, common.legend = TRUE, legend = "bottom") 
dev.off() 
