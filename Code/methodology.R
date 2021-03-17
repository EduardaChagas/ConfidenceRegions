source("Bandt-Pompe.R")
if(!require(latex2exp)){
  install.packages("latex2exp")
  require(latex2exp)
}
if(!require(ggrepel)){
  install.packages("ggrepel")
  require(ggrepel)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(ggthemes)){
  install.packages("ggthemes")
  require(ggthemes)
}
#library(directlabels)
#if(!require(plot3D)){
#  install.packages("plot3D")
#  require(plot3D)
#} 


D = 6
N = 50000
HC = read.csv("../Data/Regions-HC/HC_50k.csv")[,2:4]
index = which(HC$D == D)
HC = HC[index[1:15], ]

cotas.sup = cotas(D, 0)
cotas.sup = data.frame("H" = cotas.sup$cx, 'C' = cotas.sup$cy)
cotas.inf = cotas(D, 1)
cotas.inf = data.frame("H" = cotas.inf$cx, 'C' = cotas.inf$cy)

index.i = which(cotas.sup$H >= 0.9985 & cotas.sup$H <= 1)
sup = cotas.sup[index.i,]
sup = data.frame(H = sup$H, C = predict(lm(data = sup, formula = C ~ poly(H, 3))))
cotas.sup = data.frame(H = c(cotas.sup[-index.i,]$H, sup$H), 
                       C = c(cotas.sup[-index.i,]$C, sup$C))

index.j = which(cotas.inf$H >= 0.9985 & cotas.inf$H <= 1)
inf = cotas.inf[index.i,]
inf = data.frame(H = inf$H, C = predict(lm(data = inf, formula = C ~ poly(H, 3))))
cotas.inf = data.frame(H = c(cotas.inf[-index.i,]$H, inf$H), 
                       C = c(cotas.inf[-index.i,]$C, inf$C))

step1 <- ggplot(HC, aes(x = H, y = C)) + geom_point(size=4) +
  geom_line(data = cotas.sup, aes(x = H, y = C), color="gray") +
  geom_line(data = cotas.inf, aes(x = H, y = C), color="gray") + 
  xlim(limits = c(0.9985, 1)) + 
  ylim(limits = c(0, 0.004)) + 
  geom_rect(aes(xmin = 0.99875, xmax = 0.9990, ymin = 0.0024, ymax = 0.003), 
            fill = "blue", alpha = 0.01, inherit.aes = FALSE) + 
  xlab(expression(italic(H))) +
  ylab(expression(italic(C))) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_clean(base_size = 50, base_family = "sans") 

ggplot(HC, aes(x = H, y = C)) + geom_point(size = 4) +
  geom_line(data = cotas.sup, aes(x = H, y = C), color="gray") +
  geom_line(data = cotas.inf, aes(x = H, y = C), color="gray") + 
  xlim(limits = c(0.99875, 0.9990)) +  
  ylim(limits = c(0.0024, 0.003)) + 
  geom_rect(aes(xmin = 0.99875, xmax = 0.9990, ymin = 0.0024, ymax = 0.003), 
            fill = "blue", alpha = 0.01, inherit.aes = FALSE) + 
  xlab(expression(italic(H))) +
  ylab(expression(italic(C))) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_clean(base_size = 25, base_family = "sans") 

pca_HC = prcomp(x = HC[,-3], 
                retx = TRUE, 
                center=TRUE, scale. = TRUE)

PC = data.frame(PC1 = pca_HC$x[,1], PC2 = pca_HC$x[,2])

attach(PC)
(qpc1 = quantile(PC1, probs = c(0.125, 0.5, 1-0.125)))
(x.pc.75.1 = PC1[which(min(abs(PC1- qpc1[1])) == abs(PC1- qpc1[1]))])
(x.pc.75.2 = PC1[which(min(abs(PC1- qpc1[3])) == abs(PC1- qpc1[3]))])
(y.pc.75.1 = c(max(PC2[PC1 > x.pc.75.1 & PC1 < x.pc.75.2])))
(y.pc.75.2 = c(min(PC2[PC1 > x.pc.75.1 & PC1 < x.pc.75.2])))
detach(PC)

rect90 = data.frame(xmin = x.pc.75.1, xmax = x.pc.75.2,
                    ymin = y.pc.75.1, ymax = y.pc.75.2)
median.pca = data.frame(PC1 = median(PC$PC1), PC2 = median(PC$PC2), name = "Median")

step2 <- 
  ggplot(PC, aes(x = PC1, y = PC2)) + geom_point(size=4) +
  geom_point(data = median.pca, aes(x = PC1, y = PC2), colour = "red", size=4) +
  geom_hline(yintercept = y.pc.75.1, linetype="dashed", color = "black") + 
  geom_hline(yintercept = y.pc.75.2, linetype="dashed", color = "black") + 
  geom_vline(xintercept = x.pc.75.1, linetype="dashed", color = "black") + 
  geom_vline(xintercept = x.pc.75.2, linetype="dashed", color = "black") + 
  geom_label_repel(data = median.pca, aes(x = PC1, y = PC2, label = name),
    fontface = 'bold', color = 'red', size=10,
    box.padding = unit(0.35, "lines"),
    point.padding = unit(0.5, "lines"),
    segment.color = 'grey50'
  ) +
  geom_rect(data = rect90, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  xlab(expression(PC[1])) +
  ylab(expression(PC[2])) + 
  theme_clean(base_size = 50, base_family = "sans") +  
  theme(plot.title = element_text(hjust=0.5)) 


H = t(t(pca_HC$x %*% t(pca_HC$rotation)) * pca_HC$scale + pca_HC$center)
HC.BP = data.frame("H" = H[,1], 
                   "C" = H[,2],
                   stringsAsFactors=FALSE)

my.order = c(1,2,4,3)

rect90 = matrix(unlist(rect90), nrow = 2, ncol = 2)
M = mesh(rect90[,1], rect90[,2])
rect90 = matrix(nrow = 4, ncol = 2)
rect90[,1] = M$x
rect90[,2] = M$y
rect90 = t(t(rect90 %*% t(pca_HC$rotation)) * pca_HC$scale + pca_HC$center)
rect90 = data.frame("H" = rect90[my.order, 1], 
                    "C" = rect90[my.order, 2],
                    stringsAsFactors=FALSE)
rect90$H[rect90$H < 0] = 0
rect90$C[rect90$C < 0] = 0
rect90$H[rect90$H > 1] = 1
rect90$C[rect90$C > 1] = 1

ggplot(HC, aes(x = H, y = C)) +
  geom_line(data = cotas.sup, aes(x = H, y = C), color="gray") +
  geom_line(data = cotas.inf, aes(x = H, y = C), color="gray") +
  geom_rect(aes(xmin = 0.99875, xmax = 0.9990, ymin = 0.0024, ymax = 0.003), 
            fill = "blue", alpha = 0.01, inherit.aes = FALSE) + 
  geom_point(size = 1) +
  geom_polygon(data = rect90, aes(x = H, y = C), fill = "red", alpha = 1, inherit.aes = FALSE) +
  xlim(limits = c(0.9985, 1)) + 
  ylim(limits = c(0, 0.004)) + 
  xlab(expression(italic(H))) +
  ylab(expression(italic(C))) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_clean(base_size = 25, base_family = "sans") 

step3 <- ggplot(HC, aes(x = H, y = C)) +
  geom_line(data = cotas.sup, aes(x = H, y = C), color="gray") +
  geom_line(data = cotas.inf, aes(x = H, y = C), color="gray") +
  geom_polygon(data = rect90, aes(x = H, y = C), fill = "red", alpha = 0.4, inherit.aes = FALSE) +
  geom_rect(aes(xmin = 0.99875, xmax = 0.9990, ymin = 0.0024, ymax = 0.003), 
            fill = "blue", alpha = 0.01, inherit.aes = FALSE) + 
  geom_point(size = 4) +
  xlim(limits = c(0.99875, 0.9990)) +  
  ylim(limits = c(0.0024, 0.003)) + 
  xlab(expression(italic(H))) +
  ylab(expression(italic(C))) +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_clean(base_size = 50, base_family = "sans") 

ggsave(step1, filename = "../Reports/JOURNAL - Confidence Regions/Figures/step1.pdf", width = 14, height = 10)
ggsave(step2, filename = "../Reports/JOURNAL - Confidence Regions/Figures/step2.pdf", width = 14, height = 10)
ggsave(step3, filename = "../Reports/JOURNAL - Confidence Regions/Figures/step3.pdf", width = 14, height = 10)
