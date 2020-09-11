source("Bandt-Pompe.R")
if(!require(reshape2)){
  install.packages("reshape2")
  require(reshape2)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(ggthemes)){
  install.packages("ggthemes")
  require(ggthemes)
}
if(!require(ggpubr)){
  install.packages("ggpubr")
  require(ggpubr)
}
HC.values = read.csv('HC-Logistic-Map-Rosso.csv')
r = c(3.73, 3.70, 3.68, 3.67, 3.83, 4.00)
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
color.series = rep(rainbow_colors[1:6], 50)


hc.4 = HC.values[which(HC.values$D == 4),]
hc.4$color = color.series
hc.4$A = as.factor(hc.4$A)

cotas.sup = data.frame("c1x" = numeric(9582), "c1y" = numeric(9582), "D" = as.factor(rep(4, 9582)))
cotas.sup$c1x = readingMPR(4, 1)
cotas.sup$c1y = readingMPR(4, 2)

cotas.inf = data.frame("c1x" = numeric(27753), "c1y" = numeric(27753), "D" = as.factor(rep(4, 27753)))
cotas.inf$c1x = readingMPR(4, 3)
cotas.inf$c1y = readingMPR(4, 4)

ggplot(data = hc.4, aes(x = H, y = C, color = A)) +
  geom_point(size = 2.5, alpha = .8) +
  geom_line(data = cotas.sup, aes(x = c1x, y = c1y), color="gray") +
  geom_line(data = cotas.inf, aes(x = c1x, y = c1y), color="gray") + 
  ggtitle(expression(italic(D == 4))) +
  xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
  theme_few(base_size = 12, base_family = "serif") +  
  theme(plot.title = element_text(hjust=0.5)) -> plot4

hc.5 = HC.values[which(HC.values$D == 5),]
hc.5$color = color.series
hc.5$A = as.factor(hc.5$A)
cotas.sup = data.frame("c1x" = numeric(9915), "c1y" = numeric(9915), "D" = as.factor(rep(4, 9915)))
cotas.sup$c1x = readingMPR(5, 1)
cotas.sup$c1y = readingMPR(5, 2)
cotas.inf = data.frame("c1x" = numeric(43641), "c1y" = numeric(43641), "D" = as.factor(rep(4, 43641)))
cotas.inf$c1x = readingMPR(5, 3)
cotas.inf$c1y = readingMPR(5, 4)

ggplot(data = hc.5, aes(x = H, y = C, color = A)) +
  geom_point(size = 2.5, alpha = .8) + 
  scale_color_discrete(name = expression(italic(A))) +
  geom_line(data = cotas.sup, aes(x = c1x, y = c1y), color="gray") +
  geom_line(data = cotas.inf, aes(x = c1x, y = c1y), color="gray") + 
  ggtitle(expression(italic(D == 5))) +
  xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
  theme_few(base_size = 12, base_family = "serif") +  
  theme(plot.title = element_text(hjust=0.5)) -> plot5

hc.6 = HC.values[which(HC.values$D == 6),]
hc.6$color = color.series
hc.6$A = as.factor(hc.6$A)
cotas.sup = data.frame("c1x" = numeric(9985), "c1y" = numeric(9985), "D" = as.factor(rep(4, 9985)))
cotas.sup$c1x = readingMPR(6, 1)
cotas.sup$c1y = readingMPR(6, 2)
cotas.inf = data.frame("c1x" = numeric(61229), "c1y" = numeric(61229), "D" = as.factor(rep(4, 61229)))
cotas.inf$c1x = readingMPR(6, 3)
cotas.inf$c1y = readingMPR(6, 4)

ggplot(data = hc.6, aes(x = H, y = C, color = A)) +
  geom_point(size = 2.5, alpha = .8) +
  geom_line(data = cotas.sup, aes(x = c1x, y = c1y), color="gray") +
  geom_line(data = cotas.inf, aes(x = c1x, y = c1y), color="gray") + 
  ggtitle(expression(italic(D == 6))) +
  xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
  theme_few(base_size = 12, base_family = "serif") +  
  theme(plot.title = element_text(hjust=0.5)) -> plot6


pdf("LogisticMap.pdf", width = 12, height = 5)  
p = ggarrange(plot4, plot5, plot6,
              ncol=3, nrow=1, common.legend = TRUE, legend = "right") 
print(p)
dev.off() 
