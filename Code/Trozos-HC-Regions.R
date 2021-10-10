########################################################################################################
# Author: Eduarda Chagas
# Date : Jul 5, 2020
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

D = 3
N = 1000

hc.points = read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
rect90 = data.frame(H = hc.points$H[1:4], C = hc.points$C[1:4])
rect95 = data.frame(H = hc.points$H[5:8], C = hc.points$C[5:8])
rect99 = data.frame(H = hc.points$H[9:12], C = hc.points$C[9:12])
rect999 = data.frame(H = hc.points$H[13:16], C = hc.points$C[13:16])

cotas.sup = cotas(D, 0)
cotas.sup = data.frame("H" = cotas.sup$cx, 'C' = cotas.sup$cy)
cotas.inf = cotas(D, 1)
cotas.inf = data.frame("H" = cotas.inf$cx, 'C' = cotas.inf$cy)

index.i = which(cotas.sup$H >= min(hc.points$H) & cotas.sup$H <= 1)
sup = cotas.sup[index.i,]
sup = data.frame(H = sup$H, C = predict(lm(data = sup, formula = C ~ poly(H, 3))))
cotas.sup = data.frame(H = c(cotas.sup[-index.i,]$H, sup$H), 
                       C = c(cotas.sup[-index.i,]$C, sup$C))

index.j = which(cotas.inf$H >= min(hc.points$H) & cotas.inf$H <= 1)
inf = cotas.inf[index.i,]
inf = data.frame(H = inf$H, C = predict(lm(data = inf, formula = C ~ poly(H, 3))))
cotas.inf = data.frame(H = c(cotas.inf[-index.i,]$H, inf$H), 
                       C = c(cotas.inf[-index.i,]$C, inf$C))

ggplot(hc.points, aes(x = H, y = C)) +
  geom_polygon(data = rect90, aes(x = H, y = C), fill="white", alpha=0.9, inherit.aes = FALSE) +
  geom_polygon(data = rect95, aes(x = H, y = C), fill="red", alpha=0.5, inherit.aes = FALSE) +
  geom_polygon(data = rect99, aes(x = H, y = C), fill="green", alpha=0.2, inherit.aes = FALSE) +
  geom_polygon(data = rect999, aes(x = H, y = C), fill="yellow", alpha=0.2, inherit.aes = FALSE) +
  geom_point(aes(x = hc.points$H[17], y = hc.points$C[17]), colour="red") +
  geom_line(data = cotas.sup, aes(x = H, y = C), color="gray") +
  geom_line(data = cotas.inf, aes(x = H, y = C), color="gray") + 
  xlim(limits = c(min(hc.points$H), 1)) + 
  ylim(limits = c(0, max(hc.points$C))) + 
  xlab("H") +
  ylab("C") +
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_few(base_size = 12, base_family = "serif") 
