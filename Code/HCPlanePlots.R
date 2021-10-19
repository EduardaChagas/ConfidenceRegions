########################################################################################################
# Author: Eduarda Chagas
# Date : Nov 22, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

require(ggplot2)
require(ggthemes)
require(ggpubr)
require(reshape2)
require(ggrepel)
require(grid)
require(gridExtra)
require(fftw)
if(!require(gtools)){
  install.packages("gtools")
  require(gtools)
} 
source("Bandt-Pompe.R")


# Paleta montada a partir de https://coolors.co/ -------------------------------------------------------
rainbow_colors <- palette(c("#494947", #DarkGreen
                            "#FB4D3D", #BrightRed
                            "#B14AED", #Violet
                            "#44CCFF", #BrightLightBlue
                            "#35FF69", #BrightGreen
                            "#ED8438", #Orange
                            "#c41676", #Pink
                            "#06470d", #verde escuro
                            "#BF6F00", #DarkYellow
                            "#7494EA", #MutedDarkBlue
                            "#495867") #DarkGray
)

#Função geradora da série f^⁻k--------------------------------------------------------------------------
series_generator_fk <- function(pp, y, n, k){
  Series <- vector(mode="numeric")
  filtro <- (1:n)^-(k/2)
  filtro <- filtro / sum(filtro)
  y1 <- y * filtro    
  x1 <- IFFT(y1, plan=pp)  
  Series <- c(Re(x1)) 
  Series
}

#Função geradora do mapa logístico----------------------------------------------------------------------
series_generator_map <- function(r, x_0, n){
  Series <- vector(mode="numeric",length = n)
  Series[1] <- x_0
  for(i in c(2:n)){
    Series[i] <- r*Series[i-1]*(1 - Series[i-1])
  }
  Series
}

Entropy.complexity.values <- function(series, dimension, delay, type, rainbow_colors){
  if(is.null(dim(series)[2])){ #Apenas uma série temporal
    distributions = bandt.pompe(series, dimension, delay)
    Entropy = shannon.entropy.normalized(distributions)
    Complexity = Ccomplexity(distributions)
  }
  else{ #Analisando mais de uma série temporal
    size = dim(series)[2] 
    Complexity = Entropy = rep(0,size)
    distributions = matrix(nrow = factorial(dimension), ncol = size)
    for(i in 1:size){
      distributions[,i] = bandt.pompe(series[,i], dimension, delay)
      Entropy[i] = shannon.entropy.normalized(distributions[,i])
      Complexity[i] = Ccomplexity(distributions[,i])
    }
  }
  Shapes = c(8,15,16,17)
  Entropy.Complexity = data.frame(Entropy, Complexity, Shapes[type], rainbow_colors)
  Entropy.Complexity
}

#Plota as cotas do Plano HC-----------------------------------------------------------------------------
cotas <- function(dimension){
  c1x = readingMPR(dimension,1)
  c1y = readingMPR(dimension,2)
  c2x = readingMPR(dimension,3)
  c2y = readingMPR(dimension,4)
  
  p = qplot(xlab=expression(italic(H)), ylab=expression(italic(C))) +
    geom_line(aes(x=c2x, y=c2y), size=1.5, color="gray") +
    geom_line(aes(x=c1x, c1y), size=1.5, color="gray") + 
    theme_few(base_size = 18, base_family = "serif")  + 
    theme(plot.title = element_text(hjust=0.5)) + 
    scale_colour_few("Dark")
  print(p)
  return(p)
}


#Vai adicionando os pontos de Entropia e Complexidade no Plano HC
###IMPORTANTE: ESTA FUNÇÃO É ACUMULATIVA, SEMPRE IRÁ ADIOCIONAR PONTOS. PARA REFAZER ALGUMA ADIÇÃO É NECESSÁRIO CHAMAR NOVAMENTE A FUNÇÃO "cotas(dimension)"
HCPlane <- function(p, Entropy.Complexity, dimension, want_dotted){
  if(want_dotted == 1 && dim(Entropy.Complexity)[1] > 1){
    init = 1
    end = dim(Entropy.Complexity)[1]
    p = p + 
      geom_segment(aes(x=Entropy.Complexity$Entropy[init:(end - 1)],
                       xend=Entropy.Complexity$Entropy[(init + 1):end],
                       y=Entropy.Complexity$Complexity[init:(end - 1)],
                       yend=Entropy.Complexity$Complexity[(init + 1):end]), linetype="dotted")
  }
  p = p +
    geom_point(aes(x = Entropy.Complexity$Entropy, y = Entropy.Complexity$Complexity), 
               shape = Entropy.Complexity$Shape, color = Entropy.Complexity$rainbow_colors, 
               size = 4) 
  return(p)
}

#Função para a geração dos histogramas
histogram <- function(serie, dimension, delay, title, colorbins){
  fat = factorial(dimension)
  p_patterns = formationPattern(serie, dimension, delay,0)
  n_symbols = dim(p_patterns)[1]
  symbol = define.symbols(dimension)
  index_rep = array(0,n_symbols)
  for(i in 1:n_symbols){
    for(j in 1:fat){
      if(all(p_patterns[i,]==symbol[j, ])){
        index_rep[i]=j
        break
      }
    }
  }
  index_rep = index_rep[1:n_symbols]
  index_rep = data.frame(i = index_rep)
  p <- ggplot(index_rep) + 
    geom_histogram(aes(x = i, y = ..density..),
                   binwidth = 1, 
                   fill = colorbins, 
                   color = colorbins)+
    ggtitle(title) + xlab("") + 
    ylab("") + 
    #ylab(TeX("\\textit{Probability}")) +
    theme_few(base_size = 20, base_family = "sans") +
    theme(plot.title = element_text(hjust=0.5)) + 
    scale_x_continuous(limits = c(0, fat+2)) 
  return(p)
}

#Variáveis globais--------------------------------------------------------------------------------------
n = 10^4
dimension = 6
delay = 1
type = rep(0,9)

#f^-k com k: 0, 0.5, 1, 1.5, 2, 3
set.seed(seed = 1234567890, kind = "Mersenne-Twister")
x <- rnorm(n)
x <- x - mean(x)
pp <- planFFT(n)
y <- FFT(x, plan=pp)
k <- c(0, .5, 1, 1.5, 2, 2.5, 3)
series_fk <- matrix(nrow = n, ncol = length(k))
i <- 1
ii <- 1
for(kk in k) {
  series_fk[,i] <- series_generator_fk(pp, y, n, kk)
  type[ii] <- 1
  i <- i + 1
  ii <- ii + 1
}

#Mapa logístico: r= 4, 3.6 e x_0 = 0.1------------------------------------------------------------------
r <- c(3.6,4)
i <- 1
x_0 <- 0.1
series_map <- matrix(nrow = n, ncol = length(r))
d2 <- matrix(nrow = factorial(dimension), ncol = length(r))
for(rr in r){
  print(rr)
  series_map[,i] <- series_generator_map(rr,x_0,n)
  type[ii] <- 2
  i <- i + 1
  ii <- ii + 1
}

x <- seq(from = 0, to = 2*pi, length.out = n)

#Sequência monotônica crescente log(x + .1)-------------------------------------------------------------
series_monotonic <- log(x + 0.1)
type[ii] <- 3
ii <- ii + 1

#Sequência periódica sin(2x) * cos(2x)------------------------------------------------------------------
series_periodic <- sin(2*x) * cos(2*x)
type[ii] <- 4
series <- data.frame(series_fk,series_map,series_monotonic,series_periodic)

#Plotando os histogramas das séries--------------------------------------------------------------------- 
h0 <- histogram(series_fk[,1], 6, delay,
                "White Noise", rainbow_colors[1])
h05 <- histogram(series_fk[,2], 6, delay,
                 expression(italic(f)^{-1/2}), rainbow_colors[2])
h1 <- histogram(series_fk[,3], 6, delay,
                expression(italic(f)^{-1}), rainbow_colors[3])
h15 <- histogram(series_fk[,4], 6, delay,
                 expression(italic(f)^{-3/2}), rainbow_colors[4])
h2 <- histogram(series_fk[,5], 6, delay,
                expression(italic(f)^{-2}), rainbow_colors[5])
h25 <- histogram(series_fk[,6], 6, delay,
                 expression(italic(f)^{-5/2}), rainbow_colors[6])
h3 <- histogram(series_fk[,7], 6, delay,
                expression(italic(f)^{-3}), rainbow_colors[7])
#
hlogistic36 <- histogram(series_map[,1], 6, delay,
                         expression("Logistic Map, "~italic(r)==3.6), rainbow_colors[8])
hlogistic4 <- histogram(series_map[,2], 6, delay, expression("Logistic Map, "~italic(r)==4), rainbow_colors[9])

#pdf("../Reports/JOURNAL - Confidence Regions/Figures/h.pdf", width = 12, height = 12)

ggarrange(h0, h05, h1, h15, h2, h25, h3, hlogistic36, hlogistic4, 
          ncol = 3, nrow = 3)
#dev.off()

#Gerando os gráficos
subx <- 4500:5500
subx1 <- 4700:4900
subx2 <- 1:100*100

rainbow_colors <- palette(rainbow(11))

#Calculando os valores de entropia e complexidade
Entropy.Complexity.fk <- Entropy.complexity.values(series_fk, dimension, delay, type[1:7], rainbow_colors[1:7])
Entropy.Complexity.map <- Entropy.complexity.values(series_map, dimension, delay, type[8:9], rainbow_colors[8:9])
Entropy.Complexity.monotonic <- Entropy.complexity.values(series_monotonic, dimension, delay, type[10], rainbow_colors[10])
Entropy.Complexity.periodic <- Entropy.complexity.values(series_periodic, dimension, delay, type[11], rainbow_colors[11])

#Plotando o gráfico HCPlane
p <- cotas(dimension)
pHC <- HCPlane(p, Entropy.Complexity.fk, dimension,1)
pHC <- HCPlane(pHC, Entropy.Complexity.map, dimension,0)
pHC <- HCPlane(pHC, Entropy.Complexity.monotonic, dimension,0)
pHC <- HCPlane(pHC, Entropy.Complexity.periodic, dimension,0) + 
       xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
       theme_minimal(base_size = 25, base_family = "serif")

p0 <- qplot(x=subx, y=series_fk[subx,1], geom="line", color=I(rainbow_colors[1]), xlab="", ylab="") +
  ggtitle("White Noise") + 
  theme(plot.title = element_text(hjust=0.5)) + theme_tufte(base_size = 15, base_family = "serif")

p05 <- qplot(x=subx, y=series_fk[subx,2], geom="line", xlab="", ylab="", color=I(rainbow_colors[2])) +
  ggtitle(expression(italic(f)^{-1/2})) + 
  theme(plot.title = element_text(hjust=0.5)) + theme_tufte(base_size = 15, base_family = "serif")

p1 <- qplot(x=subx, y=series_fk[subx,3], geom="line", xlab="", ylab="", color=I(rainbow_colors[3])) +
  ggtitle(expression(italic(f)^{-1})) + 
  theme(plot.title = element_text(hjust=0.5)) + theme_tufte(base_size = 15, base_family = "serif")

p15 <- qplot(x=subx, y=series_fk[subx,4], geom="line", xlab="", ylab="", color=I(rainbow_colors[4])) +
  ggtitle(expression(italic(f)^{-3/2})) + theme(plot.title = element_text(hjust=0.5)) + 
  theme_tufte(base_size = 15, base_family = "serif")

p2 <- qplot(x=subx, y=series_fk[subx,5], geom="line", xlab="", ylab="", color=I(rainbow_colors[5])) +
  ggtitle(expression(italic(f)^{-2})) + theme(plot.title = element_text(hjust=0.5)) + 
  theme_tufte(base_size = 15, base_family = "serif")

p25 <- qplot(x=subx, y=series_fk[subx,6], geom="line", xlab="", ylab="", color=I(rainbow_colors[6])) +
  ggtitle(expression(italic(f)^{-5/2})) + theme(plot.title = element_text(hjust=0.5)) + 
  theme_tufte(base_size = 15, base_family = "serif")

p3 <- qplot(x=subx, y=series_fk[subx,7], geom="line", xlab="", ylab="", color=I(rainbow_colors[7])) +
  ggtitle(expression(italic(f)^{-3})) + theme(plot.title = element_text(hjust=0.5)) + 
  theme_tufte(base_size = 15, base_family = "serif")

plogistic36 <- qplot(x=subx1, y=series_map[subx1,1], geom="line", xlab="", ylab="", color=I(rainbow_colors[8])) +
  ggtitle(expression("Logistic Map, "~italic(r)==3.6)) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_tufte(base_size = 15, base_family = "serif")

plogistic4 <- qplot(x=subx1, y=series_map[subx1,2], geom="line" ,xlab="", ylab="", color=I(rainbow_colors[9])) +
  ggtitle(expression("Logistic Map, "~italic(r)==4)) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_tufte(base_size = 15, base_family = "serif")

plog <- qplot(x=subx2, y=series_monotonic[subx2], geom="line", xlab="", ylab="", color=I(rainbow_colors[10])) +
  ggtitle(expression(paste("log(", italic(x)+.1, ")"))) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_tufte(base_size = 15, base_family = "serif")

psincos <- qplot(x=subx2, y=series_periodic[subx2], geom="line", xlab="", ylab="", color=I(rainbow_colors[11])) +
  ggtitle(expression(paste("sin(", 2 * italic(x), ")", "cos(", 2 * italic(x), ")"))) + 
  theme(plot.title = element_text(hjust=0.5)) + 
  theme_tufte(base_size = 15, base_family = "serif")


#Plotando todos os gráficos gerados em um grid

#pdf("../Reports/JOURNAL - Confidence Regions/Figures/AllSystems.pdf", width = 15, height = 10)
grid.newpage()
pushViewport(viewport(layout = grid.layout(5, 4)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

print(pHC, vp = vplayout(2:4, 1:3))
print(plogistic36, vp = vplayout(1, 1)) 
print(plogistic4, vp = vplayout(1, 2)) 
print(p3, vp = vplayout(1, 3))
print(p25, vp = vplayout(1, 4))
print(p2, vp = vplayout(2, 4)) 
print(p15, vp = vplayout(3, 4)) 
print(p1, vp = vplayout(4, 4)) 
print(plog, vp = vplayout(5, 1)) 
print(psincos, vp = vplayout(5, 2)) 
print(p0, vp = vplayout(5, 3))
print(p05, vp = vplayout(5, 4))
dev.off()


#Plotando o zoom (RightMostCorner)
legend.names = c("White Noise", "f^-1/2", "f^-1")
Entropy.Complexity = data.frame(Entropy = Entropy.Complexity.fk$Entropy[1:3],
                                Complexity = Entropy.Complexity.fk$Complexity[1:3],
                                Shape = Entropy.Complexity.fk$Shapes.type.[1:3],
                                rainbow_colors = Entropy.Complexity.fk$rainbow_colors[1:3],
                                legend = legend.names)

p = cotas(dimension)
p = p +
  geom_point(data = Entropy.Complexity, aes(x = Entropy, y = Complexity), 
             shape = Entropy.Complexity$Shape, color = Entropy.Complexity$rainbow_colors, size = 6) +
  xlim(limits = c(0.985, 1)) + ylim(limits = c(0, 0.05)) +
  annotate("text", x = Entropy.Complexity$Entropy[1], y = Entropy.Complexity$Complexity[1], 
           label = "White Noise" ,size = 7, vjust = -1)  +
  annotate("text", x = Entropy.Complexity$Entropy[2], y = Entropy.Complexity$Complexity[2], 
           label = as.character(expression(italic(f)^{-1/2})), parse =TRUE ,size = 7, vjust = -1)  +
  annotate("text", x = Entropy.Complexity$Entropy[3], y = Entropy.Complexity$Complexity[3], 
           label = as.character(expression(italic(f)^{-1})), parse =TRUE ,size = 7, vjust = -1)  + 
  xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
  theme_minimal(base_size = 25, base_family = "serif")  
#pdf("../Reports/JOURNAL - Confidence Regions/Figures/RightmostCorner.pdf", width = 15, height = 10)
print(p)
dev.off()
