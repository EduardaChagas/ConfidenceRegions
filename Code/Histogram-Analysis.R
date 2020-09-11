source("Bandt-Pompe.R")
if(!require(reshape2)){
  install.packages("reshape2")
  require(reshape2)
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
if(!require(fftw)){
  install.packages("fftw")
  require(fftw)
}
if(!require(grid)){
  install.packages("grid")
  require(grid)
}

rainbow_colors <- palette(c("#494947", #DarkGreen
                            "#7494EA", #MutedDarkBlue
                            "#B14AED", #Violet
                            "#44CCFF", #BrightLightBlue
                            "#35FF69", #BrightGreen
                            "#ED8438", #Orange
                            "#E7AD99", #Pink
                            "#C18C5D", #LightBrown
                            "#BF6F00", #DarkYellow
                            "#FB4D3D", #BrightRed
                            "#495867") #DarkGray
)

series.generator.fk <- function(pp, y, n, k){
  Series <- vector(mode="numeric")
  filtro <- (1:n)^-(k/2)
  filtro <- filtro / sum(filtro)
  y1 <- y * filtro    
  x1 <- IFFT(y1, plan=pp)  
  Series <- c(Re(x1)) 
  Series
}

series.generator.map <- function(r, x_0, n){
  Series <- vector(mode="numeric",length = n)
  Series[1] <- x_0
  for(i in c(2:n)){
    Series[i] <- r*Series[i-1]*(1 - Series[i-1])
  }
  Series
}

calculate.histogram <- function(ts, d, t, name.title){
  fat = factorial(d)
  symbol = define.symbols(d)
  p.patterns = formationPattern(ts, d, t, 0)
  n.symbols = dim(p.patterns)[1]
  index.rep = array(0, n.symbols)
  
  for(i in 1:n.symbols){
    for(j in 1:fat){
      if(all(p.patterns[i,] == symbol[j, ])){
        index.rep[i]=j
        break
      }
    }
  }
  index.rep = data.frame(i = index.rep)
  p = ggplot(index.rep) +
    geom_histogram(aes(x = i, y = ..density..),
                   binwidth = 1, color = "#212529") +
    ggtitle(name.title) +
    theme_few(base_size = 12, base_family = "serif") +
    theme(plot.title = element_text(size = 12, hjust=0.5)) + 
    scale_x_continuous(limits=c(0, fat+2)) + 
    labs(x= "", y = "")
  return(p)
}

############################################# Analysis ####################################################

## Variables ----------------------------------------------------------------------------------------------

d = 6
t = 1
n = 10^4

## f-k ----------------------------------------------------------------------------------------------------
set.seed(seed = 1234567890, kind = "Mersenne-Twister")

x = rnorm(n)
x = x - mean(x)
pp = planFFT(n)
y = FFT(x, plan=pp)
k = c(0, .5, 1, 1.5, 2, 2.5, 3)
series.fk = matrix(nrow = n, ncol = length(k))

i = 1
for(kk in k) {
  series.fk[,i] = series.generator.fk(pp, y, n, kk)
  i = i + 1
}

## Logistic Map --------------------------------------------------------------------------------------------
i = 1
x.0 = 0.1
r = c(3.6, 4)
series.map = matrix(nrow = n, ncol = length(r))
d2 = matrix(nrow = factorial(d), ncol = length(r))

for(rr in r){
  series.map[,i] = series.generator.map(rr, x.0, n)
  i = i + 1
}

x = seq(0, 2*pi, length.out = n)

series.monotonic = log(x + 0.1)
series.periodic = sin(2*x) * cos(2*x)

## Histogram ----------------------------------------------------------------------------------------------
plots = array(list(), 11)
series = data.frame(series.fk, series.map, series.monotonic, series.periodic)
names.title = c("White Noise", expression(f^{-1/2}), expression(f^{-1}),
                expression(f^{-3/2}), expression(f^{-2}), expression(f^{-5/2}),
                expression(f^{-3}), "Logistic Map, r = 3.6", "Logistic Map, r = 4",
                expression(paste("log(", x + .1, ")")), expression(paste("sin(", 2 * x, ")", "cos(", 2 * x, ")")))

for(i in 1:11){
  plots[[i]] = calculate.histogram(series[,i], d, t, names.title[i])
}

figure = ggarrange(plots[[1]], plots[[2]], plots[[3]], 
                  plots[[4]], plots[[5]], plots[[6]], 
                  plots[[7]], plots[[8]], plots[[9]], 
                  plots[[10]], plots[[11]], 
                  ncol = 3, nrow = 4) + theme_void() 

pdf("h.pdf", width = 9, height = 9) 
annotate_figure(figure,
                bottom = text_grob("Patterns", face = "italic", size = 12),
                left = text_grob("Probability", rot = 90, face = "italic", size = 12)
)
dev.off() 
