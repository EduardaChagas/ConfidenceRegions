################################################################################
# Author: Eduarda Chagas
# Date : Out 18, 2021
# Contact: eduarda.chagas@dcc.ufmg.br
################################################################################

# Packages and sources ---------------------------------------------------------
source("Bandt-Pompe.R")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggthemes)) install.packages("ggthemes")

# Cotas do Plano HC-------------------------------------------------------------
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
  return(p)
}

# Função que exibe o ponto no plano HC, suas cotas e região de confiança especificada
plot_points_confidence_regions <- function(point, D=6, N=1000, interval=99){
  
  hc.points = read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
  
  if(interval == 99){
    rect = data.frame(H = hc.points$H[9:12], C = hc.points$C[9:12])
  }else if(interval == 95){
    rect = data.frame(H = hc.points$H[5:8], C = hc.points$C[5:8])
  }
  
  p = cotas(D)
  p = p +
    geom_point(data = point, aes(x = H, y = C), size = 4) +
    geom_polygon(data = rect, aes(x = H, y = C), fill = "green", alpha=0.2, inherit.aes = FALSE) +
    xlim(limits = c(min(min(rect$H), min(point$H)), max(max(rect$H), max(point$H)))) +
    ylim(limits = c(min(min(rect$C), min(point$C)), max(max(rect$C), max(point$C)))) +
    xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
    theme_minimal(base_size = 25, base_family = "serif")  
  
  return(p)
}

# Função que retorna um conjunto de sequências de ruído branco de um dado N
get_white_noise <- function(N){
  noise_data = readBin('../Data/random.org/random_50k.bin', n=1e8, size="4", what ='integer')
  normalize_seq = abs(noise_data/max(noise_data))
  split_seq = as.matrix(split(normalize_seq, ceiling(seq_along(normalize_seq)/N)))
  
  n_ts = length(split_seq) - 1
  ts = matrix(ncol = N, nrow = n_ts)
  for(i in 1:n_ts)
    ts[i,] = split_seq[[i]]
  
  return(ts)
}

# Parâmetros globais -----------------------------------------------------------
D = 6
tau = 1
N = 1000
interval = 95

# Obtenção da sequência e seu respectivo ponto no plano HC ---------------------
ts = get_white_noise(N)
probs.ts = bandt.pompe(ts[1,], D, tau)
h = shannon.entropy.normalized(probs.ts)
c = Ccomplexity(probs.ts)
point = data.frame(H = h, C = c)

# Obtenção do plot -------------------------------------------------------------
p = plot_points_confidence_regions(point, D, N)
print(p)
