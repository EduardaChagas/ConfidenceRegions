################################################################################
# Author: Eduarda Chagas
# Date : Out 18, 2021
# Contact: eduarda.chagas@dcc.ufmg.br
################################################################################

# Packages and sources ---------------------------------------------------------
source("Bandt-Pompe.R")
source("p-value-test.R")
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
plot_points_confidence_regions <- function(p, point, D=6, N=1000, interval=99, zoom_x, zoom_y){
  
  hc.points = read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
  
  if(interval == 99){
    rect = data.frame(H = hc.points$H[9:12], C = hc.points$C[9:12])
  }else if(interval == 95){
    rect = data.frame(H = hc.points$H[5:8], C = hc.points$C[5:8])
  }
  p = p +
    geom_point(data = point, aes(x = H, y = C), size = 4) +
    geom_polygon(data = rect, aes(x = H, y = C), fill = "green", alpha=0.2, inherit.aes = FALSE) +
    xlim(limits = c(min(min(rect$H), min(point$H)) - zoom_x, max(max(rect$H), max(point$H)) +  zoom_x)) +
    ylim(limits = c(min(min(rect$C), min(point$C)) - zoom_y, max(max(rect$C), max(point$C)) + zoom_y)) +
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

# Obtenção da série e seu respectivo ponto no plano HC ---------------------
ts = get_white_noise(N)
x <- ts[2,]
probs.ts = bandt.pompe(x, D, tau)
(h = shannon.entropy.normalized(probs.ts))
(c = Ccomplexity(probs.ts))
point = data.frame(H = h, C = c)

# Obtenção do plot -------------------------------------------------------------

p = cotas(D)
p = plot_points_confidence_regions(p, point, D, N, zoom_x = 0.1, zoom_y = 0.1)
print(p)

# Here begins the disaster by Alejandro

## New point 1: reverse the time series
# rev.ts <- rev(x)
# probs.rev.ts <- bandt.pompe(rev.ts, D, tau)
# (hrev = shannon.entropy.normalized(probs.rev.ts))
# (crev = Ccomplexity(probs.rev.ts))
# ### The point did not change (that's good)
# calculate.p.value.samples(data.frame(H = hrev, C = crev), N, D)

## New point 2: add logistic map

logistic.map <- function(r, x, N, M){
  ## r: bifurcation parameter
  ## x: initial value
  ## N: number of iteration
  ## M: number of iteration points to be returned
  z <- 1:N
  z[1] <- x
  for(i in c(1:(N-1))){
    z[i+1] <- r *z[i]  * (1 - z[i])
  }
  ## Return the last M iterations 
  z[c((N-M):N)]
}

x.logistic.map <- logistic.map(3.999999999999999, .5, 5000, 999)

p.logistic.map <- bandt.pompe(x.logistic.map, D, tau)
(h.logistic.map = shannon.entropy.normalized(p.logistic.map))
(c.logistic.map = Ccomplexity(p.logistic.map))
### Yes! We're far fom (h,c)
calculate.p.value.samples(data.frame(H = h.logistic.map, C = c.logistic.map), N, D)


## Now begins the fun; let's mix x (ts[2,]) and x.logistic.map

alpha <- 1- 11*.Machine$double.eps
x.mix <- (1-alpha) * x + alpha * x.logistic.map

probs.mix = bandt.pompe(x.mix, D, tau)
(hmix = shannon.entropy.normalized(probs.mix))
(cmix = Ccomplexity(probs.mix))

p + 
  geom_point(data=data.frame(hmix, cmix), 
             aes(x=hmix, y=cmix), 
             col="red", size=3) +
  xlim(.91, .945) +
  ylim(0.05, .25)
### Close to getting something interesting here
calculate.p.value.samples(data.frame(H = hmix, C = cmix), N, D)

## Now begins the fun 2; let's add a sine

xsin <- sin((1:1000)/1000 * 4*pi)

alpha <- .2
x.mix <- (1-alpha)*x+alpha*xsin

probs.mix = bandt.pompe(x.mix, D, tau)
(hmix = shannon.entropy.normalized(probs.mix))
(cmix = Ccomplexity(probs.mix))

p + 
  geom_point(data=data.frame(hmix, cmix), 
             aes(x=hmix, y=cmix), 
             col="red", size=2)
### Nothing interesting adding the sine: the point walks through a line
calculate.p.value.samples(data.frame(H = hmix, C = cmix), N, D)

## Now a patch from the chaotic map

x.patched <- c(x[1:990], x.logistic.map[991:1000])

probs.patched = bandt.pompe(x.patched, D, tau)
(h.patched = shannon.entropy.normalized(probs.patched))
(c.patched = Ccomplexity(probs.patched))

p + 
  geom_point(data=data.frame(h.patched, c.patched), 
             aes(x=h.patched, y=c.patched), 
             col="red", size=2) + 
  xlim(.91, .945) +
  ylim(.05, .25)
#### Calcular o p-valor de x.patched
calculate.p.value.samples(data.frame(H = h.patched, C = c.patched), N, D)

### Now I will encapsulate things

ExperimentPatched <- function(x, r, a, N, M, patch, plot.region) {
  
  x.logistic.map <- logistic.map(r, a, N, M)
  n.tail <- round(patch*length(x))
  x.patched <- c(x[1:(length(x)-n.tail)], 
                 x.logistic.map[(length(x)-n.tail+1):length(x)])
  
  probs.patched = bandt.pompe(x.patched, 6, 1)
  
  print((h.patched = shannon.entropy.normalized(probs.patched)))
  print((c.patched = Ccomplexity(probs.patched)))
  
  print(plot.region + 
    geom_point(data=data.frame(h.patched, c.patched), 
               aes(x=h.patched, y=c.patched), 
               col="red", size=2) + 
    xlim(.91, .945) +
    ylim(.05, .25))
  #### Calcular o p-valor de x.patched
  (calculate.p.value.samples(data.frame(H = h.patched, C = c.patched), length(x), 6))
}

# patch in .01 to .06 !!!
ExperimentPatched(x, 4, .5, 5000, 999, .06, p)

# Six points for illustration
x.logistic.map <- logistic.map(3.99999, .5, 5000, 999)
percentage.patch <- seq(.01, .06, by=.01)
Experiment.Logistic.Map <- data.frame(h.patched=rep(0, 6),
                                      c.patched=rep(0, 6),
                                      pvalue.patched=rep(0, 6))
for(count in 1:6){
  patch <- percentage.patch[count]
  
  n.tail <- round(patch*1000)
  x.patched <- c(x[1:(1000-n.tail)], 
                 x.logistic.map[(1000-n.tail+1):1000])
  probs.patched <- bandt.pompe(x.patched, 6, 1)
  Experiment.Logistic.Map$h.patched[count] <- shannon.entropy.normalized(probs.patched)
  Experiment.Logistic.Map$c.patched[count] <- Ccomplexity(probs.patched)
  Experiment.Logistic.Map$pvalue.patched[count] <- calculate.p.value.samples(data.frame(H = h.patched, C = c.patched), 
                            1000, 6)
}

p +
  geom_point(data=Experiment.Logistic.Map, aes(x=h.patched, y=c.patched), col="red", size=2)

# Trailing an increasing function
x.increasing <- (1:1000)/1000
percentage.patch <- seq(.01, .06, by=.01)
Experiment.Increasing <- data.frame(h.patched=rep(0, 6),
                                      c.patched=rep(0, 6),
                                      pvalue.patched=rep(0, 6))
for(count in 1:6){
  patch <- percentage.patch[count]
  
  n.tail <- round(patch*1000)
  x.patched <- c(x[1:(1000-n.tail)], 
                 x.increasing[(1000-n.tail+1):1000])
  probs.patched <- bandt.pompe(x.patched, 6, 1)
  Experiment.Increasing$h.patched[count] <- shannon.entropy.normalized(probs.patched)
  Experiment.Increasing$c.patched[count] <- Ccomplexity(probs.patched)
  Experiment.Increasing$pvalue.patched[count] <- calculate.p.value.samples(
    data.frame(H = Experiment.Increasing$h.patched[count], 
               C = Experiment.Increasing$c.patched[count]), 
    1000, 6)
}

Experiment.Increasing$percentage <- percentage.patch*100

require(ggrepel)

p +
  geom_point(data=Experiment.Increasing, aes(x=h.patched, 
                                             y=c.patched), 
             col="red", size=2) +
  geom_text_repel(data=Experiment.Increasing, aes(x=h.patched, 
                                                  y=c.patched,
                                                  label=paste0(as.character(percentage.patch*100), rep("%", 6))),
                  direction = "y") +
  xlim(.905, .945) +
  ylim(0.05, 0.3)

# Vamos reportar apenas o resultado de acrescentar ao final uma função crescente

