library(ggplot2)
setwd('Marcelo/paper/')
elmacho=read.csv('../Data/results.csv', header=T)

elmacho$X=NULL
#dEuclid=sqrt(elmacho$H^2+elmacho$C^2)

#elmacho=cbind(elmacho,dEuclid)

#cotas

read_cotas = function(D=3){
  cont_name = paste('limits/continua-N',factorial(D),'.q1', sep='')
  trozos_name = paste('limits/trozos-N',factorial(D),'.q1', sep='')
  continua = read.table(cont_name, skip=7)
  trozos = read.table(trozos_name, skip=7)
  cotas = list(continua,trozos)
  return(cotas)
}

plot_confidence = function(DD,TNN){
  #D=3, TN=1000, K=0
  out1 = subset(elmacho,D==DD&TN==TNN)
  cot = read_cotas(DD)
  xmax=max(out1$H)
  ymax=max(out1$C)
  xmin=min(out1$H)
  ymin=min(out1$C)
  ymin=0
  ggplot() +
    geom_point(aes(H,C,colour = K),data=out1, alpha=.3) +
    scale_colour_gradient(low = "yellow", high = "black") +
    geom_line(aes(V1,V2),data=cot[[1]])+
    geom_line(aes(V1,V2),data=cot[[2]])+
    scale_x_continuous(limits=c(xmin,xmax)) +
    scale_y_continuous(limits=c(ymin,ymax))+
    theme_light() 
}
plot_confidence(3,1e3)
plot_confidence(3,5e5)

plot_confidence(6,1e3)
plot_confidence(6,5e5)


