cotas <- function(dimension){
  c1x = readingMPR(dimension,1)
  c1y = readingMPR(dimension,2)
  c2x = readingMPR(dimension,3)
  c2y = readingMPR(dimension,4)
  
  cotas.1xy = data.frame("c1x" = c1x, "c1y" = c1y)
  cotas.2xy = data.frame("c2x" = c2x, "c2y" = c2y)
  
  p = ggplot(cotas.1xy, aes(c1x, c1y)) + geom_line(size=0.5, color="gray") +
    geom_line(aes(x=c2x, y=c2y), cotas.2xy, size=0.5, color="gray") +
    theme(plot.title = element_text(hjust=0.5)) 
  return(p)
}