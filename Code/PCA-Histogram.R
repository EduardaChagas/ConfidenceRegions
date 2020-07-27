########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 16, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
if(!require(plot3D)){
  install.packages("plot3D")
  require(plot3D)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  require(ggplot2)
}
if(!require(ggthemes)){
  install.packages("ggthemes")
  require(ggthemes)
}
if(!require(reshape2)){
  install.packages("reshape2")
  require(reshape2)
}

# Analysis functions -----------------------------------------------------------------------------------

pca.histogram <- function(){
  
  HC = read.csv("../Data/HC/HC_50k.csv")[,2:4]
  names(HC) = c("H", "C", "n")
  HC$n = as.factor(HC$n)
  
  HC.3 = subset(HC, n==3)[,-3]
  pca_HC.3 = prcomp(x = HC.3[,-3], 
                    retx = TRUE, 
                    center=TRUE, scale. = TRUE)
  
  HC.4 = subset(HC, n==4)[,-3]
  pca_HC.4 = prcomp(x = HC.4[,-3], 
                    retx = TRUE, 
                    center=TRUE, scale. = TRUE)
  
  HC.5 = subset(HC, n==5)[,-3]
  pca_HC.5 = prcomp(x = HC.5[,-3], 
                    retx = TRUE, 
                    center=TRUE, scale. = TRUE)
  
  HC.6 = subset(HC, n==6)[,-3]
  pca_HC.6 = prcomp(x = HC.6[,-3], 
                    retx = TRUE, 
                    center=TRUE, scale. = TRUE)
  
  PC = data.frame(PC1 = c(pca_HC.3$x[,1], pca_HC.4$x[,1], pca_HC.5$x[,1], pca_HC.6$x[,1]),
                  PC2 = c(pca_HC.3$x[,2], pca_HC.4$x[,2], pca_HC.5$x[,2], pca_HC.6$x[,2]), 
                  D = as.factor(c(rep("D = 3", 8372), rep("D = 4", 8372), rep("D = 5", 8372), rep("D = 6", 8372))))
  PC = melt(data = PC, id.vars = c(1,2), variable.name = "D")
  
  pdf("PCA-hist-50k.pdf", width = 32, height = 10)
  p = ggplot(PC) +
    ggtitle("First Component Histogram") +
    xlab("") +
    ylab("") +
    geom_histogram(aes(x = PC1, y = ..density..), binwidth = 1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
    facet_grid(. ~ value) + 
    theme_few(base_size = 35, base_family = "serif") +  
    theme(plot.title = element_text(hjust=0.5)) 
  print(p)
  dev.off()
  
  return(p)
}


plot.pca.histogram <- function(HC, D, N){
  
  pca_HC = prcomp(x = HC[,-3], 
                  retx = TRUE, 
                  center=TRUE, scale. = TRUE)
  
  PC = data.frame(PC1 = pca_HC$x[,1], PC2 = pca_HC$x[,2])
  
  pca.data = read.csv(paste0("../../Data/Regions-PCA/regions-pca-D", D, "-N", N, ".csv"))
  median.pca = data.frame(PC1 = pca.data$xmin[5], PC2 = pca.data$ymax[5])
  
  p = ggplot(PC) +
      ggtitle(paste0("First Component Histogram - D = ", D, " & N = ", N)) +
      xlab("") +
      ylab("") +
      geom_histogram(aes(x = PC1, y = ..density..), binwidth = 1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      theme_clean() + 
      theme(plot.title = element_text(hjust=0.5)) 
  
  return(p)
}
