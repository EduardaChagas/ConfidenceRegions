########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 11, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
if(!require(sp)){
  install.packages("sp")
  require(sp)
} 
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
if(!require(doParallel)){
  install.packages("doParallel")
  require(doParallel)
}

# Analysis functions -----------------------------------------------------------------------------------

k.fold.confidence.regions <- function(d = 3, N = 50000){
  
  if(N == 1000){
    HC = read.csv("../Data/HC/HC_1000.csv")[,2:4]
    names(HC) = c("H", "C", "D")
    HC$D = as.factor(HC$n)
    HC.D = subset(HC, n==D)[,-3]
    
    filePath = read.table(paste0("../../Random_/Random_", 1, "k_D", D, "-T1.dat"), header=TRUE)
    HC = matrix(data = filePath$x, nrow = 100, ncol = 3, byrow = TRUE, dimnames = NULL)
    
    HC = data.frame(H = HC[,1], 
                    C = HC[,2], 
                    D = factor(c(rep(D, 100))))
  }else{
    HC.1 = read.csv("../Data/HC/HC_50k.csv")[,2:4] #length = 8372
    names(HC.1) = c("H", "C", "D")
    HC.1$D = as.factor(HC.1$D)
    HC.1.D = subset(HC.1, D==d)[,-3]
    filePath = read.table(paste0("../../Random_/Random_", 50, "k_D", d, "-T1.dat"), header=TRUE)
    HC.2 = matrix(data = filePath$x, nrow = 100, ncol = 3, byrow = TRUE, dimnames = NULL) #100
    
    HC = data.frame(H = c(HC.1.D[,1], HC.2[,1]), 
                    C = c(HC.1.D[,2], HC.2[,2]), 
                    D = factor(c(rep(d, 8472))))
    n.k.elements = 720
    n.validation = 1270
    HC.VALIDATION = HC[c(1:n.validation),]
    write.csv(HC.VALIDATION, paste0("../Data/Regions-HC/HC-VALIDATION-D", d, "-N", N,".csv"))
    HC.TOTAL = HC[-c(1:n.validation),]
  }
  
  n.k = 10
  for(i in 1:n.k){
    print(i)
    index = seq(from = (((i-1)*n.k.elements) + 1), to = i*n.k.elements, by = 1)
    HC = HC.TOTAL[-index, ]
    save.confidence.regions(HC, d, N, i)
  }
}

save.confidence.regions <- function(HC, D, N, i){
  
  pca_HC = prcomp(x = HC[,-3], 
                  retx = TRUE, 
                  center=TRUE, scale. = TRUE)
  PC = data.frame(PC1 = pca_HC$x[,1], PC2 = pca_HC$x[,2])
  
  attach(PC)
  # Regions of 90%, 95%, 99% and 99.9% around the median
  (qpc1 = quantile(PC1, probs = c(0.001, 0.01, 0.05, 0.1, 0.5, 1-0.1, 1-0.05, 1-0.01, 1-0.001)))
  (yqpc1 = c(
    max(abs(PC2[PC1 > qpc1[4] & PC1 < qpc1[6]])),
    max(abs(PC2[PC1 > qpc1[3] & PC1 < qpc1[7]])),
    max(abs(PC2[PC1 > qpc1[2] & PC1 < qpc1[8]])),
    max(abs(PC2[PC1 > qpc1[1] & PC1 < qpc1[9]]))
  ))
  detach(PC)
  
  regions.PCA = data.frame("xmin" = numeric(5), 
                           "xmax" = numeric(5),
                           "ymin" = numeric(5),
                           "ymax" = numeric(5),
                           "region" = c("90", "95", "99", "99.9", "median"))
  
  # geom_rect uses the locations of the four corners (xmin, xmax, ymin and ymax)
  regions.PCA$xmin[1] = qpc1[4]
  regions.PCA$xmax[1] = qpc1[6]
  regions.PCA$ymin[1] = -yqpc1[1]
  regions.PCA$ymax[1] = yqpc1[1]
  
  regions.PCA$xmin[2] = qpc1[3]
  regions.PCA$xmax[2] = qpc1[7]
  regions.PCA$ymin[2] = -yqpc1[2]
  regions.PCA$ymax[2] = yqpc1[2]
  
  regions.PCA$xmin[3] = qpc1[2]
  regions.PCA$xmax[3] = qpc1[8]
  regions.PCA$ymin[3] = -yqpc1[3]
  regions.PCA$ymax[3] = yqpc1[3]
  
  regions.PCA$xmin[4] = qpc1[1]
  regions.PCA$xmax[4] = qpc1[9]
  regions.PCA$ymin[4] = -yqpc1[4]
  regions.PCA$ymax[4] = yqpc1[4]
  
  regions.PCA$xmin[5] = median(PC$PC1)
  regions.PCA$xmax[5] = median(PC$PC1)
  regions.PCA$ymin[5] = median(PC$PC2)
  regions.PCA$ymax[5] = median(PC$PC2)
  
  #write.csv(regions.PCA, paste0("../Data/Regions-PCA/regions-pca-D", D, "-N", N, i, ".csv"))
  write.csv(regions.PCA, paste0("../Data/Regions-PCA/N50kD", D, "/pca", i, ".csv"))
  
  rect90 = data.frame(xmin=qpc1[4], xmax=qpc1[6], ymin=-yqpc1[1], ymax=yqpc1[1])
  rect95 = data.frame(xmin=qpc1[3], xmax=qpc1[7], ymin=-yqpc1[2], ymax=yqpc1[2])
  rect99 = data.frame(xmin=qpc1[2], xmax=qpc1[8], ymin=-yqpc1[3], ymax=yqpc1[3])
  rect999 = data.frame(xmin=qpc1[1], xmax=qpc1[9], ymin=-yqpc1[4], ymax=yqpc1[4])
  
  #PCA to HC
  H = t(t(pca_HC$x %*% t(pca_HC$rotation)) * pca_HC$scale + pca_HC$center)
  HC.BP = data.frame("H" = H[,1], 
                     "C" = H[,2],
                     stringsAsFactors=FALSE)
  
  my.order = c(1,2,4,3)
  
  median.HC = matrix(c(median(PC$PC1), median(PC$PC2)), nrow = 1, ncol = 2)
  median.HC = t(t(median.HC %*% t(pca_HC$rotation)) * pca_HC$scale + pca_HC$center)
  
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
  
  rect95 = matrix(unlist(rect95), nrow = 2, ncol = 2)
  M = mesh(rect95[,1], rect95[,2])
  rect95 = matrix(nrow = 4, ncol = 2)
  rect95[,1] = M$x
  rect95[,2] = M$y
  rect95 = t(t(rect95 %*% t(pca_HC$rotation)) * pca_HC$scale + pca_HC$center)
  rect95 = data.frame("H" = rect95[my.order, 1], 
                      "C" = rect95[my.order, 2],
                      stringsAsFactors=FALSE)
  rect95$H[rect95$H < 0] = 0
  rect95$C[rect95$C < 0] = 0
  rect95$H[rect95$H > 1] = 1
  rect95$C[rect95$C > 1] = 1
  
  rect99 = matrix(unlist(rect99), nrow = 2, ncol = 2)
  M = mesh(rect99[,1], rect99[,2])
  rect99 = matrix(nrow = 4, ncol = 2)
  rect99[,1] = M$x
  rect99[,2] = M$y
  rect99 = t(t(rect99 %*% t(pca_HC$rotation)) * pca_HC$scale + pca_HC$center)
  rect99 = data.frame("H" = rect99[my.order, 1], 
                      "C" = rect99[my.order, 2],
                      stringsAsFactors=FALSE)
  rect99$H[rect99$H < 0] = 0
  rect99$C[rect99$C < 0] = 0
  rect99$H[rect99$H > 1] = 1
  rect99$C[rect99$C > 1] = 1
  
  rect999 =  matrix(unlist(rect999), nrow = 2, ncol = 2)
  M = mesh(rect999[,1], rect999[,2])
  rect999 = matrix(nrow = 4, ncol = 2)
  rect999[,1] = M$x
  rect999[,2] = M$y
  rect999 = t(t(rect999 %*% t(pca_HC$rotation)) * pca_HC$scale + pca_HC$center)
  rect999 = data.frame("H" = rect999[my.order, 1], 
                       "C" = rect999[my.order, 2],
                       stringsAsFactors=FALSE)
  rect999$H[rect999$H < 0] = 0
  rect999$C[rect999$C < 0] = 0
  rect999$H[rect999$H > 1] = 1
  rect999$C[rect999$C > 1] = 1
  
  regions.HC = data.frame("H" = c(rect90$H, rect95$H, rect99$H, rect999$H, median.HC[1]), 
                          "C" = c(rect90$C, rect95$C, rect99$C, rect999$C, median.HC[2]),
                          "region" = c(rep("90", 4), rep("95", 4), rep("99", 4), rep("99.9", 4), "median"))
  
  #write.csv(regions.HC, paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, i, ".csv"))
  write.csv(regions.HC, paste0("../Data/Regions-HC/N50kD", D, "/hc", i, ".csv"))
}

plot.pca.space <- function(HC, D, N){
  
  pca_HC = prcomp(x = HC[,-3], 
                  retx = TRUE, 
                  center=TRUE, scale. = TRUE)
  
  PC = data.frame(PC1 = pca_HC$x[,1], PC2 = pca_HC$x[,2])
  
  pca.data = read.csv(paste0("../Data/Regions-PCA/regions-pca-D", D, "-N", N, ".csv"))
  
  # geom_rect uses the locations of the four corners (xmin, xmax, ymin and ymax)
  rect90 = data.frame(xmin = pca.data$xmin[1], xmax = pca.data$xmax[1], 
                      ymin = pca.data$ymin[1], ymax = pca.data$ymax[1])
  rect95 = data.frame(xmin = pca.data$xmin[2], xmax = pca.data$xmax[2], 
                      ymin = pca.data$ymin[2], ymax = pca.data$ymax[2])
  rect99 = data.frame(xmin = pca.data$xmin[3], xmax = pca.data$xmax[3], 
                      ymin = pca.data$ymin[3], ymax = pca.data$ymax[3])
  rect999 = data.frame(xmin = pca.data$xmin[4], xmax = pca.data$xmax[4], 
                       ymin = pca.data$ymin[4], ymax = pca.data$ymax[4])
  median.pca = data.frame(PC1 = pca.data$xmin[5], PC2 = pca.data$ymax[5])
  
  p = ggplot(PC, aes(x = PC1, y = PC2)) +
    geom_point(size = .3, alpha = .4) +
    geom_point(data = median.pca, aes(x = PC1, y = PC2), colour = "red") +
    geom_rect(data = rect90, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              fill = "red", alpha = 0.1, inherit.aes = FALSE) +
    geom_rect(data = rect95, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              fill = "blue", alpha = 0.1, inherit.aes = FALSE) +
    geom_rect(data = rect99, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              fill = "green", alpha = 0.1, inherit.aes = FALSE) +
    geom_rect(data = rect999, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              fill = "yellow", alpha = 0.2, inherit.aes = FALSE) +
    xlab("First Principal Component") +
    ylab("Second Principal Component") +
    theme(plot.title = element_text(hjust=0.5)) + 
    theme_clean()
  
  return(p)
}

plot.hc.pca.points <- function(HC, D, N, title){
  
  pca_HC = prcomp(x = HC[,-3], 
                  retx = TRUE, 
                  center=TRUE, scale. = TRUE)
  H = t(t(pca_HC$x %*% t(pca_HC$rotation)) * pca_HC$scale + pca_HC$center)
  
  HC = data.frame("H" = H[,1], 
                  "C" = H[,2],
                  stringsAsFactors=FALSE)
  
  hc.points = read.csv(paste0("../../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
  
  colors = coloring.regions.HC(HC, D, N)
  colors = colors + 1
  
  rainbow.colors = palette(c( "#000000",
                              "#3F84E5",
                              "#B20D30", 
                              "#3F784C",
                              "#EFCD4A"))
  
  regions = c("100%", "99.9%", "99%", "95%", "90%")[colors]
  
  HC.BP = data.frame("H" = H[,1], 
                     "C" = H[,2],
                     "Regions" = regions,
                     stringsAsFactors=FALSE)
  
  p = ggplot(HC.BP, aes(x = H, y = C)) +
      ggtitle(title) +
      geom_point(size = 1.5, alpha = .5, aes(color = Regions)) +
      geom_point(aes(x = hc.points$H[17], y = hc.points$C[17]), colour="red") +
      xlab("H") +
      ylab("C") +
      theme(plot.title = element_text(hjust=0.5)) + 
      theme_clean()
  
  return(p)
}


plot.hc.pca.boxes <- function(HC, D, N, title){
  
  pca_HC = prcomp(x = HC[,-3], 
                  retx = TRUE, 
                  center=TRUE, scale. = TRUE)
  H = t(t(pca_HC$x %*% t(pca_HC$rotation)) * pca_HC$scale + pca_HC$center)
  
  HC.BP = data.frame("H" = H[,1], 
                     "C" = H[,2],
                     stringsAsFactors=FALSE)
  
  hc.points = read.csv(paste0("../../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
  rect90 = data.frame(H = hc.points$H[1:4], C = hc.points$C[1:4])
  rect95 = data.frame(H = hc.points$H[5:8], C = hc.points$C[5:8])
  rect99 = data.frame(H = hc.points$H[9:12], C = hc.points$C[9:12])
  rect999 = data.frame(H = hc.points$H[13:16], C = hc.points$C[13:16])
  
  p = ggplot(HC.BP, aes(x = H, y = C)) +
    ggtitle(title) +
    geom_point(size=.8, alpha=.2) +
    geom_point(aes(x = hc.points$H[17], y = hc.points$C[17]), colour="red") +
    geom_polygon(data=rect90, aes(x = H, y = C), fill="white", alpha=0.9, inherit.aes = FALSE) +
    geom_polygon(data=rect95, aes(x = H, y = C), fill="red", alpha=0.5, inherit.aes = FALSE) +
    geom_polygon(data=rect99, aes(x = H, y = C), fill="green", alpha=0.2, inherit.aes = FALSE) +
    geom_polygon(data=rect999, aes(x = H, y = C), fill="yellow", alpha=0.2, inherit.aes = FALSE) +
    xlab("H") +
    ylab("C") +
    theme(plot.title = element_text(hjust=0.5)) + 
    theme_clean()
  
  return(p)
}

points.confidence.regions <- function(region, D, N){
  
  hc.points = read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))[2:4]
  
  if(region == 90){
    rect90 = data.frame(H = hc.points$H[1:4], C = hc.points$C[1:4])
    point = rect90
    
    #cat("Region of 90% around the median \n")
    #cat("-Point 1: ", rect90[1,1], ", ", rect90[1,2], "\n")
    #cat("-Point 2: ", rect90[2,1], ", ", rect90[2,2], "\n")
    #cat("-Point 3: ", rect90[3,1], ", ", rect90[3,2], "\n")
    #cat("-Point 4: ", rect90[4,1], ", ", rect90[4,2], "\n", "\n")
    
  }else if(region == 95){
    rect95 = data.frame(H = hc.points$H[5:8], C = hc.points$C[5:8])
    point = rect95
    
    #cat("Region of 95% around the median \n")
    #cat("-Point 1: ", rect95[1,1], ", ", rect95[1,2], "\n")
    #cat("-Point 2: ", rect95[2,1], ", ", rect95[2,2], "\n")
    #cat("-Point 3: ", rect95[3,1], ", ", rect95[3,2], "\n")
    #cat("-Point 4: ", rect95[4,1], ", ", rect95[4,2], "\n", "\n")
    
  }else if(region == 99){
    rect99 = data.frame(H = hc.points$H[9:12], C = hc.points$C[9:12])
    point = rect99
    
    #cat("Region of 99% around the median \n")
    #cat("-Point 1: ", rect99[1,1], ", ", rect99[1,2], "\n")
    #cat("-Point 2: ", rect99[2,1], ", ", rect99[2,2], "\n")
    #cat("-Point 3: ", rect99[3,1], ", ", rect99[3,2], "\n")
    #cat("-Point 4: ", rect99[4,1], ", ", rect99[4,2], "\n", "\n")
    
  }else if(region == 99.9){
    rect999 = data.frame(H = hc.points$H[13:16], C = hc.points$C[13:16])
    point = rect999
    
    #cat("Region of 99.9% around the median \n")
    #cat("-Point 1: ", rect999[1,1], ", ", rect999[1,2], "\n")
    #cat("-Point 2: ", rect999[2,1], ", ", rect999[2,2], "\n")
    #cat("-Point 3: ", rect999[3,1], ", ", rect999[3,2], "\n")
    #cat("-Point 4: ", rect999[4,1], ", ", rect999[4,2], "\n", "\n")
    
  }
  return(point)
}

hc.pca.boxes.50k <- function(){
  
  HC = read.csv("../Data/HC/HC_50k.csv")[,2:4]
  names(HC) = c("H", "C", "n")
  HC$n = as.factor(HC$n)
  
  HC.3 = subset(HC, n==3)[,-3]
  pca_HC.3 = prcomp(x = HC.3[,-3], 
                    retx = TRUE, 
                    center=TRUE, scale. = TRUE)
  H3 = t(t(pca_HC.3$x %*% t(pca_HC.3$rotation)) * pca_HC.3$scale + pca_HC.3$center)
  
  HC.4 = subset(HC, n==4)[,-3]
  pca_HC.4 = prcomp(x = HC.4[,-3], 
                    retx = TRUE, 
                    center=TRUE, scale. = TRUE)
  H4 = t(t(pca_HC.4$x %*% t(pca_HC.4$rotation)) * pca_HC.4$scale + pca_HC.4$center)
  
  HC.5 = subset(HC, n==5)[,-3]
  pca_HC.5 = prcomp(x = HC.5[,-3], 
                    retx = TRUE, 
                    center=TRUE, scale. = TRUE)
  H5 = t(t(pca_HC.5$x %*% t(pca_HC.5$rotation)) * pca_HC.5$scale + pca_HC.5$center)
  
  HC.6 = subset(HC, n==6)[,-3]
  pca_HC.6 = prcomp(x = HC.6[,-3], 
                    retx = TRUE, 
                    center=TRUE, scale. = TRUE)
  H6 = t(t(pca_HC.6$x %*% t(pca_HC.6$rotation)) * pca_HC.6$scale + pca_HC.6$center)
  
  HC.BP = data.frame("H" = c(H3[,1], H4[,1], H5[,1], H6[,1]),
                     "C" = c(H3[,2], H4[,2], H5[,2], H6[,2]),
                     D = as.factor(c(rep("D = 3", 8372), rep("D = 4", 8372), rep("D = 5", 8372), rep("D = 6", 8372))))
  HC.BP = melt(data = HC.BP, id.vars = c(1,2), variable.name = "D")
  
  regions.D3 = read.csv("../Data/Regions-HC/regions-hc-D3-N50000.csv")[2:4]
  regions.D4 = read.csv("../Data/Regions-HC/regions-hc-D4-N50000.csv")[2:4]
  regions.D5 = read.csv("../Data/Regions-HC/regions-hc-D5-N50000.csv")[2:4]
  regions.D6 = read.csv("../Data/Regions-HC/regions-hc-D6-N50000.csv")[2:4]
  
  rect.95 = data.frame(H = c(regions.D3$H[5:8], regions.D4$H[5:8], regions.D5$H[5:8], regions.D6$H[5:8]), 
                       C = c(regions.D3$C[5:8], regions.D4$C[5:8], regions.D5$C[5:8], regions.D6$C[5:8]), 
                       D = as.factor(c(rep("D = 3", 4), rep("D = 4", 4), rep("D = 5", 4), rep("D = 6", 4))))
  rect.95 = melt(data = rect.95, id.vars = c(1,2), variable.name = "D")
  
  rect.99 = data.frame(H = c(regions.D3$H[9:12], regions.D4$H[9:12], regions.D5$H[9:12], regions.D6$H[9:12]), 
                       C = c(regions.D3$C[9:12], regions.D4$C[9:12], regions.D5$C[9:12], regions.D6$C[9:12]), 
                       D = as.factor(c(rep("D = 3", 4), rep("D = 4", 4), rep("D = 5", 4), rep("D = 6", 4))))
  rect.99 = melt(data = rect.99, id.vars = c(1,2), variable.name = "D")
  
  median = data.frame(H = c(regions.D3$H[17], regions.D4$H[17], regions.D5$H[17], regions.D6$H[17]), 
                      C = c(regions.D3$C[17], regions.D4$C[17], regions.D5$C[17], regions.D6$C[17]), 
                      D = as.factor(c("D = 3", "D = 4", "D = 5", "D = 6")))
  median = melt(data = median, id.vars = c(1,2), variable.name = "D")
  
  pdf("N50kBoxes95.pdf", width = 16, height = 12)
  p = ggplot(HC.BP, aes(x = H, y = C)) +
    ggtitle("95% confidence region") +
    geom_point(size=.8, alpha=.05) +
    facet_wrap(. ~ value, scales = "free") +
    geom_point(data=median, aes(x = H, y = C), colour="red") +
    geom_polygon(data=rect.95, aes(x = H, y = C), fill="yellow", alpha=2, inherit.aes = FALSE) +
    xlab("H") +
    ylab("C") +
    theme_few(base_size = 20, base_family = "serif") +  
    theme(plot.title = element_text(hjust=0.5))
  print(p)
  dev.off()
  
  pdf("N50kBoxes99.pdf", width = 16, height = 12)
  p = ggplot(HC.BP, aes(x = H, y = C)) +
    ggtitle("99% confidence region") +
    geom_point(size=.8, alpha=.05) +
    facet_wrap(. ~ value, scales = "free") +
    geom_point(data=median, aes(x = H, y = C), colour="red") +
    geom_polygon(data=rect.99, aes(x = H, y = C), fill="green", alpha=2, inherit.aes = FALSE) +
    xlab("H") +
    ylab("C") +
    theme_few(base_size = 20, base_family = "serif") +  
    theme(plot.title = element_text(hjust=0.5))
  print(p)
  dev.off()
}


coloring.regions.HC <- function(HC, D, N){
  colors = rep(0, dim(HC)[1])
  
  points.90 = points.confidence.regions(90, D, N)
  test.90 = point.in.polygon(HC$H, HC$C, points.90$H, points.90$C)
  
  points.95 = points.confidence.regions(95, D, N)
  test.95 = point.in.polygon(HC$H, HC$C, points.95$H, points.95$C)
  
  points.99 = points.confidence.regions(99, D, N)
  test.99 = point.in.polygon(HC$H, HC$C, points.99$H, points.99$C)
  
  points.999 = points.confidence.regions(99.9, D, N)
  test.999 = point.in.polygon(HC$H, HC$C, points.999$H, points.999$C)
  
  inside.999 = which(test.999 != 0)
  colors[inside.999] = 1
  inside.99 = which(test.99 != 0)
  colors[inside.99] = 2
  inside.95 = which(test.95 != 0)
  colors[inside.95] = 3
  inside.90 = which(test.90 != 0)
  colors[inside.90] = 4
  
  return(colors)
}

