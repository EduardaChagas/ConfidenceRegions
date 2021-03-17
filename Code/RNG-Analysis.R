  ########################################################################################################
  # Author: Eduarda Chagas
  # Date : Jun 30, 2020
  # Contact: eduarda.chagas@dcc.ufmg.br
  ########################################################################################################
  
  # Packages and sources ---------------------------------------------------------------------------------
  source("Test-point.R")
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
  if(!require(reshape2)){
    install.packages("reshape2")
    require(reshape2)
  }
  if(!require(latex2exp)){
    install.packages("latex2exp")
    require(latex2exp)
  }
  
  # Analysis functions -----------------------------------------------------------------------------------
  RNG.HC.confidence.regions <- function(D = 3, N = 1000, horizontal = 1){
    
    if(N == 1000){
      if(D == 3)
        filePath <- read.table(url(paste0("https://www.dropbox.com/s/5z1f01nu449d44m/Random_", 1, "k_D", D, "-T1.dat?dl=1")))
      if(D == 4)
        filePath <- read.table(url(paste0("https://www.dropbox.com/s/vquh76d4kaqoxcm/Random_", 1, "k_D", D, "-T1.dat?dl=1")))
      if(D == 5)
        filePath <- read.table(url(paste0("https://www.dropbox.com/s/37tygdzigybzu7u/Random_", 1, "k_D", D, "-T1.dat?dl=1")))
      if(D == 6)
        filePath <- read.table(url(paste0("https://www.dropbox.com/s/0ez7bq8zn98phqg/Random_", 1, "k_D", D, "-T1.dat?dl=1")))
    }else{
      if(D == 3)
        filePath <- read.table(url(paste0("https://www.dropbox.com/s/9enu26i3sdd77a4/Random_", 50, "k_D", D, "-T1.dat?dl=1")))
      if(D == 4)
        filePath <- read.table(url(paste0("https://www.dropbox.com/s/tnqsif6uwmsftki/Random_", 50, "k_D", D, "-T1.dat?dl=1")))
      if(D == 5)
        filePath <- read.table(url(paste0("https://www.dropbox.com/s/8etdtadnx68dzcx/Random_", 50, "k_D", D, "-T1.dat?dl=1")))
      if(D == 6)
        filePath <- read.table(url(paste0("https://www.dropbox.com/s/qed53mi0crxk6tz/Random_", 50, "k_D", D, "-T1.dat?dl=1")))
    }
    
    HC <- matrix(data = filePath$x, nrow = 100, ncol = 3, byrow = TRUE, dimnames = NULL)
    
    HC <- data.frame(H = HC[,1], 
                     C = HC[,2], 
                     D = factor(c(rep(D, 100))))
    
    result.95 = test.set.point(HC, D, N, 95)
    result.99 = test.set.point(HC, D, N, 99)
    
    HC <- melt(data = HC, id.vars = c(1, 2), variable.name = "D")
    
    regions <- read.csv(paste0("../Data/Regions-HC/regions-hc-D", D, "-N", N, ".csv"))
    
    rect.95 <- data.frame(H = c(regions$H[5:8]), 
                          C = c(regions$C[5:8]), 
                          D = factor(c(rep(D, 4))))
    rect.95 <- melt(data = rect.95, id.vars = c(1, 2), variable.name = "D")
    
    rect.99 <- data.frame(H = c(regions$H[9:12]), 
                          C = c(regions$C[9:12]), 
                          D = factor(c(rep(D, 4))))
    rect.99 <- melt(data = rect.99, id.vars = c(1, 2), variable.name = "D")
    
    median <- data.frame(H = c(regions$H[17]), 
                         C = c(regions$C[17]), 
                         D = factor(D))
    median <- melt(data = median, id.vars = c(1, 2), variable.name = "D")
    HC$H = round(HC$H, digits = 6)
    
    ggplot(data = HC, aes(x = H, y = C)) +
      geom_point(size = 1, alpha = .9) +
      geom_polygon(data=rect.95, aes(x = H, y = C), fill="red", alpha=0.5, inherit.aes = FALSE) +
      geom_polygon(data=rect.99, aes(x = H, y = C), fill="green", alpha=0.3, inherit.aes = FALSE) +
      geom_point(data = median, aes(x = H, y = C), colour = "red", size = 1) +
      #xlab("H") + ylab("C") + 
      xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
      theme_few(base_size = 25, base_family = "sans") +  
      theme(plot.title = element_text(hjust=0.5),
            axis.text.x = element_text(angle = 30)) -> p
    
    xlab = ""
    if(horizontal == 1)
      xlab = bquote(italic(D)==.(D))
    
    p = p + ggtitle(xlab)
    return(p)
  }
  
  # Analysis with N = 1k ---------------------------------------------------------------------------------
  p1 = RNG.HC.confidence.regions(D = 3, N = 1000)
  print(p1)
  
  p2 = RNG.HC.confidence.regions(D = 4, N = 1000) + ylab(NULL)
  print(p2)
  
  p3 = RNG.HC.confidence.regions(D = 5, N = 1000) + ylab(NULL)
  print(p3)
  
  p4 = RNG.HC.confidence.regions(D = 6, N = 1000) + ylab(NULL)
  print(p4)
  
  
  pdf("../Reports/JOURNAL - Confidence Regions/Figures/RNG-1000.pdf", width = 24, height = 8)
  ggarrange(p1, p2, p3, p4, ncol = 4, nrow = 1, common.legend = TRUE, 
            legend = "right", 
            label.x = expression(italic(H)), 
            label.y = expression(italic(C))) + 
    ggtitle(expression(italic(T)==1000)) +
    theme_few(base_size = 30, base_family = "sans") + 
    # theme(text=element_text(size=25, family="Times"), plot.title = element_text(hjust=0.5)) + 
    guides(colour = guide_legend(override.aes = list(size=3)))
  dev.off()
  
  # Analysis with N = 50k --------------------------------------------------------------------------------
  p1 = RNG.HC.confidence.regions(D = 3, N = 50000)
  print(p1)
  
  p2 = RNG.HC.confidence.regions(D = 4, N = 50000) + ylab(NULL)
  print(p2)
  
  p3 = RNG.HC.confidence.regions(D = 5, N = 50000) + ylab(NULL)
  print(p3)
  
  p4 = RNG.HC.confidence.regions(D = 6, N = 50000) + ylab(NULL)
  print(p4)
  
  pdf("../Reports/JOURNAL - Confidence Regions/Figures/RNG-50000.pdf", width = 24, height = 8)
  ggarrange(p1, p2, p3, p4, ncol = 4, nrow = 1, common.legend = TRUE, legend = "right") + 
    ggtitle(expression(italic(T)==50000)) +
    xlab(expression(italic(H))) + ylab(expression(italic(C))) + 
    theme_few(base_size = 30, base_family = "sans") + 
    guides(colour = guide_legend(override.aes = list(size=3)))
  dev.off()
  