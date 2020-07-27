########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 12, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Confidence-regions-functions.R")

# Analysis functions -----------------------------------------------------------------------------------

get.confidence.region <- function(HC, ph, pc, D, N){
  
  pca_HC = prcomp(x = HC[,-3], retx = TRUE, 
                  center=TRUE, scale. = TRUE)
  PC = data.frame(PC1 = pca_HC$x[,1], PC2 = pca_HC$x[,2])
  
  test = TRUE
  step.loop = 0
  region.test = 1.001
  
  attach(PC)
  while(test){
    step.loop = step.loop + 0.001
    region.test = region.test - 0.001
    
    (qpc1 = quantile(PC1, probs = c(step.loop, 0.5, 1-step.loop)))
    (yqpc1 = c(max(abs(PC2[PC1 > qpc1[1] & PC1 < qpc1[3]]))))
    rect = data.frame(xmin = qpc1[1], xmax = qpc1[3], ymin = -yqpc1[1], ymax = yqpc1[1])
    rect = matrix(unlist(rect), nrow = 2, ncol = 2)
    
    my.order = c(1,2,4,3)
    M = mesh(rect[,1], rect[,2])
    rect = matrix(nrow = 4, ncol = 2)
    rect[,1] = M$x
    rect[,2] = M$y
    rect = t(t(rect %*% t(pca_HC$rotation)) * pca_HC$scale + pca_HC$center)
    rect = data.frame("H" = rect[my.order, 1], 
                      "C" = rect[my.order, 2],
                      stringsAsFactors=FALSE)
    rect$H[rect$H < 0] = 0
    rect$C[rect$C < 0] = 0
    rect$H[rect$H > 1] = 1
    rect$C[rect$C > 1] = 1
    
    test = point.in.polygon(ph, pc, rect$H, rect$C)
  }
  detach(PC)
  cat("Level of the smallest confidence region to which it belongs this point: ", region.test*100, "%\n")
  return(region.test)
}
