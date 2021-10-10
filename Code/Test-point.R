########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 12, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Confidence-regions-functions.R")

# Analysis functions -----------------------------------------------------------------------------------
test.point <- function(H, C, D, N, region){
  
  points = points.confidence.regions(region, D, N)
  test = point.in.polygon(H, C, points$H, points$C)
  
  if(test == 0){
    result = FALSE
    #cat("This point does not belong to the region \n")
  }
  else{
    result = TRUE
    #cat("This point is within the confidence region \n")
  }
  
  return(result)
}

test.set.point <- function(HC, D, N, region){
  
  points = points.confidence.regions(region, D, N)
  test = rep(FALSE, dim(HC)[1])
  
  for(i in 1:dim(HC)[1]){
    test[i] = point.in.polygon(HC$H[i], HC$C[i], points$H, points$C)
  }
  
  if(length(test[test == 1]) != 0)
    cat("Points inside the regions of", region, "%: ", (length(test[test == 1])/length(test))*100, '%\n')
  
  return(test)
}

p.value.set.point <- function(HC, D, N, region){
  
  points = points.confidence.regions(region, D, N)
  test = rep(FALSE, dim(HC)[1])
  
  for(i in 1:dim(HC)[1]){
    test[i] = point.in.polygon(HC$H[i], HC$C[i], points$H, points$C)
  }
  
  return((length(test[test == 1])/length(test)))
}

#test.point(H = 0.9998, C = 0.0001, D = 3, N = 1000, region = 90)
