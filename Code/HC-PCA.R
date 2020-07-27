########################################################################################################
# Author: Eduarda Chagas
# Date : Jun 11, 2020
# Contact: eduarda.chagas@dcc.ufmg.br
########################################################################################################

# Packages and sources ---------------------------------------------------------------------------------
source("Confidence-regions-functions.R")
source("PCA-Histogram.R")
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
if(!require(ggpubr)){
  install.packages("ggpubr")
  require(ggpubr)
}

# Hypotheses that I DID NOT USE  -----------------------------------------------------------------------
### 1) Each confidence region must include the median (in the HxC plane) DUHHH!!!
### 2) The shape of the confidence regions must be a simple parametric form well-suited to the shape of the points
### 3) It is easier to make the analysis in the PCA plane, and then "translate" it to the HxC plane
### 4) The shape of the density looks symmetric w.r.t. the first PC, i.e., it would suffice to find a shape of the form
###    d(| PC1 |, PC2)

# Analysis with N = 1000 --------------------------------------------------------------------------------

HC = read.csv("../Data/HC/HC_1000.csv")[,2:4]
names(HC) = c("H", "C", "n")
HC$n = as.factor(HC$n)

HC.3 = subset(HC, n==3)[,-3]
#save.confidence.regions(HC.3, D = 3, N = 1000)
p.3.1 = plot.pca.space(HC.3, D = 3, N = 1000)
p.3.2 = plot.hc.pca.points(HC.3, D = 3, N = 1000, "D = 3")

HC.4 = subset(HC, n==4)[,-3]
p.4.1 = plot.pca.space(HC.4, D = 4, N = 1000)
p.4.2 = plot.hc.pca.points(HC.4, D = 4, N = 1000, "D = 4")

HC.5 = subset(HC, n==5)[,-3]
p.5.1 = plot.pca.space(HC.5, D = 5, N = 1000)
p.5.2 = plot.hc.pca.points(HC.5, D = 5, N = 1000, "D = 5")

HC.6 = subset(HC, n==6)[,-3]
p.6.1 = plot.pca.space(HC.6, D = 6, N = 1000)
p.6.2 = plot.hc.pca.points(HC.6, D = 6, N = 1000, "D = 6")

pdf("N1000Points.pdf", width = 10, height = 5)
ggarrange(p.3.2, p.4.2, p.5.2, p.6.2, ncol = 4, nrow = 1, common.legend = TRUE, legend = "right") + 
  theme_few() + theme(text=element_text(size = 12, family="Times"), axis.text.x=element_blank(), axis.text.y=element_blank(),plot.title = element_text(hjust=0.5))
dev.off()

# Analysis with N = 50k -----------------------------------------------------------------------------------

HC = read.csv("../Data/HC/HC_50k.csv")[,2:4]
names(HC) = c("H", "C", "n")
HC$n = as.factor(HC$n)

HC.3 = subset(HC, n==3)[,-3]
p.3.1 = plot.pca.space(HC.3, D = 3, N = 50000)
p.3.2 = plot.hc.pca.boxes(HC.3, D = 3, N = 50000, "D = 3")

HC.4 = subset(HC, n==4)[,-3]
p.4.1 = plot.pca.space(HC.4, D = 4, N = 50000)
p.4.2 = plot.hc.pca.boxes(HC.4, D = 4, N = 50000, "D = 4")

HC.5 = subset(HC, n==5)[,-3]
p.5.1 = plot.pca.space(HC.5, D = 5, N = 50000)
p.5.2 = plot.hc.pca.boxes(HC.5, D = 5, N = 50000, "D = 5")

HC.6 = subset(HC, n==6)[,-3]
p.6.1 = plot.pca.space(HC.6, D = 6, N = 50000)
p.6.2 = plot.hc.pca.boxes(HC.6, D = 6, N = 50000, "D = 6")

pdf("N50kBoxes.pdf", width = 16, height = 8)
ggarrange(p.3.2, p.4.2, p.5.2, p.6.2, ncol = 4, nrow = 1, common.legend = TRUE, legend = "right") + 
  theme_few() + theme(text=element_text(size = 12, family="Times"), axis.text.x=element_blank(), axis.text.y=element_blank(),plot.title = element_text(hjust=0.5))
dev.off()