
pca1 = read.csv("../Data/Regions-PCA/N50kD3/pca1.csv")
pca2 = read.csv("../Data/Regions-PCA/N50kD3/pca2.csv")
pca3 = read.csv("../Data/Regions-PCA/N50kD3/pca3.csv")
pca4 = read.csv("../Data/Regions-PCA/N50kD3/pca4.csv")
pca5 = read.csv("../Data/Regions-PCA/N50kD3/pca5.csv")
pca6 = read.csv("../Data/Regions-PCA/N50kD3/pca6.csv")
pca7 = read.csv("../Data/Regions-PCA/N50kD3/pca7.csv")
pca8 = read.csv("../Data/Regions-PCA/N50kD3/pca8.csv")
pca9 = read.csv("../Data/Regions-PCA/N50kD3/pca9.csv")
pca10 = read.csv("../Data/Regions-PCA/N50kD3/pca10.csv")

rect90.1 = data.frame(xmin = pca1$xmin[1], xmax = pca1$xmax[1], 
                      ymin = pca1$ymin[1], ymax = pca1$ymax[1])
rect90.2 = data.frame(xmin = pca2$xmin[1], xmax = pca2$xmax[1], 
                      ymin = pca2$ymin[1], ymax = pca2$ymax[1])
rect90.3 = data.frame(xmin = pca3$xmin[1], xmax = pca3$xmax[1], 
                      ymin = pca3$ymin[1], ymax = pca3$ymax[1])
rect90.4 = data.frame(xmin = pca4$xmin[1], xmax = pca4$xmax[1], 
                      ymin = pca4$ymin[1], ymax = pca4$ymax[1])
rect90.5 = data.frame(xmin = pca5$xmin[1], xmax = pca5$xmax[1], 
                      ymin = pca5$ymin[1], ymax = pca5$ymax[1])
rect90.6 = data.frame(xmin = pca6$xmin[1], xmax = pca6$xmax[1], 
                      ymin = pca6$ymin[1], ymax = pca6$ymax[1])
rect90.7 = data.frame(xmin = pca7$xmin[1], xmax = pca7$xmax[1], 
                      ymin = pca7$ymin[1], ymax = pca7$ymax[1])
rect90.8 = data.frame(xmin = pca8$xmin[1], xmax = pca8$xmax[1], 
                      ymin = pca8$ymin[1], ymax = pca8$ymax[1])
rect90.9 = data.frame(xmin = pca9$xmin[1], xmax = pca9$xmax[1], 
                      ymin = pca9$ymin[1], ymax = pca9$ymax[1])
rect90.10 = data.frame(xmin = pca10$xmin[1], xmax = pca10$xmax[1], 
                      ymin = pca10$ymin[1], ymax = pca10$ymax[1])

rect95.1 = data.frame(xmin = pca1$xmin[2], xmax = pca1$xmax[2], 
                      ymin = pca1$ymin[2], ymax = pca1$ymax[2])
rect95.2 = data.frame(xmin = pca2$xmin[2], xmax = pca2$xmax[2], 
                      ymin = pca2$ymin[2], ymax = pca2$ymax[2])
rect95.3 = data.frame(xmin = pca3$xmin[2], xmax = pca3$xmax[2], 
                      ymin = pca3$ymin[2], ymax = pca3$ymax[2])
rect95.4 = data.frame(xmin = pca4$xmin[2], xmax = pca4$xmax[2], 
                      ymin = pca4$ymin[2], ymax = pca4$ymax[2])
rect95.5 = data.frame(xmin = pca5$xmin[2], xmax = pca5$xmax[2], 
                      ymin = pca5$ymin[2], ymax = pca5$ymax[2])
rect95.6 = data.frame(xmin = pca6$xmin[2], xmax = pca6$xmax[2], 
                      ymin = pca6$ymin[2], ymax = pca6$ymax[2])
rect95.7 = data.frame(xmin = pca7$xmin[2], xmax = pca7$xmax[2], 
                      ymin = pca7$ymin[2], ymax = pca7$ymax[2])
rect95.8 = data.frame(xmin = pca8$xmin[2], xmax = pca8$xmax[2], 
                      ymin = pca8$ymin[2], ymax = pca8$ymax[2])
rect95.9 = data.frame(xmin = pca9$xmin[2], xmax = pca9$xmax[2], 
                      ymin = pca9$ymin[2], ymax = pca9$ymax[2])
rect95.10 = data.frame(xmin = pca10$xmin[2], xmax = pca10$xmax[2], 
                       ymin = pca10$ymin[2], ymax = pca10$ymax[2])

rect99.1 = data.frame(xmin = pca1$xmin[3], xmax = pca1$xmax[3], 
                      ymin = pca1$ymin[3], ymax = pca1$ymax[3])
rect99.2 = data.frame(xmin = pca2$xmin[3], xmax = pca2$xmax[3], 
                      ymin = pca2$ymin[3], ymax = pca2$ymax[3])
rect99.3 = data.frame(xmin = pca3$xmin[3], xmax = pca3$xmax[3], 
                      ymin = pca3$ymin[3], ymax = pca3$ymax[3])
rect99.4 = data.frame(xmin = pca4$xmin[3], xmax = pca4$xmax[3], 
                      ymin = pca4$ymin[3], ymax = pca4$ymax[3])
rect99.5 = data.frame(xmin = pca5$xmin[3], xmax = pca5$xmax[3], 
                      ymin = pca5$ymin[3], ymax = pca5$ymax[3])
rect99.6 = data.frame(xmin = pca6$xmin[3], xmax = pca6$xmax[3], 
                      ymin = pca6$ymin[3], ymax = pca6$ymax[3])
rect99.7 = data.frame(xmin = pca7$xmin[3], xmax = pca7$xmax[3], 
                      ymin = pca7$ymin[3], ymax = pca7$ymax[3])
rect99.8 = data.frame(xmin = pca8$xmin[3], xmax = pca8$xmax[3], 
                      ymin = pca8$ymin[3], ymax = pca8$ymax[3])
rect99.9 = data.frame(xmin = pca9$xmin[3], xmax = pca9$xmax[3], 
                      ymin = pca9$ymin[3], ymax = pca9$ymax[3])
rect99.10 = data.frame(xmin = pca10$xmin[3], xmax = pca10$xmax[3], 
                       ymin = pca10$ymin[3], ymax = pca10$ymax[3])

rect999.1 = data.frame(xmin = pca1$xmin[4], xmax = pca1$xmax[4], 
                      ymin = pca1$ymin[4], ymax = pca1$ymax[4])
rect999.2 = data.frame(xmin = pca2$xmin[4], xmax = pca2$xmax[4], 
                      ymin = pca2$ymin[4], ymax = pca2$ymax[4])
rect999.3 = data.frame(xmin = pca3$xmin[4], xmax = pca3$xmax[4], 
                      ymin = pca3$ymin[4], ymax = pca3$ymax[4])
rect999.4 = data.frame(xmin = pca4$xmin[4], xmax = pca4$xmax[4], 
                      ymin = pca4$ymin[4], ymax = pca4$ymax[4])
rect999.5 = data.frame(xmin = pca5$xmin[4], xmax = pca5$xmax[4], 
                      ymin = pca5$ymin[4], ymax = pca5$ymax[4])
rect999.6 = data.frame(xmin = pca6$xmin[4], xmax = pca6$xmax[4], 
                      ymin = pca6$ymin[4], ymax = pca6$ymax[4])
rect999.7 = data.frame(xmin = pca7$xmin[4], xmax = pca7$xmax[4], 
                      ymin = pca7$ymin[4], ymax = pca7$ymax[4])
rect999.8 = data.frame(xmin = pca8$xmin[4], xmax = pca8$xmax[4], 
                      ymin = pca8$ymin[4], ymax = pca8$ymax[4])
rect999.9 = data.frame(xmin = pca9$xmin[4], xmax = pca9$xmax[4], 
                      ymin = pca9$ymin[4], ymax = pca9$ymax[4])
rect999.10 = data.frame(xmin = pca10$xmin[4], xmax = pca10$xmax[4], 
                       ymin = pca10$ymin[4], ymax = pca10$ymax[4])

median.1 = data.frame(xmin = pca1$xmin[5], ymax = pca1$ymax[5])
median.2 = data.frame(xmin = pca2$xmin[5], ymax = pca2$ymax[5])
median.3 = data.frame(xmin = pca3$xmin[5], ymax = pca3$ymax[5])
median.4 = data.frame(xmin = pca4$xmin[5], ymax = pca4$ymax[5])
median.5 = data.frame(xmin = pca5$xmin[5], ymax = pca5$ymax[5])
median.6 = data.frame(xmin = pca6$xmin[5], ymax = pca6$ymax[5])
median.7 = data.frame(xmin = pca7$xmin[5], ymax = pca7$ymax[5])
median.8 = data.frame(xmin = pca8$xmin[5], ymax = pca8$ymax[5])
median.9 = data.frame(xmin = pca9$xmin[5], ymax = pca9$ymax[5])
median.10 = data.frame(xmin = pca10$xmin[5], ymax = pca10$ymax[5])

pdf("999confidence-PCA.pdf", width = 8, height = 6)
p = ggplot() +
  ggtitle("90% of confidence") +
  geom_point(size = .3, alpha = .4) +
  geom_rect(data = rect90.1, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_rect(data = rect90.2, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_rect(data = rect90.3, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_rect(data = rect90.4, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_rect(data = rect90.5, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_rect(data = rect90.6, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_rect(data = rect90.7, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_rect(data = rect90.8, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_rect(data = rect90.9, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_rect(data = rect90.10, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_rect(data = rect90.0, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "blue", alpha = 0.1, inherit.aes = FALSE) +
  xlab("First Principal Component") +
  ylab("Second Principal Component") +
  theme_few() +
  theme(plot.title = element_text(hjust=0.5)) 
print(p)
dev.off()

pdf("Median-confidence-PCA.pdf", width = 8, height = 6)
p = ggplot() +
  ggtitle("Median PCA") +
  geom_point(size = .3, alpha = .4) +
  geom_point(data = median.1, aes(x = xmin, y = ymax), 
            fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_point(data = median.2, aes(x = xmin, y = ymax), 
             fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_point(data = median.3, aes(x = xmin, y = ymax), 
             fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_point(data = median.4, aes(x = xmin, y = ymax), 
             fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_point(data = median.5, aes(x = xmin, y = ymax), 
             fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_point(data = median.6, aes(x = xmin, y = ymax), 
             fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_point(data = median.7, aes(x = xmin, y = ymax), 
             fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_point(data = median.8, aes(x = xmin, y = ymax), 
             fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_point(data = median.9, aes(x = xmin, y = ymax), 
             fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  geom_point(data = median.10, aes(x = xmin, y = ymax), 
             fill = "red", alpha = 0.1, inherit.aes = FALSE) +
  xlab("First Principal Component") +
  ylab("Second Principal Component") +
  theme_few() +
  theme(plot.title = element_text(hjust=0.5)) 
print(p)
dev.off()