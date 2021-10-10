### The boundaries for D=3, 4, 5

require(readr)
require(ggplot2)
require(ggthemes)
require(cowplot)

# Read the data

continua_N6 <- read_table2("../../Data/trozos/continuaN6.txt")
continua_N24 <- read_table2("../../Data/trozos/continuaN24.txt")
continua_N120 <- read_table2("../../Data/trozos/continuaN120.txt")

trozos_N6 <- read_table2("../../Data/trozos/trozosN6.txt")
trozos_N24 <- read_table2("../../Data/trozos/trozosN24.txt")
trozos_N120 <- read_table2("../../Data/trozos/trozosN120.txt")

# Names
names(continua_N6) <- names(continua_N24) <- names(continua_N120) <- c("HT", "CJT")
names(trozos_N6) <- names(trozos_N24) <- names(trozos_N120) <- c("HT", "CJT")

# Interpolated functions for the solid areas
H <- seq(from=0, to=1, length.out = 1000)

interpCminN6 <- approxfun(x=continua_N6$HT, y=continua_N6$CJT, n=length(continua_N6$HT)/4)
vinterpCminN6 <- interpCminN6(H)
interpCmaxN6 <- approxfun(x=trozos_N6$HT, y=trozos_N6$CJT, n=length(trozos_N6$HT)/4)
vinterpCmaxN6 <- interpCmaxN6(H)

interpCminN24 <- approxfun(x=continua_N24$HT, y=continua_N24$CJT, n=length(continua_N24$HT)/4)
vinterpCminN24 <- interpCminN24(H)
interpCmaxN24 <- approxfun(x=trozos_N24$HT, y=trozos_N24$CJT, n=length(trozos_N24$HT)/4)
vinterpCmaxN24 <- interpCmaxN24(H)

interpCminN120 <- approxfun(x=continua_N120$HT, y=continua_N120$CJT, n=length(continua_N120$HT)/4)
vinterpCminN120 <- interpCminN120(H)
interpCmaxN120 <- approxfun(x=trozos_N120$HT, y=trozos_N120$CJT, n=length(continua_N120$HT)/4)
vinterpCmaxN120 <- interpCmaxN120(H)

Interpolated <- data.frame(H, vinterpCminN6, vinterpCmaxN6,
                           vinterpCminN24, vinterpCmaxN24,
                           vinterpCminN120, vinterpCmaxN120)


# Plots


base.plot.boundaries <- ggplot(trozos_N6) +
  # Solid areas
  geom_ribbon(data=Interpolated, 
              aes(x=H, ymin=vinterpCminN120, ymax=vinterpCmaxN120), 
              fill="lightskyblue") +
  geom_ribbon(data=Interpolated, 
              aes(x=H, ymin=vinterpCminN24, ymax=vinterpCmaxN24), 
              fill="palegreen3") +
  geom_ribbon(data=Interpolated, 
              aes(x=H, ymin=vinterpCminN6, ymax=vinterpCmaxN6), 
              fill="lightsalmon2") +
  geom_line(aes(x=HT, y=CJT, col="red"), size=2) +
  geom_line(data=continua_N6, aes(x=HT, y=CJT, col="red"), size=2) +
  geom_line(data=trozos_N24, aes(x=HT, y=CJT, col="green"), size=2) +
  geom_line(data=continua_N24, aes(x=HT, y=CJT, col="green"), size=2) +
  geom_line(data=trozos_N120, aes(x=HT, y=CJT, col="blue"), size=2) +
  geom_line(data=continua_N120, aes(x=HT, y=CJT, col="blue"), size=2) +
  geom_rect(aes(xmin=0.5, xmax=.72, ymin=.4, ymax=.425),
            fill = "transparent", color = "black") +
  scale_colour_manual(values = c('red' = 'red', "green"= "green", 'blue' = 'blue'), 
                      name = '', 
                      labels = expression(italic(D)==5,italic(D)==4,italic(D)==3)) +
  labs(x=expression("Normalized Entropy"~italic(H)),
       y=expression("Statistical Complexity"~italic(C))) +
  theme_pander(base_size = 25, base_family = "sans") +
  theme(legend.position="top",
        legend.title = element_blank())

subplot <-
  base.plot.boundaries + xlim(.5, .72) + ylim(.4, .425) + 
  ggtitle("") +
  theme(legend.position = "none",
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        panel.background = element_rect(fill="white")
        )

plot.boundaries.with.detail <-
  ggdraw() +
  draw_plot(base.plot.boundaries) +
  draw_plot(subplot, x=.065 , y=.7, width=.33, height=.2) 
  
ggsave(file="../../Reports/JOURNAL - Confidence Regions/Figures/BoundariesPlot.pdf", width = 14, height = 10)
  