require(ggplot2)
require(ggthemes)

# Illustration of the confidence region
set.seed(123456, kind="Mersenne-Twister")
N <- 51
a <- 0.47
  
u <- runif(N, min = -3, max = 3)
v <- runif(N, min=-1, max=1)
ru <- sort(u, index.return=TRUE)$ix
rv <- sort(v, index.return=TRUE)$ix

uv <- data.frame(u=u, v=v, ru=ru, rv=rv)


# p-value of point X
X <- data.frame(u=2, v=1)

step1 <- ggplot(data=uv, aes(x=u, y=v)) +
  geom_point(size=ifelse(u==median(u), 6.8, 6),
             col=ifelse(u==median(u), "red", "seashell4")) +
  geom_point(data=X, size=6, shape=13, col="maroon") +
  xlim(-3.5, 3.5) + ylim(-1.5, 1.5)  +
  xlab(expression(italic(u))) +
  ylab(expression(italic(v))) +
  theme_clean(base_size = 30, base_family = "sans")

# Confidence region at level a
step2 <- ggplot(data=uv, aes(x=u, y=v)) +
  geom_hline(yintercept = 0, col="gray") +
  geom_vline(xintercept = 0, col="gray") +
  geom_vline(xintercept = u[ru[round(N*a/2)]], linetype=2, col="red") +
  geom_vline(xintercept = u[ru[round(N*(1-a/2))]], linetype=2, col="red") +
  geom_point(size=ifelse(u==median(u), 6.8, 
                         ifelse(u==u[ru[round(N*a/2)]], 6.8, 
                                ifelse(u==u[ru[round(N*(1-a/2))]], 6.8, 6))),
             col=ifelse(u==median(u), "red", 
                        ifelse(u==u[ru[round(N*a/2)]], "green3", 
                               ifelse(u==u[ru[round(N*(1-a/2))]], "green3", "seashell4")))) + 
  geom_point(data=X, size=6, shape=13, col="maroon") +
  geom_rug(sides = "b", 
           col=ifelse(u==median(u), "red", "gray"),
           size=ifelse(u==median(u), 1.2, 1)) +
  xlim(-3.5, 3.5) + ylim(-1.5, 1.5)  +
  xlab(expression(italic(u))) +
  ylab(expression(italic(v))) +
  theme_clean(base_size = 30, base_family = "sans")

# Confidence region at level a
step3 <- ggplot(data=uv, aes(x=u, y=v)) +
  geom_hline(yintercept = 0, col="gray") +
  geom_hline(yintercept = 1, linetype=2, col="red") +
  geom_hline(yintercept = -1, linetype=2, col="red") +
  geom_vline(xintercept = 0, col="gray") +
  geom_vline(xintercept = u[ru[round(N*a/2)]], linetype=2, col="red") +
  geom_vline(xintercept = u[ru[round(N*(1-a/2))]], linetype=2, col="red") +
  geom_rect(aes(xmin =  u[ru[round(N*a/2)]], xmax = u[ru[round(N*(1-a/2))]], ymin = -1, ymax = 1), 
            fill = "#4ea8de", alpha = 0.01, inherit.aes = FALSE) + 
  geom_point(size=ifelse(u==median(u), 6.8, 
                         ifelse(u==u[ru[round(N*a/2)]], 6.8, 
                                ifelse(u==u[ru[round(N*(1-a/2))]], 6.8, 6))),
             col=ifelse(u==median(u), "red", 
                        ifelse(u==u[ru[round(N*a/2)]], "green3", 
                               ifelse(u==u[ru[round(N*(1-a/2))]], "green3", "seashell4")))) + 
  geom_point(data=X, size=6, shape=13, col="maroon") +
  geom_rug(sides = "b", 
           col=ifelse(u==median(u), "red", "gray"),
           size=ifelse(u==median(u), 1.2, 1)) +
  xlim(-3.5, 3.5) + ylim(-1.5, 1.5)  +
  xlab(expression(italic(u))) +
  ylab(expression(italic(v))) +
  theme_clean(base_size = 30, base_family = "sans")

ggsave(step1, filename = "../../Reports/JOURNAL - Confidence Regions/Figures/PvalueStep1.pdf", width = 8, height = 6)
ggsave(step2, filename = "../../Reports/JOURNAL - Confidence Regions/Figures/PvalueStep2.pdf", width = 8, height = 6)
ggsave(step3, filename = "../../Reports/JOURNAL - Confidence Regions/Figures/PvalueStep3.pdf", width = 8, height = 6)
  
