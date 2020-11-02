
require(ggplot2)
require(ggthemes)

theme_set(theme_clean())

### This code illustrates the effect of contaminating a (pseudo) random sequence with a ramp process

set.seed(1234567890, kind = "Mersenne-Twister")

noise <- 2 * runif(12) - 1 # The sequence is in [-1,1]
ramp <- (0:11 %% 6) / 6
df <- data.frame(x = 1:12, noise = noise, ramp = ramp)

ggplot(df, aes(x = x, y = ramp)) +
  geom_line(col = "gray") +
  geom_line(aes(x = x, y = ramp+noise), col = "red") +
  geom_point(aes(x = x, y = ramp+noise), col = "red") +
  geom_line(aes(x = x, y = .5*ramp+.5*noise), col = "orange") +
  geom_point(aes(x = x, y = .5*ramp+.5*noise), col = "orange") +
  geom_line(aes(x = x, y = noise), col = "black") +
  geom_point(aes(x = x, y = noise), col = "black")
