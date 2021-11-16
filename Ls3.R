n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
y = 1 + x + x2 + x3 + rnorm(n, sd = .1)
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))
sum(ey * ex) / sum(ex ^ 2)
coef(lm(ey ~ ex - 1))
coef(lm(y ~ x + x2 + x3)) 

library(datasets)
data(swiss)
require(stats)
require(graphics)
pairs(swiss, panel = panel.smooth, main = "Swiss data", col = 3 + (swiss$Catholic > 50))
summary(lm(Fertility ~ . , data = swiss))
summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients

n <- 100
x2 <- 1 : n
x1 <- .01 * x2 + runif(n, -.1, .1)
y = -x1 + x2 + rnorm(n, sd = .01)
dat = data.frame(y =y, x1 = x1, x2 = x2, ey = resid(lm(y~x2)), ex1 = resid(lm(x1~x2)))
library(ggplot2)
g = ggplot(dat, aes(y = y, x=x1, colour = x2))+
  geom_point(colour = "grey50", size = 5)+
  geom_smooth(method = lm, se = F, colour = "black")+
  geom_point(size = 4)

g2 = ggplot(dat, aes(y = ey, x = ex1, colour = x2))  
g2 = g2 + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") + geom_point(size = 4) 
g2

data(InsectSprays)
g3 = ggplot(data = InsectSprays, aes(y = count, x = spray, fill  = spray))
g3 = g3 + geom_violin(colour = "black", size = 2)
g3 = g3 + xlab("Type of spray") + ylab("Insect count")
g3
