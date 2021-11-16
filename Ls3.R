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

summary(lm(count ~ spray, data = InsectSprays))$coef

summary(lm(count ~ 
             I(1 * (spray == 'B')) + I(1 * (spray == 'C')) + 
             I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
             I(1 * (spray == 'F'))
           , data = InsectSprays))$coef

summary(lm(count ~ 
             I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +  
             I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
             I(1 * (spray == 'F')) + I(1 * (spray == 'A')), data = InsectSprays))$coef
summary(lm(count ~ spray - 1, data = InsectSprays))$coef
library(dplyr)
summarise(group_by(InsectSprays, spray), mn = mean(count))

spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef


data(swiss)
head(swiss)
library(dplyr); 
swiss = mutate(swiss, CatholicBin = 1 * (Catholic > 50))
g4 = ggplot(swiss, aes(x = Agriculture, y = Fertility, colour = factor(CatholicBin)))
g4 = g4 + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g4 = g4 + xlab("% in Agriculture") + ylab("Fertility")
g4

summary(lm(Fertility ~ Agriculture, data = swiss))$coef

fit = lm(Fertility ~ Agriculture, data = swiss)
g5_1 = g4
g5_1 = g5_1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g5_1
summary(fit)

fit = lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss)
g5_2 = g4
g5_2 = g5_2 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g5_2 = g5_2 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], slope = coef(fit)[2], size = 2)
g5_2
summary(fit)

fit = lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss)
g5_3 = g4
g5_3 = g5_3 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g5_3 = g5_3 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], 
                      slope = coef(fit)[2] + coef(fit)[4], size = 2)
g5_3

