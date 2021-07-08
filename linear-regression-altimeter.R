if(!require (pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, car, rstatix, lmtest, ggpubr, QuantPsyc, psych, scatterplot3d)

dados <- read.csv('rlmalt.csv', sep = ',', dec = '.')
View(dados)
glimpse(dados)

mod1 <- lm(ALT ~ ï..PRE + TEMP, dados)

par(mfrow=c(2,2))
plot(mod1)

shapiro.test(mod1$residuals)

summary(rstandard(mod1))

durbinWatsonTest(mod1)

bptest(mod1)

pairs.panels(dados)

vif(mod1)

mod2 = lm(ï..PRE ~ ALT + TEMP, dados)

summary(mod1)
summary(mod2)

lm.beta(mod1)
lm.beta(mod2)

confint(mod1)
confint(mod2)

AIC(mod1, mod2)
BIC(mod1, mod2)

graph <- scatterplot3d(dados$ALT ~ dados$ï..PRE + dados$TEMP,
                       pch = 16, angle = 35, color='blue', box = FALSE,
                       xlab = 'Pressão(hPa)', ylab = 'Temperatura(°C)',
                       zlab = 'Altura(m)')
graph$plane3d(mod1, col='black',draw_polygon = TRUE)