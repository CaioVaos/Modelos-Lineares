dados1 <- read.csv("data/sorvete.csv")

attach(dados1)

plot(x = temperatura,
     y = sorvete_litros)
modelo <- lm( sorvete_litros ~ temperatura)
summary(modelo)

abline(lm( sorvete_litros ~ temperatura))

anova(modelo)

plot(modelo)

shapiro.test(modelo$residuals)

library(lmtest)
dwtest(modelo)

library(lmtest)
bptest(modelo)

# ------------------------------------------------------------------------------

dados2 <- read.csv("data/bacteria.csv")

dados2_log <- dados2

dados2_cox <- dados2

attach(dados2)

plot(x = tempo,
     y = bacteria)

# Log ----

dados2_log$bacteria <- log(dados2_log$bacteria)

plot(x = tempo,
     y = dados2_log$bacteria)
modelo_log <- lm( dados2_log$bacteria ~ tempo)
summary(modelo_log)

abline(lm( dados2_log$bacteria ~ tempo))

anova(modelo_log)

plot(modelo_log)

plot(rstudent(modelo_log))

shapiro.test(modelo_log$residuals)

library(lmtest)
dwtest(modelo_log)

library(lmtest)
bptest(modelo_log)


# Boxcox ----

library(MASS)

modelo <- lm(bacteria ~ tempo, data = dados2)

obj <- boxcox(modelo)

lambda_otimo <- obj$x[which.max(obj$y)]
lambda_otimo

dados2_cox$bacteria <- (dados2_cox$bacteria)^lambda_otimo

plot(x = tempo,
     y = dados2_cox$bacteria)
abline(lm( dados2_cox$bacteria ~ tempo))
modelo_cox <- lm( dados2_cox$bacteria ~ tempo)
summary(modelo_cox)

abline(lm( dados2_cox$bacteria ~ tempo))

anova(modelo_cox)

plot(modelo_cox)

plot(rstudent(modelo_cox))

shapiro.test(modelo_cox$residuals)

library(lmtest)
dwtest(modelo_cox)

library(lmtest)
bptest(modelo_cox)




# ------------------------------------------------------------------------------

dados3 <- read.csv("data/gasolina.csv")

attach(dados3)

plot(x = preco_gasolina,
     y = vendas_carros)

dados3_cut <- dados3
dados3_cut <- dados3_cut[-c(1, 2), ]

dados3_cut
attach(dados3_cut)

plot(x = preco_gasolina,
     y = vendas_carros)

modelo <- lm( vendas_carros ~ preco_gasolina)
summary(modelo)

abline(lm( vendas_carros ~ preco_gasolina))

anova(modelo)

plot(modelo)

plot(rstudent(modelo))

shapiro.test(modelo$residuals)

library(lmtest)
dwtest(modelo)

library(lmtest)
bptest(modelo)