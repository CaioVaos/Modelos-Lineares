# Boston ----------------------------------------------------------------------
## Setup ----
library(MASS)
data(Boston)
attach(Boston)

## Gráficos dispersão ----
# Gráficos de dispersão y vs cada variável explicativa
plot(Boston$crim, Boston$medv)
abline(lm(medv ~ crim, data = Boston), lty = 2)

plot(Boston$zn, Boston$medv)
abline(lm(medv ~ zn, data = Boston), lty = 2)

plot(Boston$indus, Boston$medv)
abline(lm(medv ~ indus, data = Boston), lty = 2)

plot(Boston$chas, Boston$medv)
abline(lm(medv ~ chas, data = Boston), lty = 2)

plot(Boston$nox, Boston$medv)
abline(lm(medv ~ nox, data = Boston), lty = 2)

plot(Boston$rm, Boston$medv)
abline(lm(medv ~ rm, data = Boston), lty = 2) #Bom

plot(Boston$age, Boston$medv)
abline(lm(medv ~ age, data = Boston), lty = 2)

plot(Boston$dis, Boston$medv)
abline(lm(medv ~ dis, data = Boston), lty = 2)

plot(Boston$rad, Boston$medv)
abline(lm(medv ~ rad, data = Boston), lty = 2)

plot(Boston$tax, Boston$medv)
abline(lm(medv ~ tax, data = Boston), lty = 2)

plot(Boston$ptratio, Boston$medv)
abline(lm(medv ~ ptratio, data = Boston), lty = 2)

plot(Boston$black, Boston$medv)
abline(lm(medv ~ black, data = Boston), lty = 2)

plot(Boston$lstat, Boston$medv)
abline(lm(medv ~ lstat, data = Boston), lty = 2) #Bom

## Correlação ----
# Correlação entre as variáveis regressoras 
round(cor(Boston[, c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","black","lstat")]), 4)

## Ajuste do modelo ----
# Ajuste do modelo completo
modelo_completo = lm(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, data = Boston)
summary(modelo_completo)
anova(modelo_completo)

# Ajustes
modelo_completo_2 = lm(medv ~ crim + zn + indus + chas + rm + age + dis + tax + ptratio + black + lstat, data = Boston)
summary(modelo_completo)
anova(modelo_completo)

## tabela ANOVA ----
# Construindo a tabela ANOVA "padrão"
medvhat = modelo_completo$fitted.values

SQ_total = sum((medv - mean(medv))^2)
SQ_reg = sum((medvhat - mean(medv))^2)
SQ_res = sum((medv - medvhat)^2)

gl_reg = length(coef(modelo_completo)) - 1
gl_res = modelo_completo$df.residual

QM_reg = SQ_reg / gl_reg
QM_res = SQ_res / gl_res

F_calc = QM_reg / QM_res
p_val = pf(F_calc, gl_reg, gl_res, lower.tail = FALSE)

tabela = data.frame(Fonte = c("Regressão", "Resíduo", "Total"),
                    gl = c(gl_reg, gl_res, gl_reg + gl_res),
                    SQ = c(SQ_reg, SQ_res, SQ_total),
                    QM = c(QM_reg, QM_res, NA),
                    F = c(F_calc, NA, NA),
                    "p-valor" = c(p_val, NA, NA))
tabela

# Cirurgia ----------------------------------------------------------------------
## Setup ----
dados <- read.csv("recuperacao_cirurgia.csv")
attach(dados)

## Gráficos dispersão ----
plot(x1, y)
abline(lm(y ~ x1, data = dados), lty = 2)

plot(x2, y)
abline(lm(y ~ x2, data = dados), lty = 2)

plot(x3, y)
abline(lm(y ~ x3, data = dados), lty = 2)

## Correlação ----
# Correlação entre as variáveis regressoras 
round(cor(dados[, c("x1", "x2", "x3")]), 4)

## Ajuste do modelo ----
# Ajuste do modelo completo
modelo_completo = lm(y ~ x1 + x2 + x3, data = dados)
summary(modelo_completo)
anova(modelo_completo)

# Ajustes
modelo_completo_2 = lm(y ~ x1 + x2, data = dados)
summary(modelo_completo)
anova(modelo_completo)

# modelo_completo <- modelo_completo_2
## tabela ANOVA ----
# Construindo a tabela ANOVA "padrão"
yhat = modelo_completo$fitted.values

SQ_total = sum((y - mean(y))^2)
SQ_reg = sum((yhat - mean(y))^2)
SQ_res = sum((y - yhat)^2)

gl_reg = length(coef(modelo_completo)) - 1
gl_res = modelo_completo$df.residual

QM_reg = SQ_reg / gl_reg
QM_res = SQ_res / gl_res

F_calc = QM_reg / QM_res
p_val = pf(F_calc, gl_reg, gl_res, lower.tail = FALSE)

tabela = data.frame(Fonte = c("Regressão", "Resíduo", "Total"),
                    gl = c(gl_reg, gl_res, gl_reg + gl_res),
                    SQ = c(SQ_reg, SQ_res, SQ_total),
                    QM = c(QM_reg, QM_res, NA),
                    F = c(F_calc, NA, NA),
                    "p-valor" = c(p_val, NA, NA))
tabela

# Agrícola ----------------------------------------------------------------------
## Setup ----
dados <- read.csv("producao_agricola.csv")
attach(dados)

## Gráficos dispersão ----
plot(X1, Y)
abline(lm(Y ~ X1, data = dados), lty = 2)

plot(X2, Y)
abline(lm(Y ~ X2, data = dados), lty = 2)

plot(X3, Y)
abline(lm(Y ~ X3, data = dados), lty = 2)

## Correlação ----
# Correlação entre as variáveis regressoras 
round(cor(dados[, c("X1", "X2", "X3")]), 4)

## Ajuste do modelo ----
# Ajuste do modelo completo
modelo_completo = lm(Y ~ X1 + X2 + X3, data = dados)
summary(modelo_completo)
anova(modelo_completo)

# Ajustes
modelo_completo_2 = lm(Y ~ X1 + X2, data = dados)
summary(modelo_completo)
anova(modelo_completo)

# modelo_completo <- modelo_completo_2
## tabela ANOVA ----
# Construindo a tabela ANOVA "padrão"
Yhat = modelo_completo$fitted.values

SQ_total = sum((Y - mean(Y))^2)
SQ_reg = sum((Yhat - mean(Y))^2)
SQ_res = sum((Y - Yhat)^2)

gl_reg = length(coef(modelo_completo)) - 1
gl_res = modelo_completo$df.residual

QM_reg = SQ_reg / gl_reg
QM_res = SQ_res / gl_res

F_calc = QM_reg / QM_res
p_val = pf(F_calc, gl_reg, gl_res, lower.tail = FALSE)

tabela = data.frame(Fonte = c("Regressão", "Resíduo", "Total"),
                    gl = c(gl_reg, gl_res, gl_reg + gl_res),
                    SQ = c(SQ_reg, SQ_res, SQ_total),
                    QM = c(QM_reg, QM_res, NA),
                    F = c(F_calc, NA, NA),
                    "p-valor" = c(p_val, NA, NA))
tabela

# Navais ----------------------------------------------------------------------
## Setup ----
dados <- read.csv("hospitais_navais.csv", sep = ";")
attach(dados)

## Gráficos dispersão ----
plot(x1, y)
abline(lm(y ~ x1, data = dados), lty = 2)

plot(x2, y)
abline(lm(y ~ x2, data = dados), lty = 2)

plot(x3, y)
abline(lm(y ~ x3, data = dados), lty = 2)

plot(x4, y)
abline(lm(y ~ x4, data = dados), lty = 2)

plot(x5, y)
abline(lm(y ~ x5, data = dados), lty = 2)

## Correlação ----
# Correlação entre as variáveis regressoras 
round(cor(dados[, c("x1", "x2", "x3", "x4", "x5")]), 4)

## Ajuste do modelo ----
# Ajuste do modelo completo
modelo_completo = lm(y ~ x1 + x2 + x3 + x4 + x5, data = dados)
summary(modelo_completo)
anova(modelo_completo)

## tabela ANOVA ----
# Construindo a tabela ANOVA "padrão"
yhat = modelo_completo$fitted.values

SQ_total = sum((y - mean(y))^2)
SQ_reg = sum((yhat - mean(y))^2)
SQ_res = sum((y - yhat)^2)

gl_reg = length(coef(modelo_completo)) - 1
gl_res = modelo_completo$df.residual

QM_reg = SQ_reg / gl_reg
QM_res = SQ_res / gl_res

F_calc = QM_reg / QM_res
p_val = pf(F_calc, gl_reg, gl_res, lower.tail = FALSE)

tabela = data.frame(Fonte = c("Regressão", "Resíduo", "Total"),
                    gl = c(gl_reg, gl_res, gl_reg + gl_res),
                    SQ = c(SQ_reg, SQ_res, SQ_total),
                    QM = c(QM_reg, QM_res, NA),
                    F = c(F_calc, NA, NA),
                    "p-valor" = c(p_val, NA, NA))
tabela

