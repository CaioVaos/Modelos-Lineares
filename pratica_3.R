df <- read.csv("data/producao_agricola.csv")
attach(df)

# X1 ---------------------------------------------------------------------------

plot(x = X1,
     y = Y)
abline(lm( Y ~ X1))

modelo <- lm( Y ~ X1)
summary(modelo)


anova(modelo)

plot(rstudent(modelo))
plot(modelo)

# X2 ---------------------------------------------------------------------------
plot(x = X2,
     y = Y)
abline(lm(Y ~ X2))

## log ----

df$X2 <- log(df$X2)
df$Y <- log(df$Y)

plot(x = df$X2,
     y = df$Y)
abline(lm( df$Y ~ df$X2))

modelo <- lm( df$Y ~ df$X2)
summary(modelo)

anova(modelo)

plot(rstudent(modelo))
plot(modelo)

shapiro.test(modelo$residuals)

library(lmtest)
dwtest(modelo)

library(lmtest)
bptest(modelo)

# X3 ---------------------------------------------------------------------------

plot(x = X3,
     y = Y)
abline(lm( Y ~ X3))

modelo <- lm( Y ~ X3)
summary(modelo)

anova(modelo)

plot(rstudent(modelo))
plot(modelo)

