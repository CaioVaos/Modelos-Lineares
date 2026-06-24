# ============================================================================
# DIAGNÓSTICO SIMPLES: RESÍDUOS STUDENTIZADOS, ALAVANCA E INFLUÊNCIA
# 3 gráficos individuais
# ============================================================================


modelo <- modelo_completo_log

h     <- hatvalues(modelo)        # leverage (hii)
rstud <- rstudent(modelo)         # resíduos studentizados (externamente)
cook  <- cooks.distance(modelo)   # distância de Cook

p <- length(coef(modelo))
n <- nobs(modelo)

limite_leverage <- (2*p) / n
limite_cook     <- (2+p) / n

# ============================================================================
# GRÁFICO 1: RESÍDUOS STUDENTIZADOS
# ============================================================================
plot(rstud,
     xlab = "Observação",
     ylab = "Resíduos studentizados",
     main = "Resíduos Studentizados",
     pch = 1)
abline(h = 0, lty = 2, col = "gray")
abline(h = c(-3, 3), lty = 2, col = "red")

sum(abs(rstud) > 3)
sum(abs(rstud) <= 3)

# ============================================================================
# GRÁFICO 2: ALAVANCA (LEVERAGE)
# ============================================================================
plot(h,
     xlab = "Observação",
     ylab = "Leverage (hii)",
     main = "Pontos de Alavanca",
     pch = 1)
abline(h = limite_leverage, lty = 2, col = "red")

sum(h > limite_leverage)
sum(h <= limite_leverage)

# ============================================================================
# GRÁFICO 3: DISTÂNCIA DE COOK (INFLUÊNCIA)
# ============================================================================
plot(cook, type = "h",
     xlab = "Observação",
     ylab = "Distância de Cook",
     main = "Pontos Influentes (Cook)")
abline(h = limite_cook, lty = 2, col = "red")
abline(h = 0.5, lty = 3, col = "orange")
abline(h = 1,   lty = 3, col = "darkred")

sum(cook > limite_cook)
sum(cook <= limite_cook)

# ============================================================================
# GRÁFICO 4: QQ-PLOT DOS RESÍDUOS STUDENTIZADOS
# ============================================================================

qqnorm(rstud,
       main = "Q-Q Plot dos Resíduos Studentizados",
       pch = 1)

qqline(rstud,
       col = "red",
       lwd = 2)
