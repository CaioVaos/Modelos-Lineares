dfb <- dfbetas(modelo)
n <- nobs(modelo)
limite_dfbeta <- 2/sqrt(n)

dff <- dffits(modelo)
p <- length(coef(modelo))
limite_dffit <- 2*sqrt(p/n)
obs_influentes <- which(abs(dff) > limite_dffit)

influence.measures(modelo)






