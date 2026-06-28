# Setup ----
library(tidyverse)
df_modelo <- readRDS("trabalho/data/df_modelo_imdb.rds")

# Modelos ----

## basico ----
modelo_completo_basico <- lm(
  rating_imdb ~ year + duration_min + vote + nomination + oscar +
    rating_mpa + genre + language,
  data = df_modelo
)

summary(modelo_completo_basico)
plot(modelo_completo_basico)
car::vif(modelo_completo_basico)

## log ----
modelo_completo_log <- lm(
  rating_imdb ~ year + log1p(duration_min) + log1p(vote) +
    log1p(nomination) + log1p(oscar) +
    rating_mpa + genre + language,
  data = df_modelo
)

summary(modelo_completo_log)
anova(modelo_completo_log)
plot(modelo_completo_log)
car::vif(modelo_completo_log)
lmtest::bptest(modelo_completo_log)

## Ponderado por oscar ----
pesos_oscar <- log1p(df_modelo$oscar) + 1

modelo_pond_oscar <- lm(
  rating_imdb ~ year + log1p(duration_min) + log1p(vote) +
    log1p(nomination) + log1p(oscar) +
    rating_mpa + genre + language,
  data = df_modelo,
  weights = pesos_oscar
)

summary(modelo_pond_oscar)
anova(modelo_pond_oscar)
plot(modelo_pond_oscar)
car::vif(modelo_pond_oscar)

## sem oscar ----
modelo_sem_oscar_log <- lm(
  rating_imdb ~ year + log1p(duration_min) + log1p(vote) +
    log1p(nomination) +
    rating_mpa + genre + language,
  data = df_modelo
)

summary(modelo_sem_oscar_log)
anova(modelo_sem_oscar_log)
plot(modelo_sem_oscar_log)
car::vif(modelo_sem_oscar_log)
lmtest::bptest(modelo_sem_oscar_log)

## Ponderado por nomination ----
pesos_nomination <- log1p(df_modelo$nomination) + 1

modelo_pond_nomination <- lm(
  rating_imdb ~ year + log1p(duration_min) + log1p(vote) +
    log1p(nomination) + 
    rating_mpa + genre + language,
  data = df_modelo,
  weights = pesos_nomination
)

summary(modelo_pond_nomination)
anova(modelo_pond_nomination)
plot(modelo_pond_nomination)
car::vif(modelo_pond_nomination)

e_pond <- residuals(modelo_pond_nomination) * sqrt(pesos_nomination)
h <- lm.influence(modelo_pond_nomination)$hat
press_resid_pond <- e_pond / (1 - h)
PRESS_pond <- sum(press_resid_pond^2)
PRESS_pond

## Sem categorica ----
modelo_sem_categoricas <- lm(
  rating_imdb ~ year + log1p(duration_min) + log1p(vote) +
    log1p(nomination) + log1p(oscar) + rating_mpa,
  data = df_modelo
)

summary(modelo_sem_categoricas)


# Comparando ----

##  Modelos aninhados ----

anova(modelo_sem_categoricas, modelo_completo_log)

anova(modelo_sem_oscar_log, modelo_completo_log)

## Informação ----
AIC(modelo_completo_basico, modelo_completo_log,
    modelo_sem_oscar_log, modelo_sem_categoricas)

BIC(modelo_completo_basico, modelo_completo_log,
    modelo_sem_oscar_log, modelo_sem_categoricas)

## RR2 ----
purrr::map_df(
  list(
    completo_basico   = modelo_completo_basico,
    completo_log      = modelo_completo_log,
    sem_oscar_log      = modelo_sem_oscar_log,
    sem_categoricas    = modelo_sem_categoricas
  ),
  ~ tibble(
      r2                 = summary(.x)$r.squared,
      r2_adj             = summary(.x)$adj.r.squared,
      sigma              = summary(.x)$sigma,
      variancia_residual = summary(.x)$sigma^2
    ),
  .id = "modelo"
)

# Variância residual ponderado:
purrr::map_df(
  list(
    pond_oscar      = modelo_pond_oscar,
    pond_nomination = modelo_pond_nomination
  ),
  ~ tibble(
      r2                          = summary(.x)$r.squared,
      r2_adj                      = summary(.x)$adj.r.squared,
      sigma_ponderado             = summary(.x)$sigma,
      variancia_residual_ponderada = summary(.x)$sigma^2
    ),
  .id = "modelo"
)

