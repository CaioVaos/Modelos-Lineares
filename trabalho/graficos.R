# Setup ----
library(tidyverse)
library(qqplotr)

df_modelo <- readRDS("trabalho/data/df_modelo_imdb.rds")

# Plots -----

## Dispersão ----

### Year ----

plot_dispersao_year <- ggplot(df_modelo, aes(x = year, y = rating_imdb)) +
  geom_point(
    alpha = 0.12,
    size = 1.3,
    color = "#F5C518"
  )+
  labs(
    x = "Ano de lançamento",
    y = "Nota IMDb"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#141414",color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    panel.grid.major = element_line(color = "#2A2A2A", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#E8E8F0"),
    axis.title = element_text(color = "#E8E8F0", face = "bold"),
    plot.title = element_text(color = "#F5C518",hjust = 0.5, face = "bold", size = 16),
    legend.position = "none"
  )
plot_dispersao_year

### duration ----

### duration ----

df_duration_facet <- df_modelo %>%
  transmute(
    rating_imdb,
    `Duração (minutos)`     = duration_min,
    `log da Duração (minutos)` = log1p(duration_min)
  ) %>%
  pivot_longer(
    cols = -rating_imdb,
    names_to  = "transformacao",
    values_to = "valor"
  ) %>%
  mutate(transformacao = factor(transformacao, levels = c("Duração (minutos)", "log da Duração (minutos)")))

plot_dispersao_duration <- ggplot(df_duration_facet, aes(x = valor, y = rating_imdb)) +
  geom_point(
    alpha = 0.5,
    size = 1.3,
    color = "#F5C518"
  ) +
  facet_wrap(~ transformacao, scales = "free_x") +
  labs(
    x = NULL,
    y = "Nota IMDb"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#141414",color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    panel.border = element_rect(color = "#D97706", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "#2A2A2A", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.text = element_text(color = "#E8E8F0"),
    axis.title = element_text(color = "#E8E8F0", face = "bold"),
    plot.title = element_text(color = "#F5C518",hjust = 0.5, face = "bold", size = 16),
    legend.position = "none",
    strip.background = element_rect(fill = "#1A1810", color = NA),
    strip.text = element_text(color = "#F5C518", face = "bold", size = 12)
  )
plot_dispersao_duration

### vote ----

df_vote_facet <- df_modelo %>%
  transmute(
    rating_imdb,
    `Número de votos`     = vote,
    `log do Número de votos` = log1p(vote)
  ) %>%
  pivot_longer(
    cols = -rating_imdb,
    names_to  = "transformacao",
    values_to = "valor"
  ) %>%
  mutate(transformacao = factor(transformacao, levels = c("Número de votos", "log do Número de votos")))

plot_dispersao_vote <- ggplot(df_vote_facet, aes(x = valor, y = rating_imdb)) +
  geom_point(
    alpha = 0.5,
    size = 1.3,
    color = "#F5C518"
  ) +
  facet_wrap(~ transformacao, scales = "free_x") +
  labs(
    x = NULL,
    y = "Nota IMDb"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#141414",color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    panel.border = element_rect(color = "#D97706", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "#2A2A2A", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.text = element_text(color = "#E8E8F0"),
    axis.title = element_text(color = "#E8E8F0", face = "bold"),
    plot.title = element_text(color = "#F5C518",hjust = 0.5, face = "bold", size = 16),
    legend.position = "none",
    strip.background = element_rect(fill = "#1A1810", color = NA),
    strip.text = element_text(color = "#F5C518", face = "bold", size = 12)
  )
plot_dispersao_vote

### nomination ----

df_nomination_facet <- df_modelo %>%
  transmute(
    rating_imdb,
    `Número de indicações`     = nomination,
    `log do Número de indicações` = log1p(nomination)
  ) %>%
  pivot_longer(
    cols = -rating_imdb,
    names_to  = "transformacao",
    values_to = "valor"
  ) %>%
  mutate(transformacao = factor(transformacao, levels = c("Número de indicações", "log do Número de indicações")))

plot_dispersao_nomination <- ggplot(df_nomination_facet, aes(x = valor, y = rating_imdb)) +
  geom_point(
    alpha = 0.5,
    size = 1.3,
    color = "#F5C518"
  ) +
  facet_wrap(~ transformacao, scales = "free_x") +
  labs(
    x = NULL,
    y = "Nota IMDb"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#141414",color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    panel.border = element_rect(color = "#D97706", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "#2A2A2A", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.text = element_text(color = "#E8E8F0"),
    axis.title = element_text(color = "#E8E8F0", face = "bold"),
    plot.title = element_text(color = "#F5C518",hjust = 0.5, face = "bold", size = 16),
    legend.position = "none",
    strip.background = element_rect(fill = "#1A1810", color = NA),
    strip.text = element_text(color = "#F5C518", face = "bold", size = 12)
  )
plot_dispersao_nomination

### oscar ----

df_oscar_facet <- df_modelo %>%
  transmute(
    rating_imdb,
    `Número de indicações ao oscar`     = oscar,
    `log do Número de indicações ao oscar` = log1p(oscar)
  ) %>%
  pivot_longer(
    cols = -rating_imdb,
    names_to  = "transformacao",
    values_to = "valor"
  ) %>%
  mutate(transformacao = factor(transformacao, levels = c("Número de indicações ao oscar", "log do Número de indicações ao oscar")))

plot_dispersao_oscar <- ggplot(df_oscar_facet, aes(x = valor, y = rating_imdb)) +
  geom_point(
    alpha = 0.5,
    size = 1.3,
    color = "#F5C518"
  ) +
  facet_wrap(~ transformacao, scales = "free_x") +
  labs(
    x = NULL,
    y = "Nota IMDb"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#141414",color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    panel.border = element_rect(color = "#D97706", fill = NA, linewidth = 0.8),
    panel.grid.major = element_line(color = "#2A2A2A", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.text = element_text(color = "#E8E8F0"),
    axis.title = element_text(color = "#E8E8F0", face = "bold"),
    plot.title = element_text(color = "#F5C518",hjust = 0.5, face = "bold", size = 16),
    legend.position = "none",
    strip.background = element_rect(fill = "#1A1810", color = NA),
    strip.text = element_text(color = "#F5C518", face = "bold", size = 12)
  )
plot_dispersao_oscar

### rating_mpa ----

plot_box_rating_mpa <- df_modelo %>%
  filter(!is.na(rating_mpa)) %>%
  mutate(rating_mpa = as.factor(rating_mpa)) %>%
  ggplot(aes(x = fct_reorder(rating_mpa, rating_imdb, .fun = median), y = rating_imdb)) +
  geom_boxplot(
    fill = "#F5C518", alpha = 0.5, color = "#F5C518",
    outlier.color = "#F5C518", outlier.alpha = 0.4, outlier.size = 1
  ) +
  coord_flip(ylim = c(0, 10)) +
  labs(title = "Classificação etária (MPA)", x = NULL, y = "Nota IMDb") +
  tema_categoricas +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#141414", color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    panel.border = element_rect(color = "#D97706", fill = NA, linewidth = 0.8),
    panel.grid.major.x = element_line(color = "#2A2A2A", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#E8E8F0"),
    axis.title = element_text(color = "#E8E8F0", face = "bold"),
    plot.title = element_text(color = "#F5C518", hjust = 0.5, face = "bold", size = 13),
    legend.position = "none"
  )

plot_box_rating_mpa

### genre ----

plot_box_genre <- df_modelo %>%
  separate_rows(genre, sep = ",\\s*") %>%
  filter(!is.na(genre), genre != "") %>%
  ggplot(aes(x = fct_reorder(genre, rating_imdb, .fun = median), y = rating_imdb)) +
  geom_boxplot(
    fill = "#F5C518", alpha = 0.5, color = "#F5C518",
    outlier.color = "#F5C518", outlier.alpha = 0.3, outlier.size = 0.8
  ) +
  coord_flip(ylim = c(0, 10)) +
  labs(title = "Gênero", x = NULL, y = "Nota IMDb") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#141414", color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    panel.border = element_rect(color = "#D97706", fill = NA, linewidth = 0.8),
    panel.grid.major.x = element_line(color = "#2A2A2A", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#E8E8F0"),
    axis.title = element_text(color = "#E8E8F0", face = "bold"),
    plot.title = element_text(color = "#F5C518", hjust = 0.5, face = "bold", size = 13),
    legend.position = "none"
  )

plot_box_genre

### language ----

plot_box_language <- df_modelo %>%
  separate_rows(language, sep = ",\\s*") %>%
  filter(!is.na(language), language != "") %>%
  ggplot(aes(x = fct_reorder(language, rating_imdb, .fun = median), y = rating_imdb)) +
  geom_boxplot(
    fill = "#F5C518", alpha = 0.5, color = "#F5C518",
    outlier.color = "#F5C518", outlier.alpha = 0.3, outlier.size = 0.8
  ) +
  coord_flip(ylim = c(0, 10)) +
  labs(title = "Idioma", x = NULL, y = "Nota IMDb") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#141414", color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    panel.border = element_rect(color = "#D97706", fill = NA, linewidth = 0.8),
    panel.grid.major.x = element_line(color = "#2A2A2A", linewidth = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#E8E8F0"),
    axis.title = element_text(color = "#E8E8F0", face = "bold"),
    plot.title = element_text(color = "#F5C518", hjust = 0.5, face = "bold", size = 13),
    legend.position = "none"
  )

plot_box_language

## Resíduos ----

modelo <- lm(
  rating_imdb ~ year + log1p(duration_min) + log1p(vote) +
    log1p(nomination) + log1p(oscar) +
    rating_mpa + genre + language,
  data = df_modelo
)

h     <- hatvalues(modelo)
rstud <- rstudent(modelo)
cook  <- cooks.distance(modelo)

p <- length(coef(modelo))
n <- nobs(modelo)

limite_leverage <- (2 * p) / n
limite_cook     <- (2 + p) / n

df_diag <- tibble(
  obs   = seq_len(n),
  rstud = rstud,
  h     = h,
  cook  = cook
)

### Resíduos studentizados ----

plot_residuos_studentizados <- ggplot(df_diag, aes(x = obs, y = rstud)) +
  geom_point(
    aes(color = abs(rstud) > 3, alpha = abs(rstud) > 3, size = abs(rstud) > 3)
  ) +
  scale_color_manual(values = c("FALSE" = "#F5C518", "TRUE" = "#FF6B6B")) +
  scale_alpha_manual(values = c("FALSE" = 0.35, "TRUE" = 0.95)) +
  scale_size_manual(values = c("FALSE" = 1.1, "TRUE" = 2.3)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#9A9080", linewidth = 0.5) +
  geom_hline(yintercept = c(-3, 3), linetype = "dashed", color = "#D97706", linewidth = 0.9) +
  labs(x = "Observação", y = "Resíduo studentizado") +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#141414", color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    panel.grid.major = element_line(color = "#2A2A2A", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#E8E8F0"),
    axis.title = element_text(color = "#E8E8F0", face = "bold"),
    plot.title = element_text(color = "#F5C518", hjust = 0.5, face = "bold", size = 16),
    legend.position = "none"
  )

plot_residuos_studentizados

### Alavanca (leverage) ----

plot_residuos_leverage <- ggplot(df_diag, aes(x = obs, y = h)) +
  geom_point(
    aes(
      color = h > limite_leverage,
      alpha = h > limite_leverage,
      size  = h > limite_leverage
    )
  ) +
  scale_color_manual(values = c("FALSE" = "#F5C518", "TRUE" = "#FF6B6B")) +
  scale_alpha_manual(values = c("FALSE" = 0.35, "TRUE" = 0.95)) +
  scale_size_manual(values = c("FALSE" = 1.1, "TRUE" = 2.3)) +
  geom_hline(yintercept = limite_leverage, linetype = "dashed", color = "#D97706", linewidth = 0.9) +
  labs(
    x = "Observação",
    y = "Leverage (hii)"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#141414",color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    panel.grid.major = element_line(color = "#2A2A2A", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#E8E8F0"),
    axis.title = element_text(color = "#E8E8F0", face = "bold"),
    plot.title = element_text(color = "#F5C518",hjust = 0.5, face = "bold", size = 16),
    legend.position = "none"
  )
plot_residuos_leverage

### Distância de Cook ----

plot_residuos_cook <- ggplot(df_diag, aes(x = obs, y = cook))+
  geom_segment(
    aes(
      xend = obs, yend = 0,
      color = cook > limite_cook,
      alpha = cook > limite_cook,
      linewidth = cook > limite_cook
    )
  ) +
  scale_color_manual(values = c("FALSE" = "#F5C518", "TRUE" = "#FF6B6B")) +
  scale_alpha_manual(values = c("FALSE" = 0.5, "TRUE" = 0.95)) +
  scale_linewidth_manual(values = c("FALSE" = 0.4, "TRUE" = 0.9)) +
  geom_hline(yintercept = limite_cook, linetype = "dashed", color = "#D97706", linewidth = 0.9) +
  labs(
    x = "Observação",
    y = "Distância de Cook"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#141414",color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    panel.grid.major = element_line(color = "#2A2A2A", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#E8E8F0"),
    axis.title = element_text(color = "#E8E8F0", face = "bold"),
    plot.title = element_text(color = "#F5C518",hjust = 0.5, face = "bold", size = 16),
    legend.position = "none"
  )
plot_residuos_cook

### QQ-Plot dos resíduos studentizados ----

rstud <- rstudent(modelo)

plot_residuos_qqplot <- ggplot(
  data.frame(rstud),
  aes(sample = rstud)
) +
  stat_qq_band(
    fill = "#60A5FA",
    alpha = 0.25
  ) +
  stat_qq_line(
    color = "#D97706",
    linewidth = 1
  ) +
  stat_qq_point(
    color = "#F5C518",
    size = 1.1
  ) +
  labs(
    x = "Quantis Teóricos",
    y = "Quantis Amostrais"
  ) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#141414", color = NA),
    panel.background = element_rect(fill = "#141414", color = NA),
    panel.grid.major = element_line(color = "#2A2A2A", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.text = element_text(color = "#E8E8F0"),
    axis.title = element_text(color = "#E8E8F0", face = "bold"),
    text = element_text(size = 14)
  )

plot_residuos_qqplot

### Contagens (para texto/discussão) ----

n_outliers_rstud <- sum(abs(rstud) > 3)
n_leverage_alto  <- sum(h > limite_leverage)
n_cook_alto      <- sum(cook > limite_cook)