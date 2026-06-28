library(tidyverse)
library(ggrepel)

df_modelo <- readRDS("trabalho/data/df_modelo_imdb.rds")

p <- length(coef(modelo))
n <- nobs(modelo)

limite_cook <- (2 + p) / n

# cria df_diag com as métricas de diagnóstico do modelo
df_diag <- tibble(
  obs_id = 1:n,
  h      = hatvalues(modelo),
  rstud  = rstudent(modelo),
  cook   = cooks.distance(modelo)
)

h_seq <- seq(min(df_diag$h[df_diag$h > 0]), max(df_diag$h) * 1.02, length.out = 200)

curva_cook <- function(cook_level, h_seq, p) {
  sqrt(cook_level * p * (1 - h_seq) / h_seq)
}

df_curvas <- tibble(h = h_seq) %>%
  mutate(
    cook_lim_pos = curva_cook(limite_cook, h, p),
    cook_lim_neg = -curva_cook(limite_cook, h, p)
  ) %>%
  filter(cook_lim_pos < 10)

ggplot(df_diag, aes(x = h, y = rstud)) +
  geom_point(
    aes(
      color = cook > limite_cook,
      alpha = cook > limite_cook,
      size  = cook > limite_cook
    )
  ) +
  scale_color_manual(values = c("FALSE" = "#F5C518", "TRUE" = "#F5C518")) +
  scale_alpha_manual(values = c("FALSE" = 0.35, "TRUE" = 0.35)) +
  scale_size_manual(values = c("FALSE" = 1.1, "TRUE" = 1.1)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "#9A9080", linewidth = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "#9A9080", linewidth = 0.5) +
  geom_line(data = df_curvas, aes(x = h, y = cook_lim_pos), color = "#D97706", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
  geom_line(data = df_curvas, aes(x = h, y = cook_lim_neg), color = "#D97706", linetype = "dashed", linewidth = 0.8, inherit.aes = FALSE) +
  geom_text_repel(
    data = df_diag %>% filter(cook > limite_cook),
    aes(label = obs_id),
    color = "#E8E8F0",
    size = 3,
    max.overlaps = 20,
    segment.color = "#5A5A5A",
    segment.size = 0.3
  ) +
  coord_cartesian(ylim = range(df_diag$rstud) * 1.1) +
  labs(
    x = "Leverage (hii)",
    y = "Resíduo studentizado"
  ) +
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


# Identificação:
# 1. Influente + alavanca (>0.02) + resíduo positivo
rotulos1 <- df_diag %>%
  filter(
    cook > limite_cook,
    h > 0.02,
    rstud > 0
  ) %>%
  pull(obs_id)
rotulos1
df_modelo[rotulos1, ]

# 2. Influente + alavanca (>0.02) + resíduo negativo
rotulos2 <- df_diag %>%
  filter(
    cook > limite_cook,
    h > 0.02,
    rstud < 0
  ) %>%
  pull(obs_id)
rotulos2
df_modelo[rotulos2, ]

# 3. Influente + resíduo negativo + alavanca <0.01
rotulos3 <- df_diag %>%
  filter(
    cook > limite_cook,
    h < 0.01,
    rstud < 0
  ) %>%
  pull(obs_id)
rotulos3
df_modelo[rotulos3, ]
