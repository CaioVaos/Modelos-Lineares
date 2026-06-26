# Setup ------------------------------------------------------------------------

# Base
# https://basedosdados.org/dataset/6ba4745d-f131-4f8e-9e55-e8416199a6af?table=79de8c5e-9c21-4398-a9fb-bc40e6d6e77f
# https://www.kaggle.com/datasets/raedaddala/top-500-600-movies-of-each-year-from-1960-to-2024/data

# Artigos
# https://towardsdatascience.com/predicting-imdb-movie-ratings-using-supervised-machine-learning-f3b126ab2ddb/
# https://github.com/chandravenky/IMDb-Movie-Ratings-Predictions-using-R
# https://github.com/nateofspades/Linear-Regression-for-IMDB-Movie-Ratings
# https://rstudio-pubs-static.s3.amazonaws.com/199071_adaee47e94104fb4b00a867cbaf5760b.html

## Pacotes ----
library(tidyverse)

## df ----
df_0 <- read.csv("trabalho/data/world_imdb_movies_top_movies_per_year.csv") %>% 
  mutate(
    horas = as.integer(str_extract(duration, "\\d+(?=h)")),
    minutos = as.integer(str_extract(duration, "\\d+(?=m)")),
    horas = ifelse(is.na(horas), 0, horas),
    minutos = ifelse(is.na(minutos), 0, minutos),
    duration_min = horas * 60 + minutos,
  ) %>%
  relocate(duration_min, .after = year) %>% 
  select(-c(horas, minutos, duration))

# Exploração ----

naniar::miss_var_summary(df_0) %>% print(n = Inf)

df <- df_0 %>% 
  select(
    title,
    year, duration_min, rating_mpa, rating_imdb,
    vote, nomination, oscar, genre, language
  )

df <- na.omit(df)

## Genre ----
unique(df$genre) %>% head(200)
generos_principais <- c(
  "Action", "Adventure", "Animation", "Biography", "Comedy",
  "Crime", "Documentary", "Drama", "Family", "Fantasy",
  "History", "Horror", "Music", "Musical", "Mystery",
  "Romance", "SciFi", "Sport", "Thriller", "War", "Western"
)

df <- df |>
  mutate(
    genre_main = str_extract(
      genre,
      paste(generos_principais, collapse = "|")
    )
  )

df |> count(genre_main, sort = TRUE)

df <- df |>
  mutate(
    genre_macro = case_when(
      genre_main %in% c("Action", "Adventure", "War",
                        "Western")              ~ "Ação/Aventura",
      genre_main %in% c("Comedy", "Musical",
                        "Music")                ~ "Comédia/Musical",
      genre_main %in% c("Horror", "Thriller",
                        "Mystery")              ~ "Terror/Suspense",
      genre_main %in% c("Drama", "Biography",
                        "History", "Romance",
                        "Sport")                ~ "Drama",
      genre_main %in% c("SciFi", "Fantasy",
                        "Animation", "Family")  ~ "Ficção/Fantasia",
      genre_main %in% c("Documentary",
                        "Crime")                ~ "Documentário/Crime",
      TRUE                                      ~ "Outros"
    )
  )

df |> count(genre_macro, sort = TRUE)

df <- df %>% 
  select(-c(genre, genre_main)) %>% 
  rename(genre = genre_macro)

## language ----

unique(df$language) %>% head(200)

df <- df |>
  mutate(
    language_main = str_extract(language, "^[^,]+") |> str_trim(),
    language_main = na_if(language_main, "")
  )

df |> count(language_main, sort = TRUE)

df <- df |>
  mutate(
    language_macro = case_when(
      language_main == "English"                        ~ "Inglês",
      language_main %in% c("French", "Spanish",
                           "Italian", "Portuguese",
                           "Romanian")                  ~ "Línguas Latinas",
      language_main %in% c("Mandarin", "Cantonese",
                           "Japanese", "Korean",
                           "Chinese", "Thai",
                           "Hindi", "Bengali")          ~ "Línguas Asiáticas",
      language_main %in% c("German", "Swedish",
                           "Norwegian", "Danish",
                           "Finnish", "Dutch",
                           "Russian", "Polish",
                           "Hungarian", "Czech")        ~ "Línguas Europeias",
      is.na(language_main)                              ~ NA_character_,
      TRUE                                              ~ "Outros"
    )
  )

df |> count(language_macro, sort = TRUE)

df <- na.omit(df)

df <- df %>% 
  select(-c(language, language_main)) %>% 
  rename(language = language_macro)

## rating_mpa ----
unique(df$rating_mpa)

# Agrupamento por nível de restrição de conteúdo (equivalência entre
# sistemas MPA e TV Parental Guidelines), olhando os coeficientes:
# - Categorias "livres"/baixa restrição: G, TV-G, TV-Y, TV-Y7, TV-Y7-FV, Approved, Passed
# - Categorias "moderadas": PG, TV-PG, GP, 13+, TV-13
# - Categorias "adolescente/jovem adulto": PG-13, TV-14, 16+
# - Categorias "adulto/restrito": R, NC-17, X, M, M/PG, TV-MA, MA-17, 18+
# - Sem classificação: Not Rated, Unrated

df <- df %>%
  mutate(
    rating_macro = case_when(
      rating_mpa %in% c("G", "TV-G", "TV-Y", "TV-Y7", "TV-Y7-FV",
                         "Approved", "Passed")        ~ "Livre",
      rating_mpa %in% c("PG", "TV-PG", "GP", "13+", "TV-13") ~ "Moderado",
      rating_mpa %in% c("PG-13", "TV-14", "16+")             ~ "Adolescente",
      rating_mpa %in% c("R", "NC-17", "X", "M", "M/PG",
                         "TV-MA", "MA-17", "18+")       ~ "Adulto/Restrito",
      rating_mpa %in% c("Not Rated", "Unrated")              ~ "Sem Classificação",
      TRUE                                                   ~ "Outros"
    ),
    rating_macro = factor(rating_macro)
  ) %>% 
  select(-rating_mpa)
df <- df %>% 
  rename(rating_mpa = rating_macro)

## Top filmes ----

df %>% arrange(desc(rating_imdb)) %>% pull(title) %>% head(10)

## Salvar ----
df <- df %>% 
  select(-c(title)) %>% 
  filter(duration_min != 0)

# saveRDS(df, "trabalho/data/df_modelo_imdb.rds")
df_modelo <- readRDS("trabalho/data/df_modelo_imdb.rds")

# Dispersões ----

### year ----
plot(df_modelo$year, df_modelo$rating_imdb, xlab = "year", ylab = "rating_imdb", pch = 20)

### duration ----
plot(df_modelo$duration_min,df_modelo$rating_imdb, xlab = "duration_min",  ylab = "rating_imdb", pch = 20)
length(df_modelo %>% 
  filter(duration_min == 0))
df_modelo <- df_modelo %>% 
  filter(duration_min != 0)
plot(df_modelo$duration_min,df_modelo$rating_imdb, xlab = "duration_min",  ylab = "rating_imdb", pch = 20)
plot(log1p(df_modelo$duration_min),df_modelo$rating_imdb, xlab = "duration_min",  ylab = "rating_imdb", pch = 20)

### vote ----
df_0 %>% arrange(desc(vote)) %>% pull(title) %>% head(10)

plot(df_modelo$vote, df_modelo$rating_imdb, xlab = "vote", ylab = "rating_imdb")
plot(log1p(df_modelo$vote), df_modelo$rating_imdb, xlab = "vote", ylab = "rating_imdb")

### nomination ----
df_0 %>% arrange(desc(nomination)) %>% pull(title) %>% head(10)

plot(df_modelo$nomination, df_modelo$rating_imdb, xlab = "nomination", ylab = "rating_imdb")
plot(log1p(df_modelo$nomination), df_modelo$rating_imdb, xlab = "nomination", ylab = "rating_imdb")

### oscar ----
df_0 %>% arrange(desc(oscar)) %>% pull(title) %>% head(10)

plot(df_modelo$oscar, df_modelo$rating_imdb, xlab = "oscar", ylab = "rating_imdb")
plot(log1p(df_modelo$oscar), df_modelo$rating_imdb, xlab = "oscar", ylab = "rating_imdb")
