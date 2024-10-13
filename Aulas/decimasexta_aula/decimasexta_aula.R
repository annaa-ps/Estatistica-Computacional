#numerador <- (1/5000) * 0.999
#denominador <- ((1/5000) * 0.999) + ((4999/5000)*(1 - 0.999))
#numerador/denominador
#20*0.999
#999980*(1-0.999)
#20/1020


library(dplyr)
library(tidytext) 
library(SnowballC) #lematização 
library(quanteda) #analise quantitativa
library(quanteda.textmodels) #modelagem de texto 

stopwords_pt <- data.frame(word = stopwords("pt"))

noticias[1,] |>
  unnest_tokens(word, texto) |>
  anti_join(stopwords_pt ) |>
  count(word, sort = TRUE) |>
  top_n(20)

noticias[1,] |>
  unnest_tokens(word, texto) |>
  anti_join(stopwords_pt ) |>
  mutate(word = wordStem(word, "portuguese")) |>
  count(word, sort = TRUE) |>
  top_n(20)

noticias_tokes <- tokens(noticias$texto, 
                         remove_punct = TRUE,
                         remove_symbols = TRUE,
                         remove_numbers = TRUE,
                         remove_url = TRUE) |>
  tokens_remove(stopwords("portuguese")) |>
  tokens_wordstem(language = "portuguese")

matriz_frequencia <- dfm(noticias_tokes)

n <- round(0.8*nrow(noticias))
n

indices <- sample(1:nrow(noticias), size = n, replace = FALSE)

treino <- matriz_frequencia[indices, ]
teste <- matriz_frequencia[-indices,]

modelo_nb <- textmodel_nb(treino, noticias$categorias[indices])

previsao <- predict(modelo_nb, newdata = teste)

mean(previsao == noticias$categorias[-indices])
