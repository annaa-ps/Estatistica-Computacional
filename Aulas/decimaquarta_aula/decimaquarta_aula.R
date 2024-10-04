library(tidytext) #manipulação de texto
library(dplyr) #manipulação de dados 
library(ggplot2)
library(tidyr) #organizar um conjunto bagunçado 
library(stopwords)
library(rvest) 

url <- "https://www.bbc.com/portuguese/articles/c3dv8yy3d8jo"

html <- read_html(url)
texto <- html |> 
  html_elements("p.bbc-hhl7in") |>
  html_text2()
  #paste(collapse = " ") - pode ser assim tbm

texto <- paste(texto, collapse = " ")

conjunto <- data.frame(texto)
conjunto

conjunto |>
  unnest_tokens(output = word, input = texto) |>
  count(word, sort = TRUE) |>
  top_n(10) 

stopwords_br <- data.frame(word = stopwords("pt"))

conjunto |>
  unnest_tokens(output = word, input = texto) |>
  anti_join(stopwords_br) |>
  count(word, sort = TRUE) |>
  top_n(10) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(y = word, x = n))+
  geom_col(fill = "red")+
  theme_minimal()

library(janeaustenr)
livro <- prideprejudice

livro<- data.frame(texto = livro)

livro |> 
  unnest_tokens(word, texto) |>
  count(word, sott = TRUE) |>
  top_n(10)

stopwords_en <- data.frame(word = stopwords("en"))

livro |> 
  unnest_tokens(word, texto) |>
  anti_join(stopwords_en) |>
  count(word, sott = TRUE) |>
  top_n(10) |>
  mutate(word = reorder(word, n)) |>
  ggplot(aes(y = word, x = n))+
  geom_col(fill = "pink")+
  theme_minimal() 

sentimentos <- get_sentiments("bing")
library(stringr)
capitulos <- str_detect(livro$texto, "^Chapter \\d+")
capitulos <- cumsum(capitulos)
library(tidyr)

livro |> 
  mutate(capitulo = capitulos) |>
  unnest_tokens(word, texto) |>
  inner_join(sentimentos) |>
  count(capitulo, sentiment) |>
  spread(sentiment, n, fill = 0) |>
  mutate(total = positive - negative) |>
  ggplot(aes(x = capitulo, y = total))+
  geom_col()+
  theme_minimal()
