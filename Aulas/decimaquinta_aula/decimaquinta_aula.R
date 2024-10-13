library(janeaustenr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(stopwords)

livros <- austen_books()
unique(livros$book)

#livros[livros$book == "Emma,]

livros |>
  filter(book == "Emma") |>
  unnest_tokens(output = word,input = text) |>
  count(word, sort = TRUE) |>
  top_n(10)

stopwords("en")

stopwords_en <- data.frame(word = stopwords("en"))

livros |>
  filter(book == "Emma") |>
  unnest_tokens(output = word,input = text) |>
  anti_join(stopwords_en) |>
  count(word, sort = TRUE) |>
  top_n(10)

emma <- livros |>
  filter(book == "Emma")

capitulos <- str_detect(emma$text, "^CHAPTER [IVXLCDM]+")
capitulos <- cumsum(capitulos)
capitulos
emma$capitulos <- capitulos
str(emma)

emma |>
  unnest_tokens(word, text) |>
  anti_join(stopwords_en) |>
  inner_join(get_sentiments("bing"), relationship = "many-to-many") |>
  count(capitulos, sentiment) |>
  spread(sentiment, n, fill = 0) |>
  mutate(total = positive - negative) |>
  ggplot(aes(x = capitulos, y = total))+
  geom_col(fill = "red")+
  theme_minimal()



livros |> 
  group_by(book) |>
  mutate(capitulos = cumsum(str_detect(text, regex("^chapter (\\d|[IVXCDLM])+", ignore_case = TRUE)))) |>
  ungroup() |>
  unnest_tokens(word, text) |>
  anti_join(stopwords_en) |>
  inner_join(get_sentiments("bing"), relationship = "many-to-many") |>
  count(book, capitulos, sentiment) |>
  spread(sentiment, n, fill = 0) |>
  mutate(total = positive - negative) |>
  ggplot(aes(x = capitulos, y = total, fill = book))+
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, scales = "free_x")+
  theme_minimal()
