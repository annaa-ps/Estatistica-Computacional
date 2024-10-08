library(rvest)
library(dplyr)

url <- "https://www.letras.mus.br/carol-biazin/1697/"

html <- read_html(url)
html

html |> 
  html_elements("h1") |>
  html_text2()

letra <- html |> 
  html_elements("div.lyric") |>
  html_elements("p") |> 
  html_text2() |> 
  paste(collapse = " ")
letra

library(tidytext)
library(ggplot2)

letra <- data.frame(letra)

letra |> 
  unnest_tokens(output = word, input = letra) |>
  count(word, sort = TRUE) |>
  head(n = 10) |>
  ggplot(aes(y = word, x = n))+
  geom_col()
  


