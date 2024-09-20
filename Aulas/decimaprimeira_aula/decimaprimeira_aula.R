library(dplyr)
library(rvest)
library(stringr)


url <- "https://www.timeout.com/film/best-horror-films"

html <- read_html(url)
html


#extraindo titulo e ano de lançamento dos filmes 
nomes <- html |> 
  html_elements("h3._h3_cuogz_1") |>
  html_text2()


posicao <- str_extract_all(string = nomes, pattern = "^\\d+")

posicao <- unlist(posicao)
posicao
posicao <- as.numeric(posicao)
posicao

anos <- str_extract_all(string = nomes, pattern = "\\(\\d+\\)$")
anos <- unlist(anos)

anos <- unlist(str_extract_all(anos, "\\d+"))
anos

titulos <- str_remove_all(nomes, "^\\d+\\.\\s+")

titulos <- str_remove_all(titulos, "\\s+\\(\\d+\\)$")

filmes_horror <- data.frame(posicao, titulos, anos)

write.csv(filmes_horror, file = "filmes_horror.csv", row.names = FALSE)
