library(rvest)
library(dplyr)
library(ggplot2)

url <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_alfabetização"

html <- read_html(url)
html

html_elements(html, "h1")
html_text2(html_elements(html, "h1"))

html |> 
  html_elements("h1") |>
  html_text2()

tabelas <- html |> 
  html_elements("table") |>
  html_table()

taxa <- tabelas[[3]]
