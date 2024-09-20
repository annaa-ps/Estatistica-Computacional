library(dplyr)
library(rvest)
library(stringr)

url <- "https://www.bosshunting.com.au/entertainment/movies/best-movies-imdb/"

html <- read_html(url)
html

html |> 
  html_elements("ol.wp-block-list") |>
  html_elements("li") |>
  html_text2()

