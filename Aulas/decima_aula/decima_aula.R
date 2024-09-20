library(rvest)
library(dplyr)
library(ggplot2)

url <- "https://pt.wikipedia.org/wiki/Lista_de_unidades_federativas_do_Brasil_por_alfabetização"

html <- read_html(url)
html

#html_elements(html, "table")
tabelas <- html |>
  html_elements("table") |>
  html_table()

alfabetizacao <- tabelas[[3]]

alfabetizacao <- alfabetizacao[,c(2,3)]
names(alfabetizacao) <- c("estado", "taxa")
names(alfabetizacao)

library(stringr)
#str_replace_all(string = "pedro145", pattern = "\\d", replacement = "")

parte1 <- str_replace_all(string = alfabetizacao$taxa, pattern = ",", replacement = ".")
parte2 <- str_replace_all(string = parte1, pattern = "%", replacement = "")
parte_final <- as.numeric(parte2)
parte_final
parte_final <- parte_final/100

alfabetizacao$taxa <- parte_final

library(geobr)
minas <- read_state(code_state = "MG")
ggplot(data = minas)+
  geom_sf(fill = "darkorange")+
  theme_void()

municipiomg <- read_municipality(code_muni = "MG")
ggplot(data = municipiomg)+
  geom_sf()+
  theme_void()

estados <- read_state()
estados
estados$name_state
order(estados$name_state)
#estados[2,]
estados <- estados[order(estados$name_state),]
estados

alfabetizacao <- alfabetizacao[order(alfabetizacao$estado),]
alfabetizacao

estados$taxa <- alfabetizacao$taxa

ggplot(data = estados, aes(fill = taxa))+
  geom_sf()+
  scale_fill_gradient(high = "#4B0082",
                      low = "#DA70D6")
