library(ggplot2)
library(palmerpenguins)
library(class)

data(penguins)
pinguins <- penguins
str(pinguins)
pinguins <- pinguins[,-c(2,7,8)]
pinguins <- na.omit(pinguins)

n<- round(0.8*nrow(pinguins))
n
indices_treino <- sample(1: nrow(pinguins), size = n, replace = FALSE)

treino <- pinguins[indices_treino,]
teste <- pinguins[-indices_treino,]
treino_padronizado <- scale(treino[,-1])
teste_padronizado <- scale(teste[,-1])

classe_treino <- treino$species
classe_teste <-teste$species

modelo1 <- knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k=1)
mean(modelo1 == teste$species)

modelo2 <- knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k=10)
mean(modelo2 == teste$species)

cor(treino[,-1])

taxa_acerto <- c()
for(k in 1:10){
  modelo <- knn(train = treino_padronizado, test = teste_padronizado, cl = classe_treino, k= k)
  taxa_acerto[k] <- mean(modelo == teste$species)
}

df <- data.frame(k = 1:10, taxa_acerto)

ggplot(data = df, aes(x = k, y = taxa_acerto))+
  geom_line()
