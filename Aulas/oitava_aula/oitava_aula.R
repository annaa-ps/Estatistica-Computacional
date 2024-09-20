library(rpart)
library(rpart.plot)

dados <- read.csv(file = "cancer.csv", header = TRUE)


n <- round(0.8*nrow(dados)) 
n

indices_treino <- sample(1:nrow(dados), size = n, replace = FALSE)

treino <- dados[1:n,]
teste <- dados[-(1:n),]

modelo.arvore <- rpart(formula = diagnosis~ ., data = treino, method = "class")
rpart.plot(modelo.arvore, extra = 101)

previsao <- predict(modelo.arvore, newdata = teste, type = "class")
previsao

mean(previsao == teste$diagnosis)
