library(rpart)
library(rpart.plot)
library(randomForest)

dados <- read.csv(file = "cancer.csv", header = TRUE)
str(dados)
dados$diagnosis <- as.factor(dados$diagnosis)

n <- round(0.8*nrow(dados)) 
n

set.seed(1731)
indices <- sample(1:nrow(dados), size = n, replace = FALSE)
indices

treino <- dados[indices,]
teste <- dados[-indices,]

arvore <- rpart(formula = diagnosis~ ., data = treino, method = "class")
previsao <- predict(arvore, newdata = teste, type = "class")
previsao
mean(previsao == teste$diagnosis)

floresta <- randomForest(formula = diagnosis~ . , data = treino, ntree = 200)
floresta

#randomForest:: randomForest()

previsao.floresta <- predict(floresta, newdata = teste, type = "class")
previsao
mean(previsao.floresta == teste$diagnosis)

