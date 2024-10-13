# Nome: Anna Karolyna Pereira Santos         Matrícula: 12221BCC046
# Nome: Fernanda Ferreira de Melo            Matrícula: 12211BCC043
#-------------------------------------------------------------------------------
#Questão 1
#Questão 1

#(a)

azul <- c(1:13)
vermelha <- c(14:26)
branca <- c(27:39)
marrom <- c(40:52)
baralho <-c(azul, vermelha, branca, marrom)

resultado<- c()

for (j in 1:100000){
  experimento <- c()
  experimento <- sample(x = baralho, size = 3, replace = FALSE)
  
  proporcaoAzul <- 0
  
  for (j in 1:3){
    if(experimento[j] < 14){
      proporcaoAzul <- proporcaoAzul + 1
    }
  }
  resultado <- c(resultado,proporcaoAzul == 2)
}

mean(as.numeric(resultado)) #Significa que a proporção de vezes em que foram sorteadas exatamente duas cartas azuis é de aproximadamente 0.13777

function (n){
  for (j in 1:n){
    experimento <- c()
    experimento <- sample(x = baralho, size = 3, replace = FALSE)
    proporcaoAzul <- 0
    
    for (j in 1:3){
      if(experimento[j] < 14){
        proporcaoAzul <- proporcaoAzul + 1
      }
    }
    resultado <- c(resultado,proporcaoAzul == 2)
  }
  
  mean(as.numeric(resultado))
}



# Questão 2: 
library(rpart)
library(rpart.plot)
library(randomForest)

dados <- read.table(file = "churn.txt", header = TRUE, sep = ";")
str(dados)

dados <- dados[,-1]
dados <- dados[, !names(dados) %in% c("CustomerId", "Surname")]

dados$Exited <- as.factor(dados$Exited)
dados$Geography <- as.factor(dados$Geography)  # Corrigido
dados$Gender <- as.factor(dados$Gender)

set.seed(1731)
n <- round(0.75 * nrow(dados))
n

indices <- sample(1:nrow(dados), size = n, replace = FALSE)
indices

treino <- dados[indices, ]
teste <- dados[-indices, ]

arvore <- rpart(formula = Exited ~ ., data = treino, method = "class")
rpart.plot(arvore, type = 2, extra = 104, fallen.leaves = TRUE, cex = 0.6)

previsao <- predict(arvore, newdata = teste, type = "class")

matriz_confusao <- table(teste$Exited, previsao)

print(matriz_confusao)

VP <- matriz_confusao[2, 2]  
VN <- matriz_confusao[1, 1]  
FP <- matriz_confusao[1, 2] 
FN <- matriz_confusao[2, 1]  

acuracia <- (VP + VN) / (VP + VN + FP + FN)

print(paste("Acurácia: ", round(acuracia * 100, 2), "%"))

#-------------------------------------------------------------------------------
# Criar modelos de árvore de decisão para cada país

paises <- unique(dados$Geography)
for (pais in paises) {
 
  dados_pais <- subset(dados, Geography == pais)
  
  set.seed(1731)
  n <- round(0.75 * nrow(dados_pais))
  indices <- sample(1:nrow(dados_pais), size = n, replace = FALSE)
  
  treino <- dados_pais[indices, ]
  teste <- dados_pais[-indices, ]
  
  
  arvore <- rpart(formula = Exited ~ ., data = treino, method = "class")
  

  rpart.plot(arvore, type = 2, extra = 104, fallen.leaves = TRUE, cex = 0.6)
  
  
  previsao <- predict(arvore, newdata = teste, type = "class")
  

  matriz_confusao <- table(teste$Exited, previsao)
  
  
  print(paste("Matriz de Confusão para", pais, ":"))
  print(matriz_confusao)

  VP <- matriz_confusao[2, 2]  
  VN <- matriz_confusao[1, 1]  
  FP <- matriz_confusao[1, 2] 
  FN <- matriz_confusao[2, 1]  
  
  acuracia <- (VP + VN) / (VP + VN + FP + FN)
  
  print(paste("Acurácia para", pais, ":", round(acuracia * 100, 2), "%"))
}
