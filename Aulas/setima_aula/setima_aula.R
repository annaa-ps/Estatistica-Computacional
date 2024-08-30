dados <- read.csv(file = "cancer.csv", header = TRUE)


n <- round(0.8*nrow(dados)) 
n

treino <- dados[1:n,]
teste <- dados[-(1:n),]


treino_padronizado <- scale(treino[,-1])
teste_padronizado <- scale(teste[,-1])

classe_treino <- treino$diagnosis
classe_teste <- teste$diagnosis

modelo_cancer <- knn(train = treino_padronizado, test = teste_padronizado, k = 1, cl = classe_treino)

mean(modelo_cancer == classe_teste)

table(modelo_cancer, classe_teste)
