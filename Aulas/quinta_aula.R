library(ggplot2)

#passo 1: dividir em treino e teste 

iris #acessando dados do conjunto iris
set.seed(1711)
iris <- iris[sample(nrow(iris)),] #embaralhando as linhas do conjunto

n <- round(0.8*nrow(iris)) #pegando 80% dos dados
n

treino <- iris[1:n,]
teste <- iris[-(1:n),]

#Gráfico 
ggplot(data = treino, mapping = aes(x = Species))+
  geom_bar()
  theme_minimal()
  
ggplot(data = treino, mapping = aes(x = Petal.Length))+
  geom_histogram(bins = 20, fill = "pink")+
  theme_minimal()

ggplot(data= treino, mapping = aes(y = Petal.Length))+
  geom_boxplot()+
  facet_wrap(~Species)

ggplot(data= treino, mapping = aes(y = Petal.Width))+
  geom_boxplot()+
  facet_wrap(~Species)

ggplot(data= treino, mapping = aes(y = Sepal.Length))+
  geom_boxplot()+
  facet_wrap(~Species)

ggplot(data= treino, mapping = aes(y = Sepal.Width))+
  geom_boxplot()+
  facet_wrap(~Species)

ggplot(data = treino, aes( x = Petal.Length, y = Petal.Width, color = Species))+
  geom_point()

resultados <- c()
for (j in 1:nrow(teste)){
  if(teste$Petal.Length[j] < 2.5){
    resultados[j] <- "setosa"
  }else{
    if(teste$Petal.Width[j] < 1.75){
      resultados[j] <- "versicolor"
    }else{
      resultados[j] <- "virginica"
    }
  }
}
