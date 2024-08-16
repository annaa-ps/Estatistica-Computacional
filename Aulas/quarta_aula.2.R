iris #acessando dados do conjunto iris
iris <- iris[sample(nrow(iris)),] #embaralhando as linhas do conjunto
iris

n <- round(0.8*nrow(iris)) #pegando 80% dos dados
n

treinamento <- iris[1:n,]
teste <- iris[-(1:n),]

#Gráfico 
ggplot(data = treinamento, aes(x = Petal.Length, y = Petal.Width, col = Species))+
  geom_point(size = 2, alpha = 0.5)+
  theme_minimal()
