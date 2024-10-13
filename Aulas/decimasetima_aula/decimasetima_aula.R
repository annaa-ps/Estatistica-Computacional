data(iris)

library(ggplot2)

ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width))+
  geom_point()

cor(iris$Petal.Length, iris$Petal.Width)
cor(iris[,-5])

setosa <- iris[iris$Species == "setosa", ]
cor(setosa$Petal.Length, setosa$Petal.Width)

#========================================================================#

femur <- read.csv(file = "femur.csv")
femur

femur$genero <- as.factor(femur$genero)
femur <- femur[,-1]


homens<- femur[femur$genero == "Male",]
mulheres <- femur[femur$genero == "Female",]

cor(homens$altura, homens$femur)
mean(homens$altura)

cor(mulheres$altura, mulheres$femur)
mean(mulheres$altura)

ggplot(data = homens, aes(x = femur, y = altura))+
  geom_point()

modelo_linear <- lm(data = homens, formula = altura ~ femur)
modelo_linear

summary(homens$femur)
