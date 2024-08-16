install.packages("ggplot2")
library(ggplot2)

titanic <- read.table(file = "titanic.txt", sep = ",", header = TRUE)

titanic <- titanic[, -c(1,9:12)]
str(titanic)

titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
str(titanic)

summary(titanic)

ggplot(data = titanic, aes(x = Survived))+
  geom_bar(fill = "#fa9fb5")+
  theme_minimal()

ggplot(data = titanic, aes(x = Survived, fill = Sex))+
  geom_bar()+
  scale_fill_manual(values =  c("male" = "#756bb1", "female" = "#c51b8a"))
  theme_minimal()
  
ggplot(data = titanic, aes(x = Sex, fill = Survived))+
  geom_bar()+
  labs(title = "Análise de sobrevivência de homens e mulheres por classe", x = "Sexo", y = "Frequência", fill = "Sobreviveu")+
  scale_fill_manual(values =  c("0" = "#f03b20", "1" = "#feb24c"), labels = c("0" = "não", "1" = "sim"))+
  facet_wrap(~Pclass)+#o ~ em R significa: "em função de"
  scale_x_discrete(labels = c("female" = "mulher", "male" = "homem"))
  theme_minimal()
