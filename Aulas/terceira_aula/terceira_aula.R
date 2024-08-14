
#função read.table que ler dados de uma tabela
dados <- read.table(file = "titanic.txt", header = TRUE, sep = ",")

#tirando os dados da primeira coluna
dados <- dados[,-1]

#tirando os dados da coluna 1 e coluna 12 sem linha identificada, ou seja, toda a coluna 
dados <- dados[,-c(1,12)]

#seleciona os dados de determinada coluna
dados$Survived <- as.factor(dados$Survived)
dados$Pclass <- as.factor(dados$Pclass)
dados$Sex <- as.factor(dados$Sex)

#função que da resumo dos dados 
summary(dados)

#pedir/imprimir a estrutura do conjundo de dados
str(dados)

#acessando dados do primeiro e decimo passageiro  
dados[c(6,10),]

#separando dados dos homens 
homens <- dados[dados$Sex == "male",]
summary(homens)

#colocando em uma tabela e gráfico a separação de quantos homens sobreviveram ou não
table(homens$Survived)
barplot(table(homens$Survived))

#separando dados dos homens 
mulheres <- dados[dados$Sex == "female",]
summary(mulheres)

#colocando em tabela e gráfico a separação de quantas mulheres sobreviveram ou não
table(mulheres$Survived)
barplot(table(mulheres$Survived))

#Quantos homens de determinada classe morreram ou não
terceiraclass <- homens[homens$Pclass == "3"& homens$Survived == "1",]
summary(terceiraclass)        


#desconbrindo quantas pessoas sobreviveram (somando a quantidade de 1)
sum(dados[,2])
sum(dados[,2])/length(dados[,2])
