#operações 
2 + 5 #soma
2 - 3 #subtração 
2 * 3 #multiplicação
40/5 #divisão
3 ** 2 #potenciação (também pode usar o ^)
10 %% 3 #resto da divisão 

#atribuindo valores
#Variável não pode começar com número e nem com caracter especial 
a <- 3*2 #(poderia também utilizar o =) 
4*3 ->b 
class(a)
class(1) 

d <- TRUE
class(d) 

d + a 
FALSE + a
TRUE + TRUE
TRUE + FALSE/FALSE
TRUE/FALSE

v1 <- 4 + 3
V1 <-3 + 90

x <- "teste"
class(x)

#criando vetores
x1 <- c(3,10,78)
#tamanho 
length(x1)

#soma
sum(x1)

#média 
mean(x1)

c(1,6,"teste")

#extraindo valores das posições do vetor 
x1[2]
x1[c(2,3,2,1)]

#combinando um vetor com mais elementos 
x2 <- c(x1,67,90)
x2
x2 < 70
sum(x2<70) 
sum(x2[x2<70])
x2<70
sum(x2 < 70)

x2 > 54
x2 == 10
x2 != 10
!TRUE
TRUE | FALSE #Ou
TRUE & FALSE #e 

?sample

dado <- sample(x = 1:6, size = 1000000, replace = TRUE)
dado
#quantas vezes o número 3 foi sorteado 
sum(dado == 3)

table(dado)
barplot(table(dado))
