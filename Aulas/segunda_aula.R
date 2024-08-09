#VETORES
a <- c(1,2,5)
b <- c(10,12)
d <- c(a,6)

#Operações básicas com os vetores 
3*a #multiplicando o vetor com um algarismo 
3+a #somando o vetor com um algarismo 
a + b #somando dois vetores 
d + b #somando dois vetores

#for no r 
x <- 0 #variável x vale 0 
for(i in 1:20){ #i indo de 1 a 20 
  x <- x+i #x recebendo a soma do numéro armazenado em x + o i conforme o loop compila
}
x #imprimindo x
print(x) #imprimindo x 

aniversarios <- sample(x = 1:365, size = 70, replace = TRUE)
aniversarios
duplicated(aniversarios)

resultados <- c() #vetor para armazenar a quantidade de aniversários repetidos 
for(j in 1:10000){
  aniversarios <- sample(x = 1:365, size = 70, replace = TRUE)
  resultados[j] <- any(duplicated(aniversarios)) #preenchendo o vetor de resultados, verificando se tem resultados duplicados (iguais) com o any perguntando se tem algum TRUE dentro do vetor 
}
mean(resultados) #fazendo e imprimindo a média dos resultados

#Função 
calcula_probabilidade <- function(n){
  resultados <- c() #vetor para armazenar a quantidade de aniversários repetidos 
  for(j in 1:10000){
    aniversarios <- sample(x = 1:365, size = n, replace = TRUE)
    resultados[j] <- any(duplicated(aniversarios)) #preenchendo o vetor de resultados, verificando se tem resultados duplicados (iguais) com o any perguntando se tem algum TRUE dentro do vetor 
  }
  return(mean(resultados)) #fazendo e imprimindo a média dos resultados
}
calcula_probabilidade(n=23)

bilhete <- c(4,5,12,43,21,34)
sorteio <- sample(x = 1:60, size = 6, replace = FALSE) 
sorteio
bilhete %in% sorteio # perguntando se o meu bilhete está em sorteio

semanas <- 0
acertos <- 0
while(acertos < 4){
  sorteio <- sample(x = 1:60, size = 6, replace = FALSE)
  acertos <- sum(bilhete %in% sorteio)
  semanas <- semanas + 1
}
semanas/52
