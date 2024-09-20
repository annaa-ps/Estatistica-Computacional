#Ler os arquivos (os arquivos devem estar salvos na mesma pasta do projeto)
#read.table(file = "papagaio.txt", header = TRUE, sep = ",")

#Questão 1: 
juju <- 18
joel <- 7 
moeda <- c("cara", "coroa")

while(juju != 0 & joel != 0){
  sorteio <- sample(moeda, size = 1)
  if (sorteio == "cara"){
    juju <- juju + 1
    joel <- joel - 1
  }else {
    juju <- juju - 1 
    joel <- joel + 1
  }
}

if(joel == 0){
  print("juju")
}else {
  print("joel")
}

#Fazendo o teste 10000
resultados <- c()
for(j in 1:10000){
  juju <- 18
  joel <- 7 
  while(juju != 0 & joel != 0){
    sorteio <- sample(moeda, size = 1)
    if (sorteio == "cara"){
      juju <- juju + 1
      joel <- joel - 1
    }else {
      juju <- juju - 1 
      joel <- joel + 1
    }
  }
  if(joel == 0){
    resultados <- c(resultados, "juju")
  }else {
    resultados <- c(resultados, "joel")
  }
}

resultados
mean(resultados == "juju")
mean(resultados == "joel")

#==============================================================================
#Questão 2: 

bilhete <- c(1:30)
sorteio <- sample(bilhete, size = 1)
sorteio

while(length(unique(sorteio))<30){
  sorteio <- c(sorteio, sample(bilhete, size = 1))
}
length(sorteio)

#Fazendo o sorteio para 10000
resultados <- c()
bilhete <- c(1:30)
for(i in 1:10000){
  sorteio <- sample(bilhete, size = 1, prob = c(1, rep(10, times = 29)))
  while(length(unique(sorteio)) < 30){
    sorteio <- c(sorteio, sample(bilhete, size = 1, prob = c(1, rep(10, times = 29))))
  }
  resultados[i] <- length(sorteio)
  
}

resultados
mean(resultados)

#==============================================================================
#Questão 3: 
#Ler os arquivos (os arquivos devem estar salvos na mesma pasta do projeto)
chicago <- read.csv(file = "chicago.csv", header = TRUE)
str(chicago)

chicago <- chicago[,-1]
chicago$season <- as.factor(chicago$season)

library(ggplot2)
ggplot(data = chicago, aes(x = time, y = temp, col = season))+
  geom_point()+
  theme_minimal()

inverno <- chicago[chicago$season == "Winter",]


