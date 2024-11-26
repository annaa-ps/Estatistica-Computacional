#Anna Karolyna Pereira Santos                    Matrícula: 12221BCC046

#Questão 1 
# Carregar bibliotecas necessárias
library(ggplot2)
library(car)
library(lmtest)

# Letra (a): Importar os dados e fazer o gráfico de dispersão
peixe_boi <- read.table(file = "peixe_boi.txt", header = TRUE, sep = ";")
str(peixe_boi)

# Gráfico de dispersão: Mortes vs. Barcos registrados
ggplot(data = peixe_boi, aes(x = barcos, y = mortes)) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Mortes de Peixes-Boi por Colisões com Barcos",
       x = "Número de Barcos Registrados (em milhares)",
       y = "Número de Mortes de Peixes-Boi") +
  theme_minimal()

# O gráfico de dispersão mostra uma tendência de aumento no número de mortes de peixes-boi conforme o número de barcos aumenta.

# Letra (b): Ajustar um modelo de regressão linear simples
modelo <- lm(mortes ~ barcos, data = peixe_boi)
summary(modelo)

# O modelo de regressão linear simples foi ajustado. A equação da reta é:
# Mortes = coeficiente_intercepto + coeficiente_barcos * Barcos

# Adicionar a reta de regressão ao gráfico
ggplot(data = peixe_boi, aes(x = barcos, y = mortes)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Mortes de Peixes-Boi por Colisões com Barcos (com Regressão Linear)",
       x = "Número de Barcos Registrados (em milhares)",
       y = "Número de Mortes de Peixes-Boi") +
  theme_minimal()

# Letra (c): Analisar os resíduos do modelo
residuos <- residuals(modelo)
par(mfrow = c(2, 2))  # Configuração para múltiplos gráficos
plot(modelo)  # Gráficos de diagnóstico do modelo
par(mfrow = c(1, 1))  # Resetar a configuração gráfica

# Os gráficos de diagnóstico mostram que os resíduos parecem ser aproximadamente distribuídos de forma normal 
# e possuem uma variância constante, o que é consistente com os pressupostos do modelo linear.

# Letra (d): Teste de hipótese para verificar se os resíduos seguem uma distribuição normal
teste_shapiro <- shapiro.test(residuos)
teste_shapiro

# Se o p-valor do teste de Shapiro-Wilk for maior que 0,05, não rejeitamos a hipótese nula de que os resíduos seguem 
# uma distribuição normal. Caso contrário, rejeitamos essa hipótese. Este teste complementa a análise gráfica.

# Letra (e): Previsão para 800.000 barcos registrados
barcos_prev <- 800  # Lembre-se que os dados estão em milhares
mortes_prev <- predict(modelo, newdata = data.frame(barcos = barcos_prev))

# O número de mortes de peixes-boi previsto para 800.000 barcos registrados é a saída da previsão.
# Podemos confiar nessa previsão porque o número de 800.000 barcos registrados está dentro do intervalo observado nos dados, 
# e a relação linear é apropriada nesse intervalo.

# Letra (f): Limitação para 200.000 barcos registrados
barcos_lim <- 200
mortes_lim <- predict(modelo, newdata = data.frame(barcos = barcos_lim))

# Não devemos confiar nessa previsão porque o número de 200.000 barcos registrados está fora do intervalo observado nos dados.
# Isso caracteriza extrapolação, e o modelo linear ajustado pode não ser válido para valores tão diferentes daqueles observados.
