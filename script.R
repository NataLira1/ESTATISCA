install.packages("dplyr")

# Carregar pacotes
library(GGally)
library(car)

# Ler os dados
dados <- read.csv("C:/Users/User/Desktop/terceiro_estagio.csv", header = TRUE, sep=';', dec=',')

dados_clean <- na.omit(dados[, c("especie", "ilha","comprimento_bico" ,"profundidade_bico", "comprimento_nadadeira", "massa_corporal", "sexo", "ano")])

graf1 <- ggpairs(dados_clean, columns = 3:6, ggplot2::aes(colour=especie))
graf1

str(dados_clean)

plot(profundidade_bico ~ massa_corporal, data = dados_clean, 
        main = "Profundidade do Bico por Massa Corporal",
        xlab = "Massa Corporal", ylab = "Profundidade Bico")

modelo1 <- lm(profundidade_bico ~ . -especie, data = dados_clean)

modelo2 <- update(modelo1, ~ . -comprimento_bico)

modelo3 <- update(modelo2, ~ . -ano)

AIC(modelo1)

AIC(modelo2)

AIC(modelo3)

summary(modelo3)
