
# Carregar pacotes
library(GGally)
library(car)
library(skimr)
library(report)


#Análise exploratória dos dados
# Caminho relativo para o arquivo .csv
dados <- read.csv("./terceiro_estagio.csv", 
                  header = TRUE, 
                  sep = ';', 
                  dec = ',')

dados_clean <- na.omit(dados[, 
                             c("especie", "ilha","comprimento_bico" 
                               ,"profundidade_bico", "comprimento_nadadeira", 
                               "massa_corporal", "sexo", "ano")])

skim(dados)

#Análise de outliers
boxplot(dados_clean$profundidade_bico, 
        main = "Boxplot de Profundidade do Bico",
        ylab = "Profundidade do Bico",
        col = "lightblue", 
        border = "darkblue",
        outline = TRUE)

boxplot(dados_clean$comprimento_bico, 
        main = "Boxplot de Comprimento do Bico",
        ylab = "Comprimento do Bico",
        col = "lightblue", 
        border = "darkblue",
        outline = TRUE)

boxplot(dados_clean$comprimento_nadadeira, 
        main = "Boxplot de Comprimento da Nadadeira",
        ylab = "Comprimento do Nadadeira",
        col = "lightblue", 
        border = "darkblue",
        outline = TRUE)

boxplot(dados_clean$massa_corporal, 
        main = "Boxplot de Massa Corporal",
        ylab = "Massa corporal",
        col = "lightblue", 
        border = "darkblue",
        outline = TRUE)

#Análise de Correlação

plot(profundidade_bico ~ comprimento_nadadeira, data = dados_clean, 
     main = "Profundidade do Bico por Comprimento nadadeira",
     xlab = "comprimento_nadadeira", ylab = "Profundidade Bico")

plot(profundidade_bico ~ comprimento_bico, data = dados_clean, 
     main = "Profundidade do Bico por Comprimento Bico",
     xlab = "comprimento_bico", ylab = "Profundidade Bico")

plot(profundidade_bico ~ massa_corporal, data = dados_clean, 
     main = "Profundidade do Bico por Massa Corporal",
     xlab = "massa_corporal", ylab = "Profundidade Bico")

graf1 <- ggpairs(dados_clean, columns = 3:6, ggplot2::aes(colour=especie))
graf1

#Modelos

modelo0 <- lm(profundidade_bico ~ ., data = dados_clean)
summary(modelo0)

vif(modelo0)

modelo1 <- update(modelo0, ~ . -especie)
summary(modelo1)

vif(modelo1)

modelo2 <- update(modelo1, ~ . -comprimento_bico)
summary(modelo2)

modelo3 <- update(modelo2, ~ . -ano)
summary(modelo3)

#Métodos de seleção de modelos
#AIC
AIC(modelo1)

AIC(modelo2)

AIC(modelo3)

#BIC
BIC(modelo1)

BIC(modelo2)

BIC(modelo3)

#ANOVA
anova(modelo1,modelo2)

anova(modelo2,modelo3)

anova(modelo1,modelo3)

#Modelo selecionado
plot(modelo3)

#Independência dos Erros, teste DurbinWatson
durbinWatsonTest(modelo3)

#Normalidade dos Erros, Shapiro
shapiro.test(residuals(modelo3))

#Analisando o VIF - Variance Inflation Factor
vif(modelo3)

#Interpretação do modelo selecionado
report(modelo3)

#Coeficientes padronizados
lm.beta::lm.beta(modelo3)

#Previsões e Estimação Intervalar
novos_dados <- data.frame(
  especie = factor(c("Pinguim-de-barbicha", 
                     "Pinguim-gentoo"),
                   levels = c("Pinguim-de-barbicha", 
                              "Pinguim-gentoo")),
  ilha = factor(c("Dream" , "Biscoe"), 
                levels = c("Dream", "Biscoe")),
  comprimento_nadadeira = c(193, 229), 
  massa_corporal = c(3800, 5950),       
  sexo = factor(c("fêmea", "macho"), 
                levels = c("fêmea", "macho"))
)


previsoes <- predict(modelo3, novos_dados, interval = "confidence")
print(previsoes)

