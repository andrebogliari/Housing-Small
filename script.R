library(ggplot2)
library(readr) 


housingSmall2 <- read_csv("C:/Users/Andre/Desktop/UFSC/Softwares/RStudio/Projetos/Projeto 5 - Regressão/housingSmall2.csv")

hr <- housingSmall2

## Nome Colunas ####

colnames(housingSmall2)[2] = "area"
colnames(housingSmall2)[3] = "garagem"
colnames(housingSmall2)[4] = "quartos"
colnames(housingSmall2)[5] = "preco"

colnames(housingSmall2)

## Plotando gráfico ####

g <- ggplot( data = housingSmall2, aes(x = area, y = preco))

g <- g + geom_point() + labs(title = "Preços de Venda de Imóveis", y = "Preço (x R$ 1000,00", x = "Área (x 100m²)")

g

## Regressão Linear ####

hr_lm <- lm(data = housingSmall2, preco ~ area)

hr_lm

summary(hr_lm) #dados estatísticos da regressão

#Plotando a regressão (há vários jeitos de plotar)

g <- g + stat_smooth(method = lm, se = FALSE, formula = y~x, colour ="Black", linetype = "solid")

g + geom_vline(xintercept = 2.0, size = 1, colour = "green", linetype = "longdash")

## Regressão Polinomial Quadrática ####

g + stat_smooth( method = lm, se = FALSE, formula = y~poly(x,2,raw = TRUE), colour = "RED")

g + stat_smooth( method = lm, se = FALSE, formula = y~poly(x,14,raw = TRUE), colour = "RED")

# A regressão polinomial em alto grau restringe a previsão intensivamente para a amostra obtida, mas não para a população em geral. Não conseguiríamos generalizar.

## Escolhendo o modelo de Erro ####

# Para decidir entre o modelo de regressão veremos o erro de cada uma e comparamos. O menor erro será o melhor. 

# Podemos escolher MAD, MAPE, MSE e etc...


