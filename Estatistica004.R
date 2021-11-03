################################################################################################################
#################################### CORRELAÇÃO E REGRESSÃO ####################################################
################################################################################################################

#pacotes
library(dplyr)
library(ggplot2)

#Lendo a base de dados
dados <- read.csv('bases/dados.csv')

head(dados)

#Rodando uma regressão linear

#Dataset de exemplo
#Y = Gasto das famílias
#X = Renda das Famílias
dataset = data.frame(
  Y = c(3011, 1305, 1879, 2654, 2849, 1068, 2892, 2543, 3074, 849, 2184, 2943, 1357, 2755, 2163, 3099, 1600, 353, 1778, 740, 2129, 3302, 2412, 2683, 2515, 2395, 2292, 1000, 600, 1864, 3027, 1978, 2791, 1982, 900, 1964, 1247, 3067, 700, 1500, 3110, 2644, 1378, 2601, 501, 1292, 2125, 1431, 2260, 1770),
  X = c(9714, 3728, 6062, 8845, 8378, 3338, 8507, 7947, 9915, 1632, 6825, 8918, 4100, 9184, 6180, 9997, 4500, 1069, 5925, 2466, 6083, 9712, 7780, 8383, 7185, 7483, 7640, 2100, 2000, 6012, 8902, 5345, 8210, 5662, 2700, 6546, 2900, 9894, 1500, 5000, 8885, 8813, 3446, 7881, 1164, 3401, 6641, 3329, 6648, 4800)
)

head(dataset)

#vendo o tamanho do dataset
nrow(dataset)

#Estatistica Descritivas
summary(dataset)

#usando applt para calcula desvio padrao nas colunas
apply(dataset, 2, sd)

#Análise gráfica

#empilhando as informações com stack
stack(dataset)

#Boxplot
ggplot(stack(dataset), aes(x = ind, y = values)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() + #invertendo  x no y ou vice-versa
  xlab("Variáveis") + 
  ylab("Reais (R$)") + 
  ggtitle('Box-plot') +
  theme(
    plot.title=element_text(size = 14, hjust = 0.5),
    axis.title.y=element_text(size = 12, vjust = +0.2),
    axis.title.x=element_text(size = 12, vjust = -0.2),
    axis.text.y=element_text(size = 10),
    axis.text.x=element_text(size = 10),
  )

#Gráfico de dispersão
ggplot(data = dataset, aes(x = X, y = Y)) + 
  geom_point(size = 1.5, stroke = 0) + 
  geom_smooth(method = lm) +
  xlab("Renda das Famílias") + 
  ylab("Gasto das Famílias") + 
  ggtitle('Reta de Regressão - Gasto X Renda') +
  theme(
    plot.title=element_text(size = 12, hjust = 0.5),
    axis.title.y=element_text(size = 10, vjust = +0.2),
    axis.title.x=element_text(size = 10, vjust = -0.2),
    axis.text.y=element_text(size = 10),
    axis.text.x=element_text(size = 10),
  )

#Análise de correlação
cor(dataset)

#Modelo de regressão linear simples

#estimando o modelo
resultado_regressao <- lm(formula = Y ~ X, data = dataset)
resultado_regressao

summary(resultado_regressao)

#obtendo previsões dentro da amostra
dataset$Y_previsto <- resultado_regressao$fitted.values
dataset

#Correlação

#covariância
#A covariância, ou variância conjunta, é a medida do grau de interdependência (ou inter-relação) 
#numérica entre duas variáveis. É definida da seguinte maneira:

#Covariância populacional
#Covariância amostral


#Gerando uma amostra aleatória para facilitar o entendimento
amostra <- data.frame(
  Idade = c(39, 29, 21, 49, 29, 34, 32, 32, 24, 53, 28, 28, 46, 58, 41, 43, 31, 55, 52, 54),
  Renda = c(1500, 1000, 3500, 1570, 600, 1200, 2000, 500, 1300, 600, 1500, 3000, 0, 550, 1500, 1600, 746, 1000, 0, 1400),
  Anos.de.Estudo = c(6, 7, 12, 13, 9, 12, 12, 6, 7, 5, 7, 16, 12, 3, 12, 9, 1, 6, 1, 6),
  Altura = c(1.6162, 1.7525, 1.6940, 1.8041, 1.7854, 1.7468, 1.6633, 1.6937, 1.6569, 1.6671, 1.6786, 1.6730, 1.7853, 1.6090, 1.7833, 1.6709, 1.6392, 1.6861, 1.7107, 1.7288)
)

#Obtendo a matriz de covariância
matriz_cov <- cov(amostra)
matriz_cov

#Identificando as variâncias na diagonal principal da matriz
var(amostra$Idade)

# Interpretação da Covariância

#Verificando a existência de uma associação linear negativa
grafico <- amostra[, c('Renda', 'Idade')]

ggplot(data= grafico, aes(x= Renda, y=Idade))+
  geom_point(size = 1.5, stroke= 0)+
  geom_hline(yintercept = mean(amostra$Idade), color = 'black')+
  geom_vline(xintercept = mean(amostra$Renda), color = 'black')

#Verificando a existência de uma associação linear positiva
grafico <- amostra[, c('Renda', 'Anos.de.Estudo')]

ggplot(data= grafico, aes(x= Renda, y=Anos.de.Estudo))+
  geom_point(size = 1.5, stroke= 0)+
  geom_hline(yintercept = mean(amostra$Anos.de.Estudo), color = 'black')+
  geom_vline(xintercept = mean(amostra$Renda), color = 'black')
  
#Verificando a inexistência de uma associação linear entre as variáveis
grafico <- amostra[, c('Idade', 'Altura')]

ggplot(data= grafico, aes(x= Idade, y=Altura))+
  geom_point(size = 1.5, stroke= 0)+
  geom_hline(yintercept = mean(amostra$Altura), color = 'black')+
  geom_vline(xintercept = mean(amostra$Idade), color = 'black')

#Coeficiente de correlação de Pearson

#𝜎𝑥𝑦  = covariância populacional entre x e y

#𝑠𝑥𝑦 = covariância amostral entre x e y

#𝑥 e 𝜎𝑦 = desvios padrão populacionais de x e y, respectivamente

#𝑠𝑥 e 𝑠𝑦 = desvios padrão amostrais de x e y, respectivamente

#obtendo s xy
s_xy <- cov(amostra[c('Altura', 'Renda')])
s_xy

s_xy <- s_xy['Altura', 'Renda']
s_xy

#obtendo Sx e Sy
s_x <- sd(amostra$Altura)
s_y <- sd(amostra$Renda)

#obtendo o coeficiente da correlação R xy
r_xy <- s_xy  / (s_x * s_y)
r_xy

#obtendo a matriz de correlação
cor(amostra[c('Altura', 'Renda')])

cor(dataset[c('Y', 'X')])

#gráfico de dispersao
grafico <- amostra[, c('Renda', 'Altura')]

ggplot(data = grafico, aes(x = Renda, y = Altura)) + 
  geom_point(size = 1.5, stroke = 0) + 
  geom_hline(yintercept = mean(amostra$Altura), color = 'black') + 
  geom_vline(xintercept = mean(amostra$Renda), color = 'black')

ggplot(data = dataset, aes(x = X, y = Y)) + 
  geom_point(size = 1.5, stroke = 0) + 
  geom_hline(yintercept = mean(dataset$Y), color = 'black') + 
  geom_vline(xintercept = mean(dataset$X), color = 'black')

#exercicio
s_xy <- 2178803.59
s_x_2 <- 7328865.85
s_y_2 <- 667839.78
s_x <- sqrt(s_x_2)
s_y <- sqrt(s_y_2)

r_xy <- s_xy / (s_x * s_y)

#Regressão Linear
# Regressão linear simples

#carregando dataser
dataset <- data.frame(
  Y = c(3011, 1305, 1879, 2654, 2849, 1068, 2892, 2543, 3074, 849, 2184, 2943, 1357, 2755, 2163, 3099, 1600, 353, 1778, 740, 2129, 3302, 2412, 2683, 2515, 2395, 2292, 1000, 600, 1864, 3027, 1978, 2791, 1982, 900, 1964, 1247, 3067, 700, 1500, 3110, 2644, 1378, 2601, 501, 1292, 2125, 1431, 2260, 1770),
  X = c(9714, 3728, 6062, 8845, 8378, 3338, 8507, 7947, 9915, 1632, 6825, 8918, 4100, 9184, 6180, 9997, 4500, 1069, 5925, 2466, 6083, 9712, 7780, 8383, 7185, 7483, 7640, 2100, 2000, 6012, 8902, 5345, 8210, 5662, 2700, 6546, 2900, 9894, 1500, 5000, 8885, 8813, 3446, 7881, 1164, 3401, 6641, 3329, 6648, 4800)
)
head(dataset)

#Identificando a relação entre as variáveis
ggplot(data = dataset, aes(x = X, y = Y)) + 
  geom_point(size = 1.5, stroke = 0) + 
  geom_smooth(method = lm) +
  xlab("Renda das Famílias") + 
  ylab("Gasto das Famílias") + 
  ggtitle('Reta de Regressão - Gasto X Renda') +
  theme(
    plot.title=element_text(size = 12, hjust = 0.5),
    axis.title.y=element_text(size = 10, vjust = +0.2),
    axis.title.x=element_text(size = 10, vjust = -0.2),
    axis.text.y=element_text(size = 10),
    axis.text.x=element_text(size = 10),
  )

#Matriz de correlação
cor(dataset)

#O método de mínimos quadrados

#obter n
n <- nrow(dataset)
n

#obter SOMA Y
soma_y <- sum(dataset$Y)
soma_y

#obter SOMA X
soma_x <-  sum(dataset$X)
soma_x
  
#obter soma X2
dataset$X2 <- dataset$X ** 2

soma_X2 <- sum(dataset$X2)
soma_X2

soma_x2 <- sum(apply(dataset, 1, function(x) x['X'] ** 2))
soma_x2

dataset$X2 <- NULL   
dataset

#obter  soma Y2
soma_Y2 <- sum(apply(dataset, 1, function(x) x['Y'] ** 2))
soma_Y2

#obter soma soma XY
soma_XY <- sum(apply(dataset, 1, function(x) x['Y'] * x['X']))
soma_XY

#obtendo beta2 
numerador <- n * soma_XY  - soma_x * soma_y
denominador <- n * soma_X2 - (soma_x) ** 2
  
beta_2 <- numerador / denominador
beta_2

beta_1 <- mean(dataset$Y) - beta_2 * mean(dataset$X)
beta_1

#Obtendo a estimativa dos parametros
resultado_regressao <- lm(formula = Y ~X, data = dataset)
resultado_regressao

#Visualizando os parametros estimados
beta_1
beta_2

coefficients(resultado_regressao)

resumo <- summary(resultado_regressao)
resumo

resumo$coefficients

#exercicio
dataset <- data.frame(
  Y = c(670, 220, 1202, 188, 1869, 248, 477, 1294, 816, 2671, 
        1403, 1586, 3468, 973, 701, 5310, 10950, 2008, 9574, 
        28863, 6466, 4274, 6432, 1326, 1423, 3211, 2140),
  X = c(1.59, 0.56, 2.68, 0.47, 5.2, 0.58, 1.32, 3.88, 2.11, 
        5.53, 2.6, 2.94, 6.62, 1.91, 1.48, 10.64, 22.39, 4.2, 
        21.9, 59.66, 14.22, 9.57, 14.67, 3.28, 3.49, 6.94, 6.25)
)
resultado_regressao <- lm(formula = Y ~X, data = dataset)
resultado_regressao

#Obtendo Previsoes

#previsoes dentro da amostra
dataset['Y_previsto'] <-beta_1 + beta_2 * dataset$X
head(dataset, 10)
  
#utilizando a função do R
dataset['Y_previsto_R'] <- resultado_regressao$fitted.values
head(dataset, 10)

#eliminando variavel
dataset['Y_previsto_R'] <- NULL
head(dataset, 10)

#estimando o 'gasto das familias' fora da amostra
prever <- function(X){
  return(beta_1 + beta_2 * X)
}

prever(7510)

#estimando o 'gasto das familias' fora da amostra com a função R
predict(resultado_regressao, data.frame(X = c(7510, 7500)))

#Resíduos

dataset$u <- dataset$Y - dataset$Y_previsto
head(dataset)

dataset['Resíduos'] <- resultado_regressao$residuals
dataset

dataset$u <- NULL
head(dataset)

#Suposições sobre o termo de erro  𝑢
mean(dataset$Resíduos)

#Plotando os resíduos do modelo
ggplot(data = dataset, aes(x = X, y = Resíduos)) + 
         geom_hline(yintercept = 0, color = 'black') +
         geom_point(size = 1.5, stroke = 0) + 
         xlab("X") + 
         ylab("Resíduos") + 
         ggtitle('Resíduos vs Variável Independente') +
         theme(
           plot.title=element_text(size = 12, hjust = 0.5),
           axis.title.y=element_text(size = 10, vjust = +0.2),
           axis.title.x=element_text(size = 10, vjust = -0.2),
           axis.text.y=element_text(size = 10),
           axis.text.x=element_text(size = 10)
         )

ggplot(data = dataset, aes(x = Y_previsto, y = Resíduos)) + 
  geom_hline(yintercept = 0, color = 'black') +
  geom_point(size = 1.5, stroke = 0) + 
  xlab("Y_Previsto") + 
  ylab("Resíduos") + 
  ggtitle('Resíduos vs Y_Previsto') +
  theme(
    plot.title=element_text(size = 12, hjust = 0.5),
    axis.title.y=element_text(size = 10, vjust = +0.2),
    axis.title.x=element_text(size = 10, vjust = -0.2),
    axis.text.y=element_text(size = 10),
    axis.text.x=element_text(size = 10),
  )

#Hipótese de variância constante       
ggplot(data = dataset, aes(x = Y_previsto, y = Resíduos^2)) + 
  geom_point(size = 1.5, stroke = 0) + 
  xlab("Y_Previsto") + 
  ylab("Resíduos²") + 
  ggtitle('Resíduos² vs Y_Previsto') +
  theme(
    plot.title=element_text(size = 12, hjust = 0.5),
    axis.title.y=element_text(size = 10, vjust = +0.2),
    axis.title.x=element_text(size = 10, vjust = -0.2),
    axis.text.y=element_text(size = 10),
    axis.text.x=element_text(size = 10),
  )

#O coeficiente de determinação  R2

#Soma do quadrados do erros (SQE)
head(dataset)

SQE <- sum(apply(dataset, 1, function(x) x['Resíduos']**2))
SQE

#Soma do quadrados total (SQT)
SQT <- sum(apply(dataset, 1, function(x) (x['Y'] - mean(dataset$Y)) ** 2))
SQT

#Soma do quadrados da regressão (SQR)
SQR <- sum(apply(dataset, 1, function(x) (x['Y_previsto'] - mean(dataset$Y)) ** 2))
SQR

#ANOVa - Análise da variância
anova <- anova(resultado_regressao)
anova

#selecionado pelo indice Soma do quadrados da regressão
anova[1,2]

#selecionando pelo indice o residuo
anova[2, 2]

#selecionando pelo indice Soma do quadrados total
anova[1, 2] + anova[2, 2]

#Relação entre as somas de quadrados
#Coeficiente de determinação (R²)

R2 <- SQR / SQT
R2
#com valor de 0.96 esta proximo de 1 significa que estamos conseguir explicar bastante a variação dos dados

#obtendo o r2 de outra forma
resumo <- summary(resultado_regressao)
resumo$r.squared

#exercico
R2 <- 0.96
SQR <- 1325485

SQE <- (SQR / R2) - SQR
SQE

#Testes aplicados a modelos de regressão

#Output do modelo de regressão estimado
resumo
anova

#Erro quadrático médio - estimativa de  𝜎2 
SQE

n
#Erro quadrático médio
EQM <- SQE / (n- 2)
EQM

#outra forma de se obter
EQM <- anova$'Mean Sq'[2]
EQM

#mais um jeito
EQM <- anova['Residuals', 'Mean Sq']
EQM

#Exercicio obtendo EQM
resultado_regressao <- lm(formula = Y ~ X, data = dataset)
anova <- anova(resultado_regressao)
EQM <- anova$'Mean Sq'[2]
EQM

#Teste de hipótese para nulidade do coeficiente angular
#H0: B2 = 0
#H1 = B2 =! 0

#calculando s
s <- sqrt(EQM)
s

#Calculando  ∑(𝑋𝑖−𝑋¯)
soma_desvio2 <- sum(apply(dataset,1, function(x) (x['X'] - mean(dataset$X)) ** 2))
soma_desvio2

#Calculando s beta 2
s_beta_2 <- s / sqrt(soma_desvio2)
s_beta_2

#Determinando as áreas de aceitação e rejeição de H0

#niveis de confianca  e significancia
confianca <- 0.95
significancia <- 1 - confianca

graus_de_liberdade <- resultado_regressao$df.residual
graus_de_liberdade

#obtendo probabilidade e t alpha 2
probabilidade <-  (0.5 + (confianca / 2))
probabilidade

t_alpha_2 <- qt(probabilidade, graus_de_liberdade )
t_alpha_2

#obtendo Obtendo t = b2 - B2 / sb2
t<-(beta_2 - 0) / s_beta_2
t

coeficientes <- resumo$coefficients
coeficientes

coeficientes['X', 't value']

#Teste Bicaudal 
#Rejeitar  H0  se  t<= -t_alpha_2  ou se t >= t_alpha_2

t <= -t_alpha_2
t >= t_alpha_2

#Criterio do p - valor
#Teste Bicaudal
#Rejeitar h0 se o valor p <= significancia

p_valor <- 2 * (pt(t, graus_de_liberdade, lower.tail = F))
p_valor

p_valor <= significancia
#Rejeitamos  h0  e concluímos que existe uma relação significativa entre as duas variáveis.

#Teste F
#𝐻0:𝛽2=0 
#𝐻0:𝛽2≠0 

#Calculando a estatística de teste (F)
anova
F <- anova['X', 'Mean Sq'] / anova['Residuals', 'Mean Sq']
F

anova['X', 'F value']

#Regras de rejeição de  H0
#Rejeitar se  p_valor <= significancia
#obtendo p valor
p_valor <- anova['X', 'Pr(>F)']
p_valor

p_valor <= significancia

#Projeto

Fonte: https://www.kaggle.com/dongeorge/beer-consumption-sao-paulo
#Descrição:
#A cerveja é uma das bebidas mais democráticas e consumidas no mundo. 
#Não sem razão, é perfeito para quase todas as situações, 
#desde o happy hour até grandes festas de casamento.

#Os dados (amostra) foram coletados em São Paulo - Brasil, em uma área universitária, 
#onde existem algumas festas com grupos de alunos de 18 a 28 anos de idade (média).

#Dados:
#X - Temperatura Média (°C)
#Y - Consumo de Cerveja (litros)

#Importando o dataset
dataset <- read.csv('bases/dados_projeto.csv', sep = ';')

#Visualize o conteúdo do Data Frame
nrow(dataset) 
head(dataset)

#Obtenha e avalie as estatísticas descritivas dos dados
summary(dataset)

#usando applt para calcula desvio padrao nas colunas
apply(dataset, 2, sd)

#Análise gráfica
#Construa um box plot para cada variável do dataset
ggplot(dataset, aes( y = Y)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1')) + 
  coord_flip() + #invertendo  x no y ou vice-versa
  ylab("Consumo de Cerveja (litros)") + 
  ggtitle('Box-plot') +
  theme(
    plot.title=element_text(size = 14, hjust = 0.5),
    axis.title.y=element_text(size = 12, vjust = +0.2),
    axis.title.x=element_text(size = 12, vjust = -0.2),
    axis.text.y=element_text(size = 10),
    axis.text.x=element_text(size = 10),
  )

#Construa um box plot para cada variável do dataset
ggplot(dataset, aes( y = X)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1')) + 
  coord_flip() + #invertendo  x no y ou vice-versa
  ylab("Temperatura Média") + 
  ggtitle('Box-plot') +
  theme(
    plot.title=element_text(size = 14, hjust = 0.5),
    axis.title.y=element_text(size = 12, vjust = +0.2),
    axis.title.x=element_text(size = 12, vjust = -0.2),
    axis.text.y=element_text(size = 10),
    axis.text.x=element_text(size = 10),
  )

#Identifique se existe uma relação linear entre as variáveis  Y E X
ggplot(data = dataset, aes(x = X, y = Y)) + 
  geom_point(size = 1.5, stroke = 0) + 
  geom_smooth(method = lm) +
  xlab("Temperatura Média") + 
  ylab("Consumo de cerveja(Litros)") + 
  ggtitle('Reta de Regressão - Consumo X Temperatura') +
  theme(
    plot.title=element_text(size = 12, hjust = 0.5),
    axis.title.y=element_text(size = 10, vjust = +0.2),
    axis.title.x=element_text(size = 10, vjust = -0.2),
    axis.text.y=element_text(size = 10),
    axis.text.x=element_text(size = 10),
  )

#utilizando a matriz de correlação
cor(dataset)
#podemos perceber que ha uma relação linear positva 

#Estimando o modelo de regressão linear simples
resultado_regressao <- lm(formula =  Y ~ X, data = dataset)
resultado_regressao
  

#Visualize o resultado da regressão
summary(resultado_regressao)

#Obtenha y_previsto
dataset$Y_previsto <- resultado_regressao$fitted.values

#utilizando nosos modelo para fazer previsões

#Qual seria  o consumo de cerveja para um dia com temperatura média de 42°c?
predict(resultado_regressao, data.frame(X = 42))

#obtendo residuos da regressao
dataset['Residuos'] <- resultado_regressao$residuals
head(dataset)        

#gráfico de dispersão dos residuoes da regressao conttra o y previsto
ggplot(data = dataset, aes(x = Y_previsto, y = Residuos)) + 
  geom_point(size = 1.5, stroke = 0) + 
  geom_smooth(method = lm) +
  xlab("Y_previsto") + 
  ylab("Residuos") + 
  ggtitle('Residuos vs Y_previsto') +
  theme(
    plot.title=element_text(size = 12, hjust = 0.5),
    axis.title.y=element_text(size = 10, vjust = +0.2),
    axis.title.x=element_text(size = 10, vjust = -0.2),
    axis.text.y=element_text(size = 10),
    axis.text.x=element_text(size = 10),
  )
  
#Obtenha o R² da regressão pelo método da soma dos quadrados
SQE <- sum(apply(dataset, 1, function(x) x['Residuos'] ** 2))
SQE

SQR <- sum(apply(dataset, 1, function(x) (x['Y_previsto'] - mean(dataset$Y_previsto)) ** 2))
SQR

SQT = SQR+SQE
SQT

R2 = SQR /SQT
R2

summary(resultado_regressao)
#podemos observar que nosso r2 esta muito baixo,mas mostra que conseguimso explicar alguma variação
#temperatura do consumo cerveja

