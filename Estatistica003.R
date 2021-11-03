##########################################################################################################
###################################### TESTES DE HIPÓTESES ###############################################
##########################################################################################################

#Teste de normalidade
  
#shapiro.test

#definindo a signifiancia dos teste(a)

significancia <- 0.05

#testando a variavel renda
ggplot(dados, aes(x =Renda))+
  geom_histogram() +
  ylab("Frequenica")+
  xlab("Renda")+
  ggtitle("Histograma de Renda")+
  theme(plot.title=element_text(hjust = 0.5))

#Critério P VALOR

#rejeitar h0,  se o valor p < a

#definindo para que os valores nunca mudem
set.seed(2811)

#pegando uma amostra de 5000 
amostra <- sample_n(dados, 5000)

#atribuindo o resultado do teste shapiro 
resultado <- shapiro.test(amostra$Renda)
resultado

#aribuindo o valor p
p_valor <- resultado$p.value
   
resultado$statistic

#testando se h0 de q a, amostra e proveniente de uma distribuição normal
p_valor <= significancia
# nesse caso não é proveniente de uma distribuição normal, entao é uma HA

#Fazendo para altura

#testando a variavel altura
ggplot(dados, aes(x =Altura))+
  geom_histogram() +
  ylab("Frequenica")+
  xlab("Altura")+
  ggtitle("Histograma de Altura")+
  theme(plot.title=element_text(hjust = 0.5))

#atribuindo o resultado do teste shapiro 
resultado <- shapiro.test(amostra$Altura)
resultado

#aribuindo o valor p
p_valor <- resultado$p.value

#testando se h0 de q a, amostra e proveniente de uma distribuição normal
p_valor <= significancia
# nesse caso é proveniente de uma distribuição normal, entao é uma HA

#Teste parametrico Bicaudal

amostra <- c(509, 505, 495, 510, 496, 509, 497, 502, 503, 505, 
             501, 505, 510, 505, 504, 497, 506, 506, 508, 505, 
             497, 504, 500, 498, 506, 496, 508, 497, 503, 501, 
             503, 506, 499, 498, 509, 507, 503, 499, 509, 495, 
             502, 505, 504, 509, 508, 501, 505, 497, 508, 507)

#criando dataframe
amostra <- data.frame(Amostra = amostra)
head(amostra)

#média
media_amostra <- mean(amostra$Amostra)

#desvio padrao
sd_amostra <- sd(amostra$Amostra)

media <- 500
significancia <- 0.05
confianca <- 1 - significancia
n <- 50

#Formulando hipoteses do teste
#h0 media = 500
#ha media =! 500

#fixação da significancia do teste(a)
probabilidade <- (0.5 + (confianca / 2))

#obtendo z alfa 2
z_alpha_2 <- qnorm(probabilidade)

#calculo da estatistica teste  e verificacao  desse valor com as areas  de aceitação  e rejeição  do teste
z <- (media_amostra - media) / (sd_amostra / sqrt(n)) 

#Critério do valor critico
#rejeitar h0 se z <= -z alfa 2 ou se z >= z alfa 2
z <= z_alpha_2

z >= z_alpha_2

if(z <= z_alpha_2 || z >= z_alpha_2){
  'Rejeitar H0'
}else{
  'Aceitar H0'
}  

#como a media é =! de 500, ficamos com a ha, nesse caso deve ser tomado providencias para ajustar
#maquinario que preenche as embalagens


#Criterio p valor
#h0 se o p valor p <= alfa

p_valor <- 2 * (1 - pnorm(z))
p_valor

p_valor <=  significancia
#rejeitar a h0

library(DescTools)

#Ztest, mostra tambem o pvalor direto
ZTest(amostra$Amostra, mu = media, sd_pop = sd_amostra)

#teste de normalidade
resultado <- ZTest(amostra$Amostra, mu = media, sd_pop = sd_amostra)

#comparando valor de z
resultado$statistic 

z

#verificando p valor
p_valor <- resultado$p.value  

p_valor <= significancia
#nesse caso rejeitamos a hipotese

#Teste unicaudal

#Construindo tabela T de Student
q <- seq(0.05, 0.005, by = -0.005)
df <- seq(1, 30, by = 1)
probabilidade <- c()
for(i in df){
  for(j in q){
    probabilidade <- c(probabilidade, qt(j, i, lower.tail = F))
  }
}
tabela_t_student <- matrix(probabilidade, ncol=10, byrow=TRUE)
colnames(tabela_t_student) <- format(q)
rownames(tabela_t_student) <- format(df)
view(tabela_t_student)

#criando uma nova amostra
amostra <- c(37.27, 36.42, 34.84, 34.60, 37.49, 
             36.53, 35.49, 36.90, 34.52, 37.30, 
             34.99, 36.55, 36.29, 36.06, 37.42, 
             34.47, 36.70, 35.86, 36.80, 36.92, 
             37.04, 36.39, 37.32, 36.64, 35.45)

#Criando data frame
amostra <- data.frame(Amostra = amostra)
amostra

#media da amostra
media_amostra <- mean(amostra$Amostra)

#desvio padrao da amostra
sd_amostra <- sd(amostra$Amostra)

media <- 37
significancia <- 0.05
confianca <- 1 - significancia
n <- 25
graus_de_liberade <- n - 1

#formulação das hipóteses h0 e h1

#h0 media <= 37
#h1 media > 37

#fixação  da significancia do teste alfa
#Tdist

tabela_t_student[19:25, ]
#para nivel de confianaca de 5% e 24graus de liberdade, temos valor de 1.71

#obtendo t alpha direto
t_alpha <- qt(confianca, graus_de_liberade)
t_alpha

#obtendo t
t <- (media_amostra - media) / (sd_amostra/sqrt(n))
t
#como caiu na area de aceitação, nao posso rejeitar h0

#validando 
#rejeitar h0 se  t >= t_alpha
t >= t_alpha

#Critério P valor
#Teste unicaudal superior
#Rejeitar h0 se p valor p <= alpha

t

p_valor <- pt(t, graus_de_liberade, lower.tail = F) 
p_valor

p_valor <= significancia

#Validando direto pelos teste
#T.test
resultado <- t.test(amostra$Amostra, alternative = 'greater', mu= media)
resultado

resultado$statistic

p_valor <- resultado$p.value
p_valor

resultado$p.value <=  significancia
#com nivel de significancia de 95$, nao podemos rejeitar h0, ou seja, a alegação do fabricante é verdadeira.

#exercico
media <- 75
n <- 23
graus_de_liberdade <- n - 1
desvio_padrao <- 12
significancia <- 0.05

# 1) Média amostral = 82
t <- (82 - media) / (desvio_padrao / sqrt(n))

p_valor <- pt(t, graus_de_liberdade, lower.tail = T)
p_valor

if (p_valor <= significancia) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

# 2) Média amostral = 70
t <- (70 - media) / (desvio_padrao / sqrt(n))

p_valor <- pt(t, graus_de_liberdade, lower.tail = T)
p_valor

if (p_valor <= significancia) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}



vendas <- c(32.45, 26.8, 27.81, 30.22, 30.88, 24.9, 31.94, 16.02, 
            24.39, 26.01, 21.83, 25.35, 22.46, 38.2, 29.86, 22.79, 28.83, 27.34, 32.22, 33.26)
dataset <- data.frame(Amostra = vendas)

resultado <- t.test(dataset$Amostra, alternative = "two.sided", mu = media)
t <- resultado$statistic
t

p_valor <- resultado$p.value
p_valor


if (p_valor <= significancia) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

#Testes para duas amostras
homens <- matrix(c(300, 1000, 4000, 5000, 300, 350, 1200, 1200, 1100, 1800, 700, 5000, 250, 
                   1560, 400, 5500, 3000, 0, 1100, 1500, 500, 500, 788, 500, 3000, 788, 2400,
                   788, 3300, 1350, 2500, 2000, 1300, 2430, 9000, 10120, 1380, 2000, 1400, 1000, 
                   500, 1500, 2200, 2100, 4000, 1000, 1200, 2900, 1800, 2000, 788, 1576, 800,
                   2400, 788, 788, 788, 0, 12000, 7880, 850, 1000, 1000, 1600, 3800, 788, 1200, 
                   300, 350, 700, 6000, 1500, 5000, 2000, 1200, 800, 250, 800, 1600, 400, 3000, 
                   2304, 800, 1400, 450, 788, 2200, 6000, 1200, 4000, 6000, 1100, 1200, 1300,
                   3000, 3000, 1500, 1280, 788, 1400, 788, 4000, 0, 1000, 2500, 1300, 3000, 
                   500, 1600, 2000, 1280, 150, 0, 0, 7000, 1061, 700, 788, 2000, 788, 10500, 
                   788, 1600, 1050, 1100, 3000, 800, 1400, 1200, 2500, 1000, 1500, 1500, 900,
                   2000, 300, 3000, 788, 1400, 2000, 2000, 3000, 2600, 15000, 1500, 950, 1200, 
                   1500, 7500, 1400, 350, 750, 1200, 788, 0, 2500, 1700, 788, 700, 1600, 1200, 
                   320, 6000, 2000, 2000, 3000, 900, 2100, 0, 788, 1800, 1600, 4000, 1300, 1300,
                   1500, 8900, 1400, 788, 600, 1000, 950, 2000, 4000, 2300, 2000, 800, 2500, 1500, 
                   1400, 800, 6000, 788, 900, 200, 1300, 788, 2600, 1500, 8000, 900, 2000, 2000, 
                   350, 3000, 9580, 0, 400, 800, 1700, 1800, 1200, 18000, 9000, 3000, 1200, 700, 
                   1200, 400, 987, 6000, 2000, 0, 0, 480, 500, 800, 1680, 10000, 1200, 1700, 788, 
                   2200, 10000, 10000, 100, 1200, 1600, 2500, 1300, 1200, 1300, 2200, 200, 60, 
                   1100, 1200, 6000, 4500, 100, 788, 2900, 2500, 900, 788, 2500, 4000, 788, 1400, 
                   1000, 300, 788, 1000, 4000, 2200, 788, 1000, 1000, 600, 1600, 6000, 2500, 2500, 
                   1000, 3000, 2200, 4500, 1500, 4300, 1500, 8500, 3200, 1200, 1200, 1500, 4000, 
                   2000, 1350, 1500, 890, 5000, 2400, 13000, 800, 1500, 1500, 2000, 580, 500, 920,
                   1200, 2000, 788, 500, 20000, 1350, 1576, 500, 8000, 1800, 600, 1000, 3000, 4000,
                   1500, 788, 600, 2300, 1500, 500, 500, 800, 900, 8000, 1600, 3000, 788, 1500, 5200,
                   900, 3565, 650, 1700, 1600, 788, 1200, 788, 788, 100, 900, 1700, 10000, 1600, 200,
                   788, 1400, 1500, 400, 4200, 1400, 4000, 5516, 3500, 700, 1400, 1200, 0, 7000, 2000,
                   5000, 2000, 750, 3000, 2000, 1500, 200, 3000, 1700, 1500, 7000, 1500, 788, 2400, 2500, 
                   1500, 2000, 1500, 2000, 1100, 800, 1800, 480, 3500, 700, 3400, 5000, 5000, 1300, 3000, 
                   2000, 900, 2000, 1000, 1800, 0, 1500, 788, 3500, 875, 5000, 2000, 1300, 1600, 0, 750, 
                   1200, 120, 1800, 1200, 788, 1000, 0, 1400, 2300, 7000, 4000, 5000, 788, 20000, 4000, 0, 
                   1200, 1800, 500, 1000, 788, 1300, 5000, 50, 1700, 2000, 1600, 2000, 1350, 1500, 600, 
                   1700, 780, 2446, 2100, 5000, 1700, 1200, 1000, 788, 4500, 1500, 788, 0, 1580, 1000,
                   4500, 2400, 788, 1100, 2000, 788, 100, 1200, 1200, 1200, 1000, 2000, 788, 2000, 15000, 600, 0, 1500, 3000, 4000, 900, 810, 600, 1500, 4000, 1200, 5000, 5300, 2500, 800, 0, 1400, 1500, 4000, 1200, 400, 1000, 820, 1000, 1000, 788, 1500, 2500, 1500, 220, 600, 788, 1750, 7000))
mulheres <- matrix(c(788, 1200, 6000, 1000, 788, 1100, 1900, 1577, 900, 950, 1200, 788, 788, 
                     1100, 30, 620, 900, 1000, 1200, 2000, 0, 500, 1200, 1500, 1200, 1120, 
                     788, 788, 2300, 2400, 3000, 788, 4000, 1000, 500, 500, 1700, 200, 6000, 
                     400, 950, 1100, 50, 930, 850, 1100, 3500, 1500, 1200, 900, 1100, 1500, 
                     788, 1000, 788, 4500, 4000, 8000, 3500, 788, 1050, 1000, 1400, 3500,
                     600, 3000, 500, 930, 2660, 788, 360, 2364, 788, 160, 1100, 1085, 1050, 
                     1500, 100, 0, 788, 250, 1700, 1300, 800, 0, 2000, 820, 910, 300, 2000, 
                     200, 788, 788, 800, 1500, 1300, 1200, 0, 600, 1036, 400, 1100, 788, 
                     400, 1500, 1200, 1800, 1000, 788, 850, 60, 1800, 3500, 600, 200, 
                     500, 200, 1100, 1540, 1100, 900, 800, 500, 200, 1200, 1250, 788, 500, 
                     200, 788, 200, 0, 1800, 2000, 1000, 900, 3000, 700, 1200, 788, 2800, 
                     3300, 400, 0, 850, 1022, 6000, 750, 1000, 3500, 400, 1500, 1000, 800, 
                     0, 980, 2400, 850, 1100, 788, 1100, 788, 1200, 788, 864, 1000, 500, 400, 
                     3000, 1200, 100, 80, 900, 2000, 1250, 1000, 300, 400, 1500, 60, 8000, 1000, 
                     600, 800, 350, 788, 0, 600, 788, 2500, 1300, 800, 8000, 1100, 800, 900, 
                     2000, 0, 800, 1400, 1000, 1200, 788, 3840, 788, 3940, 788, 560, 800, 2000, 
                     600, 2900, 0, 400, 800, 7000, 788, 788, 788, 788, 1250, 1500, 1386, 100, 
                     300, 788, 788, 600, 600, 900, 2800, 788, 350, 900, 0, 150, 788, 788, 
                     1000, 30, 788, 780, 900, 0, 0, 3000, 1800, 2000, 1100, 788, 0, 7500, 
                     1800, 788, 788, 2000, 3000, 180, 2500, 800, 0, 1250, 200, 1100, 0, 
                     1100, 400, 1300, 300, 0, 1312, 600, 200, 4000, 2400, 12000, 2150, 
                     500, 400, 0, 600, 400, 788, 1200, 500, 2000, 1500, 70, 500, 2000, 
                     1000, 100, 500, 2000, 100, 2115, 800, 5800, 2300, 750, 788, 0, 900,
                     1000, 600, 3000, 5500, 1600, 788, 4000, 0, 1100, 120, 320, 1100,
                     2500, 200, 800, 3000, 550, 4200, 7000, 720, 0, 2000, 5000, 2000, 788, 
                     500, 788, 7000, 500, 788, 1760, 1200, 3500, 1500, 150, 800, 788, 3000, 
                     400, 788, 1000, 2000, 2500, 0, 788, 5200, 788, 0, 600, 1300, 120, 850, 
                     1576, 788, 2000, 1300, 788, 800, 1800, 655, 1580, 789, 788, 850, 788, 
                     7000, 788, 430, 800, 788, 900, 10000, 1200, 300, 400, 788, 788, 3000, 900, 1300, 1300, 788, 800, 1000, 1890, 0, 788, 900, 4000, 788, 1010, 150, 450, 1700, 890, 1200, 2800, 1000, 788, 788, 700, 600, 800, 880, 6000, 800, 800, 820, 788, 1030, 560, 2100, 6500, 2500, 788, 400, 300, 788, 2300, 1000, 3500, 984, 1576, 420, 1700, 450, 1800, 400, 1500, 1500, 4256, 1200, 890, 1200, 300, 400, 850, 1500, 3800, 1800, 996, 2000, 1000, 788, 1500, 300, 600, 950, 1200, 788, 1200, 1500, 250, 788, 1200, 6000, 300, 789, 1500, 788, 3800, 780, 1200, 1200, 220, 788, 500, 200, 480, 1576, 1576, 1035, 900, 800, 1000, 805, 200, 1200, 2220, 1500, 880, 220, 2000, 788, 150, 0, 13000, 40, 5500, 788, 788, 1000, 400, 2000, 200, 1600))


#Média das amostras
media_amostra_M <- mean(mulheres)
media_amostra_M

#Média amostra homens
media_amostra_H <- mean(homens)
media_amostra_H

#desvio padrao de homens de mulheres
desvio_padrao_M <- sd(mulheres)
desvio_padrao_H <- sd(homens)

significancia <- 0.01
confianca <-  1 - significancia
n_M <- 500
n_H <- 500
D_0 <- 0 #DIFERENCA

#Teste significancia 
probabilidade <- confianca

z_alpha <- qnorm(probabilidade)

#calculo da estatistica teste e verificacao  desse valor  com as areas de aceitacao e rejeicao do teste
numerador <- (media_amostra_H - media_amostra_M) - D_0

denominador <- sqrt((desvio_padrao_H ** 2 /n_H )+(desvio_padrao_M **2 / n_M))

z <- numerador/denominador
z
# nesse caso caiu na area de rejeicao

# rejeitar h0 se z >= z_alpha
z >= z_alpha

#com nivel de 99% rejeitamos  h0, isto e concluimos  que a media das rendas do sexo masculino e maior
#que a media das rendas

#Criterio p valor
#rejeitar h0 se o valor p < significancia
z
p_valor <- pnorm(z, lower.tail = F)

p_valor <= significancia

#utilizando BSDA
install.packages("BSDA")
library(BSDA)

#z.test, fazendo direto no caso
resultado <- z.test(x = homens, 
                    y = mulheres,
                    alternative = 'greater',   #unicaudal superior nesse caso ulilizando greater
                    mu = 0,
                    sigma.x = sd(homens),
                    sigma.y = sd(mulheres),
                    conf.level = 0.99)
resultado

p_valor <- resultado$p.value
p_valor <= significancia

#Como observação
#t.test
resultado <- t.test(x = homens, 
                    y = mulheres,
                    alternative = 'greater',
                    paired = F,
                    var.equal = T
                    )
resultado

p_valor <- resultado$p.value
p_valor <= significancia


#Uma imobiliária da cidade do Rio de Janeiro alega que o valor do m² no bairro do Leblon está avaliado 
#em, no máximo, R$ 1000,00 a mais, quando comparado ao bairro de Ipanema. Uma pesquisa realizada com 
#uma amostra de 13 imóveis de cada bairro revelou os seguintes parâmetros:

media_Leblon <- 21800
desvio_padrao_Leblon <- 450
media_Ipanema <- 20300
desvio_padrao_Ipanema <- 320

significancia <- 0.05
confianca <- 1 - significancia
n_Leblon <- 13
n_Ipanema <- 13
D_0 <- 1000

t_alpha <- qt(confianca, n_Leblon + n_Ipanema - 2)

numerador <- (media_Leblon - media_Ipanema) - D_0
denominador <- sqrt((desvio_padrao_Leblon ** 2 / n_Leblon) + (desvio_padrao_Ipanema ** 2 / n_Ipanema))
t <- numerador / denominador
t

if (t >= t_alpha) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

# Testes nao paramétricos
#teste qui quadrado

#construindo tabela
p <- c(0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975, 0.95, 0.99, 0.995)
df <- seq(1, 30, by = 1)
probabilidade <- c()
for(i in df){
  for(j in p){
    probabilidade <- c(probabilidade, round(qchisq(j, i, lower.tail = T), 4))
  }
}
tabela_chi_2 <- matrix(probabilidade, ncol=13, byrow=TRUE)
colnames(tabela_chi_2) <- format(p)
rownames(tabela_chi_2) <- format(df)
tabela_chi_2

#dados do problema

F_Observada <- c(17,33)
F_Esperada <- c(25,25)
significancia <- 0.05
confianca <- 1 - significancia
k <-  2# Número de eventos possíveis
graus_de_liberdade <- k -1

#H0: FCARA = FCOROA
#H1: FCARA =! FCOROA

#Fixação da significancia do teste
tabela_chi_2[1:3, ]

#obtendo xa2
chi_2_alpha <- qchisq(confianca, graus_de_liberdade)
chi_2_alpha

#cálculo da estatistica teste e verificacao desse valor com as áreas de aceitação e rejeição do teste
chi_2 <- ( (F_Observada[1] - F_Esperada[1]) ** 2 / F_Esperada[1]) + ( (F_Observada[2] - F_Esperada[2]) ** 2 / F_Esperada[2])
chi_2

#obtendo de outra forma
chi_2 =0
for (i in 1:k){
  chi_2 <- chi_2 + ( (F_Observada[i] - F_Esperada[i]) ** 2 / F_Esperada[i])
}
chi_2

#calculo valor critico
#rejeitar  se x2teste  > x2alfa

chi_2 > chi_2_alpha
#com nivel de 95% de confianca rejeitamos a h0 e concluimos que as frequecias observadas e esperadas
#sao discrepantes, ou seja a moeda  nao é honesta e precisa ser substituída 

#critério valor p
#rejeitar h0 se o p valor <= significancia
p_valor <- pchisq(chi_2, df = graus_de_liberdade, lower.tail = F) #calda superior nesse caso F
p_valor

#Rodando a função direto
chisq.test(x = F_Observada)

resultado <- chisq.test(x = F_Observada, p=c(0.5, 0.5))
resultado

#teste
resultado$statistic

#parametros ou variavel
resultado$parameter

#p valor
p_valor <- resultado$p.value

#valores observados
resultado$observed

#valores esperados
resultado$expected

#Na realização de um teste não paramétrico Qui-quadrado, com apenas 5 graus de liberdade, 
#obteve-se a estatística de teste no valor de 7,45. Qual seria o p-valor para este teste?
graus_de_liberdade <- 5
chi_2 <- 7.45

pchisq(chi_2, graus_de_liberdade, lower.tail = F)COPIAR 

#Teste Wilcoxon
# Comparação de duas populações - amostras dependentes

#dados do problema
fumo <- data.frame(
  Antes = c(39, 25, 24, 50, 13, 52, 21, 29, 10, 22, 50, 15, 36, 39, 52, 48, 24, 15, 40, 41, 17, 12, 21, 49, 14, 55, 46, 22, 28, 23, 37, 17, 31, 49, 49),
  Depois = c(16, 8, 12, 0, 14, 16, 13, 12, 19, 17, 17, 2, 15, 10, 20, 13, 0, 4, 16, 18, 16, 16, 9, 9, 18, 4, 17, 0, 11, 14, 0, 19, 2, 9, 6)
)

significancia <- 0.05
confianca <- 1 - significancia
n <- 35

#visualizando a base
head(fumo)

media_antes <- mean(fumo$Antes)
media_depois <- mean(fumo$Depois)

#Formulação  das hipóteses H0 e H1
#H0 = media antes = media depois
#H0 = media antes > media depois

#fixação da significancia do teste alfa
#obtendo zalfa2

probabilidade <- (0.5+(confianca / 2))
probabilidade

z_alpha_2 <- qnorm(probabilidade)
z_alpha_2

#contruindo a tabela com os postos
fumo

fumo['Dif'] <- fumo$Depois - fumo$Antes
fumo
#como os valores estao neggativo devemos tirar o sinal

#pronto utilizamos  a função abs ("absoluto") para deixar positov
fumo['|Dif|'] = abs(fumo$Dif)
fumo

#ordenando
fumo <- fumo[order(fumo$'|Dif|'),]
fumo

#sequência ou rank
fumo$Posto <- seq(1, nrow(fumo))
fumo

#agregate
posto <- aggregate(x = fumo$'Posto', by =list(fumo$`|Dif|`), FUN = mean)
colnames(posto) <- c('|Dif|', 'Posto')
posto

#elimiando variavel no dataframe
fumo$Posto <- NULL
fumo

#unindo segundo a variavel dif absoluto
fumo <- merge(x = fumo, y= posto, by='|Dif|', all.x = T)
fumo

#aplicando a função a cada linha 
fumo['Posto (+)'] <- apply(fumo[, c('Dif', 'Posto')], 1, function(x) if(x[1] > 0) x[2] else 0 )
fumo

fumo['Posto (-)'] <- apply(fumo[, c('Dif', 'Posto')], 1, function(x) if(x[1] < 0) x[2] else 0 )
fumo

#elimiando variavel no dataframe
fumo$Posto <- NULL
fumo

#Obter T
T <- min( sum(fumo$`Posto (+)`), sum(fumo$`Posto (-)`))
T

#Obter mut
mu_T <- (n *(n+1)) / 4
mu_T

#sigma T
sigma_T <- sqrt((n  *(n+1) * (( 2 * n)+1)) / 24)
sigma_T

#obter z test
Z <-  (T - mu_T) / sigma_T
Z

#critério de valor critico
#rejeiar h0 se Z <= - zlpha 2 ou se  Z >= zlapha 2
Z <= -z_alpha_2
Z >= z_alpha_2
#rejeitamos a hipotese de que nao existe diferencas entre os grupos. COntudo, existe diferença entre
#as médias de cigarros fumados pelos pacientes antes era de 31.86 e depois foi para 11.2
# após do tratamento e podemos concluir que o tratamento apresentou resultado satisfatório 

#Fazendo direto
resultado <- wilcox.test(fumo$Depois, fumo$Antes, paired = T, correct=F)
resultado
#valor de V é o T
p_valor <- resultado$p.value

p_valor <= significancia

#exercicio


pesos <- data.frame(
  Sem_Composto = c(180, -39, 325, 303, 127, 149, 271, 163, 287, 255, 398, 324, 335, 421, 
                   216, 274, 373, 116, 197, -37, 321, 431, 112, 304, 417, 362, 51, 187, 195, 304, 202, 158, 105, 90, 245),
  Com_Composto = c(484, 187, 442, 108, 488, 283, 286, 419, 240, 266, 198, 130, 484, 424, 
                   145, 133, 282, 386, 408, 290, 429, 386, 318, 390, 347, 442, 440, 356, 517, 454, 401, 108, 228, 471, 495)
)

resultado <- wilcox.test(
  pesos$Sem_Composto,
  pesos$Com_Composto,
  alternative = "two.sided",
  paired = T
)

resultado

#Projeto

#xercicio
#Uma empresa de consultoria alega que seus consultores recebem um salário médio igual a,
#no mínimo, R$ 2.500,00, com desvio padrão de R$ 500,00 e distribuição aproximadamente normal. 
#Uma amostra aleatória de 300 consultores mostrou uma média de R$ 2.400,00. 
#A afirmação da empresa pode ser aceita a um nível de significância de 3%?

media <- 2500
media_amostra <- 2400
desvio_padrao <- 500
n <- 300
significancia <- 0.03

z <- (media_amostra - media) / (desvio_padrao / sqrt(n))
z

p_valor <- pnorm(z, lower.tail = T)
p_valor

if (p_valor <= significancia) {
  'Rejeitar H0'
} else {
  'Aceitar H0'
}

#Projeto
#Você é um pesquisador que estuda o mercado de trabalho brasileiro e resolve estudar as diferenças 
#salariais dos trabalhadores dos estados do Rio de Janeiro e de São Paulo. 
#Durante sua pesquisa você verifica que, aparentemente, os rendimentos dos trabalhadores 
#no estado do Rio de Janeiro são mais baixos que os rendimentos dos trabalhadores no estado de São Paulo.
#Para confirmar esta conclusão realize um teste de hipótese de comparação de médias em cima de 
#duas amostras de trabalhadores dos dois estados. Siga as seguintes etapas:
  
#Utilize o dataset 'dados_proj.csv' para rodar o teste. Este dataset tem uma amostra de 
#tamanho 500 dos rendimentos dos trabalhadores dos dois estados (RJ e SP).
#Considere o nível de significância de 5%.
#Teste a hipótese de que a renda média dos trabalhadores do Rio de Janeiro é menor que a
#renda média dos trabalhadores de São Paulo.

#lendo a base
dados <- read.csv('dados_proj.csv')
head(dados)

#dados do problema
#média
media_amostra_rj <- mean(dados$Renda_RJ)
media_amostra_rj

media_amostra_sp <- mean(dados$Renda_SP)
media_amostra_sp

#desvio padrao
desvio_padrao_amostra_rj <-sd(dados$Renda_RJ)
desvio_padrao_amostra_rj

desvio_padrao_amostra_sp <- sd(dados$Renda_SP)
desvio_padrao_amostra_sp


significancia <- 0.05
confianca <- 1 - significancia
n_rj <- nrow(dados)
n_sp <- nrow(dados)
D_0 <- 0

#formulação das hipóteses
#𝜇1⇒  Rendimento médio no Rio de Janeiro
#𝜇2⇒  Rendimento médio em São Paulo
#𝐻0:𝜇1≥𝜇2
#𝐻1:𝜇1<𝜇2

#fixaçãp da significancia do teste alfa
probabilidade <- significancia
probabilidade

z_alpha <- qnorm(probabilidade)
round(z_alpha, 2)

#cálculo da estatística-teste e verificação desse valor com as áreas de aceitação e rejeição do teste
numerador <- (media_amostra_rj - media_amostra_sp) - D_0
denominador <- sqrt((desvio_padrao_amostra_rj ** 2 / n_rj) + (desvio_padrao_amostra_sp ** 2 / n_sp))

z <- numerador / denominador
z

#Aceitação ou rejeição da hipótese nula
#Critério do valor crítico
z <= z_alpha

#Critério do valor p
z

pnorm(z, lower.tail = T)

p_valor <- pnorm(z, lower.tail = T)
p_valor

p_valor <= significancia
#Com um nível de confiança de 95% rejeitamos  h0 , isto é, concluímos que a renda média no 
#estado do Rio de Janeiro é menor que a renda média no estado de São Paulo.