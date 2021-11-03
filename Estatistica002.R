##########################################################################################################
################################### DISTRIBUIÇÃO E PROBABILDIADE #########################################
##########################################################################################################

#biblioteca
library(ggplot2)

#importando dataset
dados <- read.csv('bases/dados.csv')
head(dados)

############### Distribuição Binomial ################### 
#combinacoes

combinacoes <- choose(25,20)
combinacoes

probabilidade <- 1 / combinacoes
probabilidade

#Experimento Binomial

n <- 10
n

#probabilidade de sucesso
numero_de_alternativa_por_questao <- 3
p <- 1/ numero_de_alternativa_por_questao
p

#probabilidade de fracasso
q <- 1 - p
q

#total de eventos q quer ter sucesso
k <- 5
k

probabilidade <- choose(n,k) * (p ** k) * (q **(n -k))
probabilidade

#fazendo direto
dbinom(x = k, size= n, prob = p)

#e para passar na proxima fase?
sum(dbinom(x = 5: 10, size= n, prob = p))

#usando pbinom
pbinom(q = 4, size= n, prob = p, lower.tail = F)

#Exemplo gincana

p <- 0.7
p

n <- 5
n

k <- 2
k

probabilidade <- dbinom(x = k, size= n, prob = p)
probabilidade

equipes <- 1500 * probabilidade
equipes

################### Distribuição Poisson ################### 

#constante
exp(1)

#numero meio de ocorrencia por hota(u)?
media <- 20
media

#qual numero de occorencia que queremos obter no periodo (k)?
k <- 25 
k

probabilidade <- dpois(x =k, lambda = media)
probabilidade

################### Distribuição normal ################### 
#carregando biblitoeca
library(tidyverse)

#Tabelas padronizadas
Z <- seq(0, 3.99, by=0.01)
probabilidade <- pnorm(Z)
tabela_normal_padronizada <- matrix(probabilidade, ncol=10, byrow=TRUE)
colnames(tabela_normal_padronizada) <- format(seq(0.00, 0.09, by=0.01))
rownames(tabela_normal_padronizada) <- format(seq(0.00, 3.90, by=0.10), digits = 2, nsmall = 2)
view(tabela_normal_padronizada)

#exemplo qual sua altura?
media <- 8
media

desvio_padrao <- 30
desvio_padrao

Z <- (1.8- media) / desvio_padrao
Z

probabilidade <- 0.8413447
probabilidade

#fazendo direto
pnorm(Z)

#identificaçao da area sobre a curva
Z_inferior <- (650 - media) / desvio_padrao 
round(Z_inferior, 4)

Z_superior <- (750 - media) / desvio_padrao 
round(Z_superior, 4)

probabilidade <- (0.8413447 - 0.5) *2 
probabilidade

#outro metodo
probabilidade <- pnorm(Z_superior) - pnorm(Z_inferior)
probabilidade


Z <- (700- media) / desvio_padrao
Z

probabilidade <- 1- 0.9772499 
probabilidade

#outro metodo
probabilidade <- 1 -pnorm(Z)
probabilidade


#obtendo as probavilidades do valor Z
round(pnorm(1.23), 4) - round(pnorm(0), 4)

# 2. Z > 2,14
1 - round(pnorm(2.14), 4)

# 3. -1,56 < Z < 1,48
round(pnorm(1.48), 4) - round(pnorm(-1.56), 4)

# 4. Z < -0,78
round(pnorm(-0.78), 4)


#treinando
media <- 80
desvio_padrao <- 10
N <- 60

Z = (90 - media) / desvio_padrao

n <- pnorm(-Z) * N
round(n)

################### Amostragem ################### 

#Amostragem aleatoria simples
mean(dados$Renda)

#definindo uma aleatoriedade
set.seed(2811)

#selecionando uma amostra de 1000 do dadso
amostra <- sample_n(dados, 1000)

#verificando tamanho da amostra
nrow(amostra)

#verificando a  media da amostra
mean(amostra$Renda)

#vendo o percentual da amostra e da base original
prop.table(table(dados$Sexo))
prop.table(table(amostra$Sexo))
  
#Amostragem estratificada
#Amostragem por conglomerados


#Teorema de limite central
n <- 2000
total_de_amostras <- 1500

for (i in 1:total_de_amostras){
  if(i == 1){
    amostras <- data.frame('Amostra_1' = sample(dados$Idade, n))
  }else{
    amostras[paste('Amostra_', i)] <- sample(dados$Idade, n)
  }
}
amostras

#media de cada coluna
colMeans(amostras)

#histgrama das idades médias
hist(
  x = colMeans(amostras),
  main = 'Histograma das Idades Médias',
  xlab = 'Idades',
  ylab = 'Frequências'
)

#média da população
mean(dados$Idade)

#media das media
mean(colMeans(amostras))

#desvio padrao
sd(colMeans(amostras))

sd(dados$Idade) / sqrt(n)
  
#Intervalos de COnfiança

media_amostral <- 20 
media_amostral

significancia <- 0.05
significancia

confianca <-  1 - significancia
confianca

tabela_normal_padronizada

0.95/2

#area sob a curva normal
0.5+(0.95 /2)
#1.96

#ddescobrindo a area de confianca
z <- qnorm(0.975)
z 

#desvio padrao conhecido
desvio_padrao <- 20
desvio_padrao

#media amostral
n <- 120
n

#raiz de n
raiz_de_n <- sqrt(n)
raiz_de_n

#sigma
sigma <- desvio_padrao / raiz_de_n
sigma

#obtendo e
e <- z * sigma
e

#calculando intervalo de confianca para a média
intervalo <- c(
  media_amostral - e,
  media_amostral + e
)
intervalo

#carregando pacote
library(DescTools)

#metodo2 #calculando intervalo de confianca para a média
MeanCI(x = 28, sd = sigma, type = 'norm', conf.level = 0.90, sides = 'two.sided')

#na base de dados original
mean(dados$Idade) - (z * (sd(dados$Idade) / sqrt(nrow(dados))))
mean(dados$Idade) + (z * (sd(dados$Idade) / sqrt(nrow(dados))))

sigma <- sd(dados$Idade) / sqrt(nrow(dados))

MeanCI(x = mean(dados$Idade), sd = sigma, type = 'norm', conf.level = 0.95, sides = 'two.sided')


media <- 120
desvio_padrao <- 20

# 1. Qual a probabilidade de um voo durar menos de 110 min?
Z = (110 - media) / desvio_padrao

probabilidade <- pnorm(Z)
round(probabilidade, 4)

# 2. Qual a probabilidade de um voo durar mais de 90 min?
Z = (90 - media) / desvio_padrao

probabilidade <- pnorm(-Z)
round(probabilidade, 4)

# 3. Qual a duração mínima de 80% das viagens?
Z <- -qnorm(0.80)
x <- Z * desvio_padrao + media

round(x, 1)

##### Calculo do tamano da amostra ######
#variaveis quantitativas  e população infinita

0.95 /2

0.5+(0.95 / 2)

#area
z <- qnorm(0.95)

#sigma dados
sigma <- 3323.9

#erro
e <- 0.1

#descobrindo n
n <- (z * (sigma / e)) **2
round(n)

#media
media <- 45.5

#sd
desvio_padrao <- 15

#erro
e <- 0.1

#confianca 0.90
significancia <- 0.1

confianca <- 1 - significancia


#Z
Z <- qnorm(0.5 + (confianca / 2))
e <- e * media

n = ( Z * ( desvio_padrao / e ) ) ** 2
round(n)

#desvio
desvio_padrao <- 42

#erro
e <- 6

#confianca
confianca <- 0.9

#descorbindo Z
Z <- qnorm(0.5 + (confianca / 2))

#descobrindo n
n = ( Z * ( desvio_padrao / e ) ) ** 2
round(n)

#Variaveis quantitativas e população  finita
N <-  2000

#obtendo z
z <- qnorm((0.5+(0.95/ 2)))

#obtendo s
s <- 0.48

#obtendo e
e <- 0.3

#obtendo n
n <- ((z**2) * (s**2) * (N)) / ((z**2)*(s**2)+(e**2)*(N -1)) 
round(n)

# vender 12 carro em 15 dias, mas queremos saber so 7 
media <- 12 / 2
k <- 7

probabilidade <- dpois(x = k, lambda = media)
round(probabilidade, 6)

#Projeto

head(dados)

#Problema A

k <- 7
n <- 10
p <- 0.70

probabilidade <- dbinom(k, n, p)
probabilidade
# A probilidade de que seja 7 homens é de 26%


# media de distribuição binomial
#media = n *p
# n = media / p
n <- 100 / probabilidade
n <- round(n)
n
#precisa-se selecionar 375 grupos para que seja forma um om 7 homens e 3 mulheres

#2 - Problema

#Selecao de amostra simples
set.seed(100)
dataset <- sample_n(dados, 200)$Renda

#media do dataset
mean(dataset)

#desvio padrao
sd(dataset)

#Dados do Problema
media_amostra <- mean(dataset)
desvio_padrao_amostra <- sd(dataset)
recursos <- 150000
custo_entrevista <- 100

#Obtendo a margem de erro
e <- 0.10 * media_amostra
glue('A marge  de erro é de R$ {format(e, digits= 2, decimal_mark=",", nsmall=2, big.mark=".", scientific=F
     )} para mais ou menos')

#taamnho da amostra 90% confianca
0.5+(0.9 / 2)

z <- qnorm(.90)
n_confianca_90 <- (z*(desvio_padrao_amostra / e)) **2
n_confianca_90 <-round(n_confianca_90)
glue('Para um nível de confianca de 90% devemos selecionar uma amostra de {n_confianca_90} elementos')
 
#tamanho da amostra 95% confianca
0.5 + (0.95 /  2) 

z <- qnorm(.95)
n_confianca_95 <- (z*(desvio_padrao_amostra / e)) **2
n_confianca_95 <-round(n_confianca_95)
glue('Para um nível de confianca de 95% devemos selecionar uma amostra de {n_confianca_95} elementos')

#tamanho da amostra 99% confianca
0.5 + (0.99 /  2) 

z <- qnorm(.99)
n_confianca_99 <- (z*(desvio_padrao_amostra / e)) **2
n_confianca_99 <-round(n_confianca_99)
glue('Para um nível de confianca de 99% devemos selecionar uma amostra de {n_confianca_99} elementos')

#custo de pqsuisa  para o nivel de confianca de 90%
custo_confianca_90 <- n_confianca_90 * custo_entrevista
glue('Para um nivel de confianca de 90%  o custo da pesquisa seria {format(
     custo_confianca_90, digits= 2, decimal_mark=",", nsmall=2, scientific=F
     )}')


#custo de pqsuisa  para o nivel de confianca de 95%
custo_confianca_95 <- n_confianca_95 * custo_entrevista
glue('Para um nivel de confianca de 95%  o custo da pesquisa seria {format(
     custo_confianca_95, digits= 2, decimal_mark=",", nsmall=2, scientific=F
     )}')

#custo de pqsuisa  para o nivel de confianca de 99%
custo_confianca_99 <- n_confianca_99 * custo_entrevista
glue('Para um nivel de confianca de 99%  o custo da pesquisa seria {format(
     custo_confianca_99, digits= 2, decimal_mark=",", nsmall=2, scientific=F
     )}')


#intervalo de confianca
probabilidade <- 0.5 + (0.95 /2)
z <- qnorm(probabilidade)
media_amostra - (z *(desvio_padrao_amostra / sqrt(n_confianca_95)))
media_amostra + (z *(desvio_padrao_amostra / sqrt(n_confianca_95)))

#margem de erro considerando todo recurso
n_confianca_95 <- recursos / custo_entrevista
n_confianca_95

z <- qnorm(.975)
e <- z *(desvio_padrao_amostra / sqrt(n_confianca_95))
e

e_percentual <- (e / media_amostra) * 100
glue('a nova margem de erro é de {format(e_percentual, digits= 2, nsmall =2, decimal.mark = ",")}%')

#assumindo nivel de confianca de 95, quanto a pesquisa custaria  condeirando margem de erro de apenas 5% em
#relacao a media estimada?

e <- 0.05 * media_amostra
glue('A margem de erroé de R$ {format(e, digits= 2, nsmall =2, decimal.mark = ",", scientific=F)} para mais
     ou menos')

z <- qnorm(.975)
n_confianca_95 <- (z*(desvio_padrao_amostra / e)) **2
n_confianca_95 <-round(n_confianca_95)
glue('Para um nível de confianca de 95% devemos selecionar uma amostra de {n_confianca_95} elementos')


custo_confianca_95 <- n_confianca_95 * custo_entrevista
glue('Para um nivel de confianca de 95%  o custo da pesquisa seria {format(
     custo_confianca_95, digits= 2, decimal_mark=",", nsmall=2, scientific=F
     )}')

  
#Considere que a variável X segue uma distribuição normal, com média igual a 500 e desvio padrão igual a 100. 
#Obtenha o valor de y, de forma que P(X ≤ y) = 0,05.


media <- 500
desvio_padrao <- 100

Z <- qnorm(0.05)

x <- Z * desvio_padrao + media
round(x, 2)