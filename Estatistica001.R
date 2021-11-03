#VAMOS UTILIZAR DADOS DO IBGE
#Fonte dos Dados:
https://ww2.ibge.gov.br/home/estatistica/populacao/trabalhoerendimento/pnad2015/microdados.shtm
#Observação
#Os seguintes tratamentos foram realizados nos dados originais:
  
#Foram eliminados os registros onde a Renda era inválida (999 999 999 999);
#Foram eliminados os registros onde a Renda era missing;
#Foram considerados somente os registros das Pessoas de Referência de cada domicílio (responsável pelo domicílio).
##########################################################################################################
################################### ANÁLISE DESCRITVA SIMPLES ############################################
##########################################################################################################

# informações básicas do R
sessionInfo()

# Instalação e Carregamento de Todos os Pacotes ---------------------------

pacotes <- c("datasets","forecast","fpp2","tseries","patchwork", "DataCombine", "TTR", "dplyr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


# importando dataset do projeto
 dados <- read.csv("dados.csv")
 dados
 
# visualizando os 5 primeiros
 head(dados,5)

# Variaveis Qualitativas Ordinais
 unique(select(dados, Anos.de.Estudo))

# ordenando
 arrange(unique(select(dados, Anos.de.Estudo)), Anos.de.Estudo)

# usando vetor
 c(arrange(unique(select(dados, Anos.de.Estudo)), Anos.de.Estudo))
 
# Variaveis Qualitativas Nominais
  c(arrange(unique(select(dados, UF)), UF))
  c(arrange(unique(select(dados, Sexo)), Sexo))
  c(arrange(unique(select(dados, Cor)), Cor))

# Variaveis Qualitativas Discretas
 sprintf('De %s até  %s Anos', min(dados$Idade), max(dados$Idade))

#usando pacote glue 
 library(glue)

 glue(" De {min(dados$Idade)} até {max(dados$Idade)} Anos")

# Variaveis Qualitativas Continuas

  glue(" De {min(dados$Altura)} até {max(dados$Altura)} Metros")

##########################################################################################################
################################### DISTRIBUIÇÃO DE FREQUÊNCIAS ##########################################
##########################################################################################################

table(dados$Sexo)

# Analisando em porcentagem
prop.table(table(dados$Sexo)) * 100
  
# Crinado um dataframe com a frequencia e o percentual do sexo

dist_freq_qualitativ <- cbind(freq = table(dados$Sexo), percent = prop.table(table(dados$Sexo)) * 100)
dist_freq_qualitativ

# criando matriz e atribuindo nomes
colnames(dist_freq_qualitativ) <- c('Frequência', 'Porcentagem(%)')

# agora para as linhas
rownames(dist_freq_qualitativ) <- c('Masculino', 'Feminino')

dist_freq_qualitativ

# Metódo 2

frequencia <- table(dados$Sexo, dados$Cor)
frequencia

rownames(frequencia) <- c('Masculino', 'Feminino')
colnames(frequencia) <- c('Indígena','Branca','Preta','Amarela','Parda')

# Deixando como matriz
frequencia <- cbind(frequencia)
frequencia

# Mostrando em percentual
percentual <- prop.table(frequencia) *100
percentual

# Função Tapply fazendo a média de renda ddas raças
media <- tapply(dados$Renda, list(dados$Sexo, dados$Cor), mean)
rownames(media) <- c('Masculino', 'Feminino')
colnames(media) <- c('Indígena','Branca','Preta','Amarela','Parda')
media


# Variaveis quantitativas

#definindo as classes dos salarios e sua frequencia

# A ► Acima de 20 SM
# B ► De 10 a 20 SM
# C ► De 4 a 10 SM
# D ► De 2 a 4 SM
# E ► Até 2 SM

# onde SM é o valor do salário mínimo na época. Em nosso caso R$ 788,00 (2015):
  
# A ► Acima de 15.760
# B ► De 7.880 a 15.760
# C ► De 3.152 a 7.880
# D ► De 1.576 a 3.152
# E ► Até 1.576

min(dados$Renda)
max(dados$Renda)

classes <- c(0,1576,3152, 7880,15760,200000)
labels <- c('E', 'D', 'C','B', 'A')


frequencia <- table(
    cut(
    x = dados$Renda,
    breaks = classes,
    labels = labels,
    include.lowest = T)
)
frequencia

# fazendo percentual
percentual <- prop.table(frequencia) * 100
percentual

#visualizando como df
dist_freq_quantitativas_personalizadas <- cbind('Frequência' = frequencia, 'Porcentagem (%)' =percentual)
dist_freq_quantitativas_personalizadas

#ordenando
dist_freq_quantitativas_personalizadas[
  order(row.names(dist_freq_quantitativas_personalizadas)),
]

# Definindo o número de classes
# Regra de Sturges

#verificando numero de registros no df
n <- nrow(dados)
n

#numero de classes
k <- 1+(10/3)*log10(n)
k

k <- round(k)
k
#no caso tenho 17 classes

labels <- c(
  '      0.00 |—|  11,764.70', 
  ' 11,764.70  —|  23,529.40', 
  ' 23,529.40  —|  35,294.10', 
  ' 35,294.10  —|  47,058.80', 
  ' 47,058.80  —|  58,823.50', 
  ' 58,823.50  —|  70,588.20', 
  ' 70,588.20  —|  82,352.90', 
  ' 82,352.90  —|  94,117.60', 
  ' 94,117.60  —| 105,882.00', 
  '105,882.00  —| 117,647.00', 
  '117,647.00  —| 129,412.00', 
  '129,412.00  —| 141,176.00', 
  '141,176.00  —| 152,941.00', 
  '152,941.00  —| 164,706.00', 
  '164,706.00  —| 176,471.00', 
  '176,471.00  —| 188,235.00', 
  '188,235.00  —| 200,000.00'
)

frequencia <- table(
  cut(
    x = dados$Renda,
    breaks = k, #estamos usando k (total de classes)
    labels = labels,
    include.lowest = T)
)
frequencia


# fazendo percentual
percentual <- prop.table(frequencia) * 100
percentual

#visualizando como df
dist_freq_quantitativas_amplitude_fixa <- cbind('Frequência' = frequencia, 'Porcentagem (%)' =percentual)
dist_freq_quantitativas_amplitude_fixa

#Plotando Histogram

hist(dados$Altura)

hist(
  x = dados$Altura,
  breaks = 'Sturges',
  col = 'lightblue',
  main = 'Histograma das Alturas',
  xlab = 'Altura',
  ylab = 'Frequência',
  prob = TRUE,
  las = 1
)

# Utilizando ggplot

library(ggplot2)

ggplot(dados, aes(x = Altura)) + 
         geom_histogram(binwidth = 0.02, color = "black", alpha = 0.9) + 
         ylab("Frequência") + 
         xlab("Alturas") + 
         ggtitle('Histograma das Alturas') +
         theme(
           plot.title = element_text(size = 14, hjust = 0.5),
           axis.title.y = element_text(size = 12, vjust = +0.2),
           axis.title.x = element_text(size = 12, vjust = -0.2),
           axis.text.y = element_text(size = 10),
           axis.text.x = element_text(size = 10)
         )

# atribuindo theme a variavel formatos
formatos <- theme(
  plot.title = element_text(size = 14, hjust = 0.5),
  axis.title.y = element_text(size = 12, vjust = +0.2),
  axis.title.x = element_text(size = 12, vjust = -0.2),
  axis.text.y = element_text(size = 10),
  axis.text.x = element_text(size = 10)
)


ggplot(dados, aes(x = Altura, y = ..density..)) + 
  geom_histogram(binwidth = 0.02, color = "black", alpha = 0.9) + 
  geom_density(color = 'red') +
  ylab("Frequência") + 
  xlab("Alturas") + 
  ggtitle('Histograma das Alturas') +
  formatos  #utilizando a variavel criada

#transformando em um dataframe para fazer o grafico de classes de rendas
bar_chart <- data.frame(dist_freq_quantitativas_personalizadas)
bar_chart

#gráfico de classes de rendas
ggplot(bar_chart, aes(x = row.names(bar_chart), y = bar_chart$Frequência)) + 
  geom_bar(stat = "identity") + 
  ylab("Frequência") + 
  xlab("Classes de Renda") + 
  ggtitle('Gráfico Classes de Renda') +
  formatos

##########################################################################################################
################################### MEDIDAS DE TENDÊNCIA CENTRAL #########################################
##########################################################################################################

# vetores
materias <- c('Matemática', 'Português', 'Inglês', 'Geografia', 'História', 'Física', 'Química')
Fulano <- c(8, 10, 4, 8, 6, 10, 8)
Beltrano <- c(10, 2, 0.5, 1, 3, 9.5, 10)
Sicrano <- c(7.5, 8, 7, 8, 8, 8.5, 7)

#criando df

df <- data.frame(Fulano,Beltrano,Sicrano, row.names=materias)
df

# Média
mean(df$Fulano)
mean(dados$Renda)

# medias das rendas por sexo
aggregate(list(Renda=dados$Renda), list(Sexo = dados$Sexo), mean)

# Mediana

df_fulano <- df[order(df$Fulano), ]
df_fulano

n <-  nrow(df_fulano)
n

elemento_md <- (n+1)/2
elemento_md

df_fulano[elemento_md, ]

#fazendo direto
median(df$Fulano)

set.seed(101)
sample(nrow(df), 6)

#deixando com 6 materias aleatorias
df_beltrano <- df[sample(nrow(df), 6), ]
df_beltrano

#ordenando
df_beltrano <- df_beltrano[order(df_beltrano$Beltrano), ]
df_beltrano

n <-  nrow(df_beltrano)
n 

#elemento mediano
elemento_md_b <- n/2
elemento_md_b

mean(df_beltrano[c(elemento_md_b, elemento_md_b + 1), ]$Beltrano)

median(df_beltrano$Beltrano)

# Obtendo a media em nosso dataset
median(dados$Renda)

#Moda
#criando a função
Moda <- function(x) {
  freq <- table(x)
  return(names(freq)[freq == max(freq)])
}

Moda(df$Fulano)

#fazendo na nossa base
Moda(dados$Renda)

# Avaliando a variavel Renda
ggplot(dados[dados$Renda < 20000, ], aes(x= Renda, y = ..density..)) +
  geom_histogram(binwidth = 500)+
  geom_density(color = 'red')

moda <- as.numeric(Moda(dados$Renda))
moda

mediana <- median(dados$Renda)
mediana

media <- mean(dados$Renda)
media

moda < mediana
mediana < media

# Avaliando a variavel Altura
ggplot(dados, aes(x= Altura, y = ..density..)) +
  geom_histogram()+
  geom_density(color = 'red')


moda <- as.numeric(Moda(dados$Altura))
moda

mediana <- median(dados$Altura)
mediana

media <- mean(dados$Altura)
media

# Avaliando a variavel anos de estudos
ggplot(dados, aes(x= Anos.de.Estudo, y = ..density..)) +
  geom_histogram()+
  geom_density(color = 'red')

moda <- as.numeric(Moda(dados$Anos.de.Estudo))
moda

mediana <- median(dados$Anos.de.Estudo)
mediana

media <- mean(dados$Anos.de.Estudo)
media

moda > mediana
mediana > media

##########################################################################################################
########################################## MEDIDAS SEPARATRIZES ##########################################
##########################################################################################################

#Quartis
quantile(dados$Renda)

#Decis
decis <- c()
for(i in 1:9){
  decis <- c(decis, i/10)
}
quantile(dados$Renda, decis)

#Centis
centis <- c()
for(i in 1:99){
  centis <- c(centis, i/100)
}
quantile(dados$Renda, centis)

#Plotando
ggplot(data = dados, aes(x = Idade)) + 
  geom_histogram(
    aes(y = cumsum(..count..)/sum(..count..)),  #fazendo a soma acumulada div pela soma 
    bins = 10
  ) + 
  geom_freqpoly(
    aes(y = cumsum(..count..)/sum(..count..)), 
    color = 'green'
  )

quantile(dados$Idade, decis)
#50 % das pessoas tem idade abaixo de 43 anos

#Classificao percentual
length(dados$Idade[dados$Idade <= 40]) / length(dados$Idade) *100
#42% estao com idade abaixo de 40anos

#Pessas que ganhavam no máximo meio salário mínimo
length(dados$Renda[dados$Renda <= 788/2]) / length(dados$Renda) *100
#10%

##########################################################################################################
########################################## BOXPLOT #######################################################
##########################################################################################################

#Criando os vetores
sexo = c(
  'Masculino', 
  'Feminino'
)
cor = c(
  'Indígena', 
  'Branca', 
  'Preta', 
  'Amarela', 
  'Parda'
)
anos_de_estudo = c(
  'Sem instrução e menos de 1 ano', 
  '1 ano', 
  '2 anos', 
  '3 anos', 
  '4 anos', 
  '5 anos', 
  '6 anos', 
  '7 anos', 
  '8 anos', 
  '9 anos', 
  '10 anos', 
  '11 anos', 
  '12 anos', 
  '13 anos', 
  '14 anos', 
  '15 anos ou mais', 
  'Não determinados'
)

#Plotando Altura
ggplot(data = dados, aes(x = "#", y = Altura)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = '#3274A1') + 
  coord_flip() +
  ylab("Metros") + 
  xlab("") + 
  ggtitle('Box-plot Alturas') +
  formatos

#Plotando Altura x Sexo
ggplot(data = dados, aes(x = Sexo, y = Altura, group = Sexo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Metros") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Alturas X Sexo') +
  formatos

#Mudando as labels
dados$Cat.Sexo <- factor(dados$Sexo)
#Criando os niveis
levels(dados$Cat.Sexo) <- sexo

# Sexo x Altura
ggplot(data = dados, aes(x = Cat.Sexo, y = Altura)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Metros") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Alturas X Sexo') +
  formatos

# Plotando Renda
ggplot(data = dados[dados$Renda < 10000, ], aes(x = "", y = Renda)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = '#3274A1') + 
  coord_flip() +
  ylab("R$") + 
  xlab("") + 
  ggtitle('Box-plot Renda') +
  formatos

#Plotando Renda x Sexo
ggplot(data = dados[dados$Renda < 10000, ], aes(x = Cat.Sexo, y = Renda)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("R$") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Renda X Sexo') +
  formatos

#Mudando as labels
dados$Cat.Anos.de.Estudo <- factor(dados$Anos.de.Estudo, order = T)
#Criando os niveis
levels(dados$Cat.Anos.de.Estudo) <- anos_de_estudo

head(dados)

#Plotando anos de estudo
ggplot(data = dados, aes(x = "", y = Anos.de.Estudo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = '#3274A1') + 
  coord_flip() +
  ylab("Anos") + 
  xlab("") + 
  ggtitle('Box-plot Anos de Estudo') +
  formatos

# Plotado Anos de estudo X Sexo
ggplot(data = dados, aes(x = Cat.Sexo, y = Anos.de.Estudo)) + 
  stat_boxplot(geom ='errorbar', width = 0.4) + 
  geom_boxplot(fill = c('#3274A1', "orange")) + 
  coord_flip() +
  ylab("Anos") + 
  xlab("Sexo") + 
  ggtitle('Box-plot Anos de Estudo X Sexo') +
  formatos


#Renda de Sao Paulo x Bahia
dados$Cat.UF <- factor(dados$UF) 

ggplot( 
  data = dados[(dados$UF == 29 | dados$UF == 35) & dados$Renda < 10000, ],  
  aes(y = Renda, x = Cat.UF) 
) +  
  stat_boxplot(geom ='errorbar', width = 0.4) +  
  geom_boxplot(fill = c('#3274A1', "orange")) +  
  coord_flip() + 
  ylab("R$") +  
  xlab("UF") +  
  ggtitle('Renda (R$) - Bahia X São Paulo') + 
  formatos
  
##########################################################################################################
######################################### MEDIDAS DE DISPERSAO ###########################################
##########################################################################################################

#Desvio Médio Absoluto

df

#estatistica descritivas
summary(df)

notas_fulano <- data.frame(Fulano = df$Fulano, row.names = row.names(df))
notas_fulano

#calculando media
notas_media_fulano <- mean(notas_fulano$Fulano)
notas_media_fulano

#criando a variavel desvio
notas_fulano$Desvio <- notas_fulano$Fulano - notas_media_fulano
notas_fulano

#tirar o valores negativos e pegar os absoluto
notas_fulano$Desvio.Abs <- abs(notas_fulano$Desvio)
notas_fulano

#desvios em relacao a media
ggplot(data = notas_fulano, aes(x = row.names(notas_fulano), y = Fulano)) + 
  geom_point() + 
  geom_hline(yintercept = mean(notas_fulano$Fulano), color = 'red') + 
  geom_segment(aes(x = 1, y = 10, xend = 1, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 2, y = 8, xend = 2, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 3, y = 6, xend = 3, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 4, y = 4, xend = 4, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 5, y = 8, xend = 5, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 6, y = 10, xend = 6, yend = mean(notas_fulano$Fulano))) + 
  geom_segment(aes(x = 7, y = 8, xend = 7, yend = mean(notas_fulano$Fulano)))

#verificando a media do desvio absoluto
mean(notas_fulano$Desvio.Abs)

#instalando novo pacote
install.packages("DescTools")
library(DescTools)

#fazendo a media do desvio abs de forma mais rapida
MeanAD(df$Fulano)

# Variância
notas_fulano$Desvio2 <- notas_fulano$Desvio ^ 2
notas_fulano

sum(notas_fulano$Desvio2) / (nrow(notas_fulano) - 1)

#de forma direta
variancia <- var(notas_fulano$Fulano)
variancia

#Desvio Padrão
sqrt(variancia)

#de forma direta
desvio_padrao <- sd(notas_fulano$Fulano)
desvio_padrao

#visualizando o df
df

#descritiva
summary(df)

#a nota que mais aparece
Moda(df$Fulano)
Moda(df$Sicrano)

#Mas será que realmente possui uma mesma variancia?
sd(df$Fulano)
sd(df$Sicrano)
# nao, as notas do sicrano sao costantes agora do Fulano, varia muito 


# Projetin

dados


#Classes de renda:
  
#A ► Acima de 25 SM
#B ► De 15 a 25 SM
#C ► De 5 a 15 SM
#D ► De 2 a 5 SM
#E ► Até 2 SM

classes <- c(
  min(dados$Renda),
  2 * 788,
  5 * 788,
  15* 788,
  25* 788,
  max(dados$Renda)
)

classes

labels <- c('E', 'D', 'C', 'B', 'A')
labels

#construindo a coluna frequencias
frequencia <- table(
  cut(
  x = dados$Renda,
  breaks =classes,
  labels = labels,
  include.lowest = T
 )
)
frequencia

# fazendo percentual
percentual <- prop.table(frequencia) * 100
percentual

#juntado
dist_fre_renda <- cbind('Frequencia' = frequencia, 'Porcentagem %' = percentual)
dist_fre_renda

#ordenando
dist_fre_renda[
  order(row.names(dist_fre_renda)),
]

#transformando em um dataframe para fazer o grafico de classes de rendas
barra_chart <- data.frame(dist_fre_renda)
barra_chart

#gráfico de classes de rendas
ggplot(barra_chart, aes(x = row.names(barra_chart), y = barra_chart$Frequencia))+
  geom_bar(stat = 'identity')+
  ylab("Frequencia")+
  xlab("Classes Renda")+
  ggtitle("Gráfico Classes de Renda")+
  formatos

#Histograma variavel quantitativa Idade
ggplot(dados, aes(x =Idade))+
  geom_histogram(bins = 50)+
  ylab("Frequencia")+
  xlab("idades")+
  ggtitle("Histograma de Idade")+
  formatos

#Histograma variavel quantitativa Altura
ggplot(dados, aes(x =Altura))+
  geom_histogram(bins = 50)+
  ylab("Frequencia")+
  xlab("Altura")+
  ggtitle("Histograma de Altura")+
  formatos

#Histograma variavel quantitativa Renda
ggplot(dados, aes(x = Renda))+
  geom_histogram(bins = 100)+
  ylab("Frequencia")+
  xlab("Renda")+
  ggtitle("Histograma de Renda")+
  formatos


#Avaliando a variavel Renda de pessoas com rendimento até R$ 20.000,00
ggplot(dados[dados$Renda < 20000, ], aes(x= Renda)) +
  geom_histogram()+
  ylab("Frequencia")+
  xlab("R$")+
  ggtitle("Histograma de Renda - Pessoas com Renda  até R$ 20k")+
  formatos

# Tornando as variaveis catégoricas
#Sexo
dados$Cat.Sexo <- factor(dados$Sexo)
levels(dados$Cat.Sexo) <- sexo

#Cor
dados$Cat.Cor <- factor(dados$Cor)
levels(dados$Cat.Cor) <- cor

#Anos de estudo
dados$Cat.Anos.de.Estudo <- factor(dados$Anos.de.Estudo, order = T)
levels(dados$Cat.Anos.de.Estudo) <- anos_de_estudo

head(dados)

#Fazendo a tabela de frequencia
frequencia <- table(dados$Cat.Sexo, dados$Cat.Cor)
frequencia <- cbind(frequencia)
frequencia

# Mostrando em percentual
percentual <- cbind(prop.table(frequencia)*100)
percentual

#Analise da renda
mean(dados$Renda)

#mediana
median(dados$Renda)

#convertendo para numeric pois a funçção moda retorna string
as.numeric(Moda(dados$Renda))

#variancia
var(dados$Renda)

#desvio padrao
sd(dados$Renda)

#media da renda segundo sexo e cor
medias <- tapply(dados$Renda, list(dados$Cat.Sexo, dados$Cat.Cor), mean)
medias

#renda mediana segundo sexo e cor
medianas <- tapply(dados$Renda, list(dados$Cat.Sexo, dados$Cat.Cor), median)
medianas

#valor maximo da renda segundo sexo e cor
maximo <- tapply(dados$Renda, list(dados$Cat.Sexo, dados$Cat.Cor), max)
maximo

#variancia da renda segundo sexo e cor
variancias <- tapply(dados$Renda, list(dados$Cat.Sexo, dados$Cat.Cor), var)
variancias

#desvio padrao da renda segundo sexo e cor
sds <- tapply(dados$Renda, list(dados$Cat.Sexo, dados$Cat.Cor), sd)
sds


#Boxplot de renda por sexo e cor
ggplot(data = dados[dados$Renda < 10000, ], aes(x = Cat.Cor, y = Renda, fill =Cat.Sexo)) + 
  geom_boxplot(size = 0.2) + 
  coord_flip() +
  ylab("R$") + 
  xlab("Cor") + 
  guides(fill = guide_legend(title = 'Sexo')) +
  ggtitle('Box-plot Renda por Sexo e Cor') +
  formatos

#Percentual de pessoas do dataset que ganham saalario minimo ou menos
length(dados$Renda[dados$Renda <= 788])/ length(dados$Renda) *100

#valor maximo ganho por 99% das pessoas
quantile(dados$Renda, .99)


#media de renda por ano de estudo e sexo 
medias <- tapply(dados$Renda, list(dados$Cat.Anos.de.Estudo, dados$Cat.Sexo), mean)
medias

#renda mediana por ano de estudo e sexo 
medianas <- tapply(dados$Renda, list(dados$Cat.Anos.de.Estudo, dados$Cat.Sexo), median)
medianas

#valor maximo da renda por ano de estudo e sexo
maximo <- tapply(dados$Renda, list(dados$Cat.Anos.de.Estudo, dados$Cat.Sexo), max)
maximo


#variancia da renda por ano de estudo e sexo
variancias <- tapply(dados$Renda, list(dados$Cat.Anos.de.Estudo, dados$Cat.Sexo), var)
variancias

#desvio padrao da renda por ano de estudo e sexo
sds <- tapply(dados$Renda, list(dados$Cat.Anos.de.Estudo, dados$Cat.Sexo), sd)
sds

#Boxplot de renda  por ano de estudo e sexo
ggplot(data = dados[dados$Renda < 10000, ], aes(x = Cat.Anos.de.Estudo, y = Renda, fill =Cat.Sexo)) + 
  geom_boxplot(size = 0.2) + 
  coord_flip() +
  ylab("R$") + 
  xlab("Anos de Estudo") + 
  guides(fill = guide_legend(title = 'Sexo')) +
  ggtitle('Box-plot Renda por Sexo e Anos de Estudos') +
  formatos

