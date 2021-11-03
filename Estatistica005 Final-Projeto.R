# Definindo o Projeto
# Pergunta: O que afeta a qualidade do ar?

# Instalando pacotes
install.packages("Ecdat")
library(Ecdat) #carregando pacotes

# carregando dataset
data(Airq)

# verificando nome das variaveis
names(Airq)

# Descrevendo variaveis
# airq:indice de qualidade do ar (quanto menor, melhor)
# vala: valor das empresas nas cidades (milhares de dólares)
# rain: quantiade de chuva (em polegadas)
# coas: posição costeira da cidade (sim ou não)
# dens: densindade populacional (milha quadrada)
# medi: renda média per capita (dólares )

# Análise descritiva ou explanatória 
# ------------------------------------------

summary(Airq) #sumário das variáveis

# as variáveis podem ser contínuas ou categóricas (dividas em categorias)
# a variável resposta é a qualidade do ar (airq)
plot(airq~vala, data=Airq)

# Criando um  modelo estatistico
# y (resposta) ~ x (explicativa)
# y ~ x1 + x2 + x3
# airq ~ vala + coas + rain

############################################################################################################

# Montando  modelo
# ------------------------------------------

m1 <- lm(airq~vala, data=Airq) # lm (Modelo Linear)
# alguns dados podem nao ser lineares
summary(m1) #para saber a significância do modelo
plot(airq~vala, data=Airq) # plot de regressão linear

# o p-valor indica  a significancia do modelo ou da variavel
# se o p-valor  < 0.05 a variável é significativa 
# se o p-valor > 0.05 não existe efeito esperado 

# a variável "vala" não influenciou a qualidade de ar nas cidades ("airq")

# a variáel "coas" afeta a variável  "airq"
m2 <- lm(airq~coas, data=Airq)
summary(m2) 
# Sim, a posição costeira da cidade influencia a qualidade do ar nas cidades
# as cidades costeiras apresenta melhor qualidade do ar
plot(airq~coas, data=Airq)
  
# A variável medi afeta  a qualidade do ar?      
m3 <- lm(airq~medi, data=Airq)
summary(m3)

plot(airq~medi, data=Airq)
# a variável não afetou a qualidade do ar

# A quantidade de chuva influencia na qualidade doa ar?
m4 <- lm(airq~rain, data=Airq)
summary(m4)
# quantidade de chuva não afeta a qualidade do ar

# A densidade populacional afeta a qualidade do ar?
m5 <- lm(airq~dens, data=Airq)
summary(m5)
# nãó existe efeito da densidade populacional na qualidade do ar

# retas em modelos não significativos são opcionais nos gráficos

# retas nos gráficos
plot(airq~medi, data=Airq)
# y=a+b*x
# a <- intercepto (onde a reta vai tocar o eixo y)
# b <- inclinação da reta
curve(9.936e+01+5.638e-04*x, add=T)

# Melhorar o gráficos m3
plot(airq~medi, data=Airq, xlab="Renda média per capita", ylab="Qualidade do Ar", pch=1, col='blue')
curve(9.936e+01+5.638e-04*x, add=T, col='red', lwd=2,lty=2)

# Melhorando o gráficos m1
plot(airq~vala, data=Airq, xlab="Valor das empreas $",
     ylab="Qualidade do Ar", col='blue', pch=16, cex=1.2)
curve(96.451419+0.001969*x, add=T, col='darkblue', lwd=2, lty=2)
  
# Melhorando o gráficos m2
plot(airq~coas, data=Airq, xlab="Posição Costeira", ylab='Qualidade do Ar',
     col='lightblue', ylim=c(50,170), cex.lab=1.3, main='Análise da qualidade do ar')

#Melhorando grafico m5
plot(airq~dens, data=Airq, xlab="Densidade Populacional", ylab="Qualidade do Ar", pch=1, col='blue')
curve(1.054e+02+-3.857e-04*x, add=T, col='red', lwd=2, lty=2)

# Regressão Múltipla
# ------------------------------------------

m_rm1 <- lm(airq~vala+coas, data=Airq)
summary(m_rm1)
# então existe ume efeito da posição costeira e do valor das empresas na qualdiade do ar


# Gráfico Regressão Múltipla
plot(airq~vala, data=Airq, xlab="Valor das empreas $",
     ylab="Qualidade do Ar")
curve(1.171e+02+1.999e-03*x, add=T) # cidade não costeira
curve(1.171e+02+1.999e-03*x+-2.968e+01, lty=2, add=T) # cidade costeira
legend("bottomright", c("Não Costeiras", "Costeiras"), pch=1, lty=c(1,2), bty='n')
# A qualidade do ar das cidades tanto pelo valor das empreas quanto pela
# posição costeiras das cidades. Quanto maior valor das empresas, pior a qualidade do ar das cidades.
# Além, disso, as cidades não-costeiras apresentam qualidade do ar pior do que as cidades costeiras.

m_rm2 <- lm(airq~vala+coas+dens, data=Airq)
summary(m_rm2)

# Contraste de modelos
# ------------------------------------------

# Comparar  um modelo completo com um modelo sem variavel em questão
modelo_completo <- lm(airq~vala+coas+dens, data=Airq)
modelo_incompleto <- lm(airq~vala+coas, data=Airq)
# os modelos são iguals?
# se p valor for > 0.05 não existe diferença dos modelos, entao continuo com modelo mais simples
# se p valor for < 0.05 os modelos são diferentes e a variável não deve ser retirada do modelo
anova(modelo_completo, modelo_incompleto)

# Gráfico Final
# ------------------------------------------

plot(airq~vala, data=Airq, xlab="Valor das empreas $",
     ylab="Qualidade do Ar", cex.lab=1.3, col='blue')
curve(1.171e+02+1.999e-03*x, add=T,col='darkblue', lwd=1.4) # cidade não costeira
curve(1.171e+02+1.999e-03*x+-2.968e+01, lty=2, add=T,col='darkblue', lwd=1.4) # cidade costeira
legend("bottomright", c("Não Costeiras", "Costeiras"), pch=1, lty=c(1,2), bty='n', 
       col=c("darkblue", "darkblue"))

# Conclusão
# ------------------------------------------
# O que afeta a qualidade do ar nas cidades?
# As variáveis que afetaram foram: (a) o valor das empresas e (b) a posição costeira das cidades
# Quanto maior valor das empresas, pior a qualidade do ar. Cidades costerias apresentam uma melhor 
# qualidade do ar.