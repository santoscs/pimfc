##################################################################
### Prevendo a inflacao no Brasil com grande conjunto de dados ###
### uma aplicacao do modelo de fatores e indice de difusao     ###
##################################################################

#############
### Dados ###
#############

# importa os dados das series macroeconomicas do Brasil transformadas
data(macroseries)
periodo <- macroseries[1,1]
ano <- as.numeric(substring(periodo[1],1,4))
mes <- as.numeric(substring(periodo[1],6,7))
series <- ts(macroseries[,-1], start = c(ano,mes), freq=12)
# ipca
ipca <- series[,"X38513"]

# previsoes da pesquisa Focus do BC do Brasil
data(focus)
periodo <- focus[1,1]
ano <- as.numeric(substring(periodo[1],1,4))
mes <- as.numeric(substring(periodo[1],6,7))
ipca12.focus <- ts(focus$ipca12.focus, start = c(ano,mes), frequency = 12)
ipca12.focus <- window(ipca12.focus, end=end(series))

# ipca em 12 meses
ipca12 <- acum(ipca, m=12)

# restringe periodo dos dados ao mesmo do ipca12
series <- window(series, start=start(ipca12))

#series para o modelo VAR
pib <- series[,"X521274780"]
selic <- series[,"X32241"]

############################################
### Realiza as previsoes fora da amostra ###
############################################

# Previsao fora da amostra com  VAR
y <- cbind(ipca12, pib, selic)
out12.VAR <- outsample.var(y, max.p = 6, h=12, k=70)

# Previsao fora da amostra com modelos indice de difusao (DI)
y <- ipca12
x <- series[,colnames(series)!="X38513"]
out12.DI <- outsample.di(yh=y, yt=y, x=x, k=4, m=3, p=6, n=70, h=12)
out12.DI_tf <- outsample.ditf(yh=y, yt=y, x=x, m=3, p=6, n=70, h=12)
out12.DI_tp <- outsample.ditp(yh=y, yt=y, x=x, k=4, m=3, p=6, n=70, h=12)
out12.DI_tfp <- outsample.ditfp(yh=y, yt=y, x=x, m=3, p=6, n=70, h=12)

# Previsao fora da amostra com ARIMA
out12.arima <- outsample.arima(y, h = 12, k = 70)

# previsoes dos modelos
ipca12.arima <- out12.arima$fcast[,1]
ipca12.var <- out12.VAR$fcast$ipca[,1]
ipca12.DI <- out12.DI$fcast
ipca12.DItp <- out12.DI_tp$fcast
ipca12.DItf <- out12.DI_tf$fcast
ipca12.DItfp <- out12.DI_tfp$fcast

# ipca em 12 meses no periodo fora da amostra
ipca12 <- window(ipca12, start=start(ipca12.arima))

# previsao da focus no periodo fora da amostra
ipca12.focus <- window(ipca12.focus, start=start(ipca12), end=end(ipca12))

# matriz de dados com as previsoes
x <- cbind(ipca12, ipca12.focus, ipca12.arima,
           ipca12.var, ipca12.DI, ipca12.DItf, 
           ipca12.DItp, ipca12.DItfp)

################
### Figura 1 ###
################

library(ggfortify)
autoplot(ipca12)


#######################################################
### Tabela 1 - Habilidade preditiva fora da amostra ###
#######################################################

tab <- tab.reqm(x, obs = "ipca12", ref = "ipca12.focus")
knitr::kable(tab)

##############################################################
### Tabela 2 - Resultados do teste de previsão incorporada ###
##############################################################

tab <- tab.enctest(x, obs = "ipca12", ref = "ipca12.focus")
knitr::kable(tab)

################################################################
### Tabela 3 - Habilidade preditiva das previsões combinadas ###
################################################################

# combinacao da previsao da focus com a de cada modelo
# por meio da media aritimetica
ipca12.carima <- 0.5*ipca12.focus +  0.5*ipca12.arima
ipca12.cvar <- 0.5*ipca12.focus +  0.5*ipca12.var
ipca12.cdi <- 0.5*ipca12.focus +  0.5*ipca12.DI
ipca12.cditp <- 0.5*ipca12.focus +  0.5*ipca12.DItp
ipca12.cditf <- 0.5*ipca12.focus +  0.5*ipca12.DItf
ipca12.cditfp <- 0.5*ipca12.focus +  0.5*ipca12.DItfp

# matriz com previsoes combinadas
x <- cbind(ipca12, ipca12.focus, ipca12.carima, ipca12.cvar,
           ipca12.cdi, ipca12.cditp, ipca12.cditf, ipca12.cditfp)

tab <- tab.reqm(x, obs = "ipca12", ref = "ipca12.focus")
knitr::kable(tab)


