# prevendo.inflacao.fatores.comuns

Rápido tutorial para reproduzir os resultados do artigo "Prevendo a Inflação no Brasil com grande conjunto de dados: uma aplicação do modelo de fatores comuns"


## Requisitos técnicos

Caso não tenha ainda instalado:

1. Instale o pacote `devtools` do CRAN com o seguinte comando `install.packages("devtools")`.
2. Programas auxiliares:
    - **Windows:** instale [Rtools](http://cran.r-project.org/bin/windows/Rtools/)
    - **Mac:** Instale Xcode no Mac App Store
    - **Linux:** Instale várias bibliotecas de desenvolvimento (detalhes variam entre as diferentes distribuições de Linux).
    
## Reproduzindo os resultados 


### Instala os dados e funções necessárias

```r
devtools::install_github("santoscs/prevendo.inflacao.fatores.comuns")
```

### Importa os dados


```r
library(prevendo.inflacao.fatores.comuns)

# importa dados das séries macroeconômicas
series <- read.csv("data-raw/series_transformadas.csv", sep="")
periodo <- series[1,1]
ano <- as.numeric(substring(periodo[1],1,4))
mes <- as.numeric(substring(periodo[1],6,7))
series <- ts(series[,-1], start = c(ano,mes), freq=12)

# importa o ipca em 12 meses previsto pela focus
ipca12.focus <- read.csv("data-raw/focus_ipca_bcb.csv", sep="")
periodo <- ipca12.focus[1,1]
ano <- as.numeric(substring(periodo[1],1,4))
mes <- as.numeric(substring(periodo[1],6,7))
ipca12.focus <- ts(ipca12.focus[,2], start = c(ano,mes), freq=12)
ipca12.focus <- window(ipca12.focus, end=end(series))

# ipca em 12 meses
ipca12 <- acum(series[,"X38513"])

#restringe periodo dos dados ao mesmo do ipca12
series <- window(series, start=start(ipca12))

pib <- series[,"X521274780"]
selic <- series[,"X32241"]
```

### Realiza as previsões fora da amostra

Este procedimento é demorado devido a grande quantidade de processamento.


```r
# previsões fora da amostra usando fatores comuns
y <- ipca12
x <- series[,colnames(series)!="X38513"]
out.DI <- outsample.di(yh=y, yt=y, x=x, k=3, m=3, p=3, n=60, h=12)
out.DI_tf <- outsample.ditf(yh=y, yt=y, x=x, m=3, p=3, n=60, h=12)
out.DI_tp <- outsample.ditp(yh=y, yt=y, x=x, k=3, m=3, p=3, n=60, h=12)
out.DI_tfp <- outsample.ditfp(yh=y, yt=y, x=x, m=3, p=3, n=60, h=12)

# previsões fora da amostra usando ARIMA e VAR
out.arima <- outsample.arima(ipca12, h=12, k=60)
y <- cbind(ipca12, pib, selic)
out.VAR <- outsample.var(y, h=12, k=60)

# previsoes dos modelos
ipca12.arima <- out.arima$fcast[,1]
ipca12.var <- out.VAR$fcast$ipca[,1]
ipca12.DI <- out.DI$fcast
ipca12.DItp <- out.DI_tp$fcast
ipca12.DItf <- out.DI_tf$fcast
ipca12.DItfp <- out.DI_tfp$fcast
# ipca em 12 meses
ipca12.fig <- ipca12
ipca12 <- window(ipca12, start=start(ipca12.arima))
# previsao da focus
ipca12.focus <- window(ipca12.focus, start=start(ipca12), end=end(ipca12))
```

### Figura 1 - IPCA acumulado em 12 meses - 1996.1 - 2014.2


```r
plot(ipca12.fig)
```



### Tabela 1 - Habilidade preditiva fora da amostra, 2008.5-2014.2


```r
# matriz de dados 
x <- cbind(ipca12, ipca12.focus, ipca12.arima,
           ipca12.var, ipca12.DI, ipca12.DItf, 
           ipca12.DItp, ipca12.DItfp)
tab <- tab.reqm(x, obs = "ipca12", ref = "ipca12.focus")
knitr::kable(tab)
```


              | reqm  | eqmr  | dm test 
------------- | ----- | ----- | --------
ipca12.focus  | 5,29  | 1,00  |         
ipca12.arima  | 0,89  | 0,03  | 0       
ipca12.var    | 1,48  | 0,08  | 0       
ipca12.DI     | 0,96  | 0,03  | 0       
ipca12.DItf   | 1,06  | 0,04  | 0       
ipca12.DItp   | 0,95  | 0,03  | 0       
ipca12.DItfp  | 1,08  | 0,04  | 0       

### Tabela 2 - Teste de previsão incorporada, 2008.5-2014.2


Modelo A      | Modelo B      | Lambda (valor p) 
------------- | ------------- | -----------------
ipca12.focus  | ipca12.arima  | 0,34 (0,39)      
ipca12.focus  | ipca12.var    | -0,23 (0,42)     
ipca12.focus  | ipca12.DI     | 0,23 (0,24)      
ipca12.focus  | ipca12.DItf   | -0,92 (0,00)     
ipca12.focus  | ipca12.DItp   | 0,31 (0,21)      
ipca12.focus  | ipca12.DItfp  | 0,06 (0,89)      

### Tabela 3 - Habilidade preditiva fora da amostra das previsões combinadas, 2008.5-2014.2


```r
ipca12.carima <- 0.5*ipca12.focus +  0.5*ipca12.arima
ipca12.cvar <- 0.5*ipca12.focus +  0.5*ipca12.var
ipca12.cdi <- 0.5*ipca12.focus +  0.5*ipca12.DI
ipca12.cditf <- 0.5*ipca12.focus +  0.5*ipca12.DItf
ipca12.cditp <- 0.5*ipca12.focus +  0.5*ipca12.DItp
ipca12.cditfp <- 0.5*ipca12.focus +  0.5*ipca12.DItfp

x <- cbind(ipca12, ipca12.focus, ipca12.comb,
           ipca12.comb2, ipca12.carima, ipca12.cvar,
           ipca12.cdi, ipca12.cditf, ipca12.cditfp)
tab <- tab.reqm(x, obs = "ipca12", ref = "ipca12.focus")
knitr::kable(tab)
```


              |  reqm |  eqmr |  dm test 
--------------|  -----|  -----|  --------
ipca12.focus  |  5,29 |  1,00 |          
ipca12.carima |  2,82 |  0,28 |  0       
ipca12.cvar   |  2,87 |  0,29 |  0       
ipca12.cdi    |  2,65 |  0,25 |  0       
ipca12.cditf  |  2,61 |  0,24 |  0       
ipca12.comb2  |  2,68 |  0,26 |  0       
ipca12.cditfp |  2,46 |  0,22 |  0       
