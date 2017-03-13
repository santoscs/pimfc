# prevendo.inflacao.fatores.comuns

Tutorial para reproduzir os resultados do artigo "Prevendo a Inflação no Brasil com grande conjunto de dados: uma aplicação do modelo de fatores comuns"


## Requisitos técnicos

Caso não tenha ainda instalado:

1. Instale o pacote `devtools` do CRAN com o seguinte comando `install.packages("devtools")`.
2. Programas auxiliares:
    - **Windows:** instale [Rtools](http://cran.r-project.org/bin/windows/Rtools/)
    - **Mac:** Instale Xcode no Mac App Store
    - **Linux:** Instale várias bibliotecas de desenvolvimento (detalhes variam entre as diferentes distribuições de Linux).
    

## Reproduzindo os resultados no R

### Instala os dados e funções necessárias

```{r}
#devtools::install_github("santoscs/pimfc")
```

### Importa os dados


```{r}
devtools::load_all()
library(zoo)
# dados das séries macroeconômicas
series <- macroseries

# importa o ipca em 12 meses previsto pela focus
ipca12.focus <- focus

# ipca em 12 meses
ipca12 <- acum(series[,"X38513"])

#restringe periodo dos dados ao mesmo do ipca12
series <- window(series, start=start(ipca12))

pib <- series[,"X521274780"]
selic <- series[,"X32241"]
```

### Realiza as previsões fora da amostra

Este procedimento é demorado devido a grande quantidade de processamento.


```{r}
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
ipca12.arima <- out.arima$fcast
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


```{r}
library(nimcno)
tsplot(ipca12.fig, name = "IPCA")
```



### Tabela 1 - Habilidade preditiva fora da amostra, 2008.5-2014.2


```{r}
# matriz de dados
x <- cbind(ipca12, ipca12.focus, ipca12.arima,
           ipca12.var, ipca12.DI, ipca12.DItf,
           ipca12.DItp, ipca12.DItfp)
tab <- tab.reqm(x, obs = "ipca12", ref = "ipca12.focus")
knitr::kable(tab)
```



### Tabela 2 - Teste de previsão incorporada, 2008.5-2014.2

```{r }
tab <- tab.enctest(x, obs = "ipca12", ref = "ipca12.focus")
knitr::kable(tab)
```


### Tabela 3 - Habilidade preditiva fora da amostra das previsões combinadas, 2008.5-2014.2


```{r }
ipca12.carima <- 0.5*ipca12.focus +  0.5*ipca12.arima
ipca12.cvar <- 0.5*ipca12.focus +  0.5*ipca12.var
ipca12.cdi <- 0.5*ipca12.focus +  0.5*ipca12.DI
ipca12.cditf <- 0.5*ipca12.focus +  0.5*ipca12.DItf
ipca12.cditp <- 0.5*ipca12.focus +  0.5*ipca12.DItp
ipca12.cditfp <- 0.5*ipca12.focus +  0.5*ipca12.DItfp

x <- cbind(ipca12, ipca12.focus, ipca12.carima, ipca12.cvar,
           ipca12.cdi, ipca12.cditf, ipca12.cditp, ipca12.cditfp)

tab <- tab.reqm(x, obs = "ipca12", ref = "ipca12.focus")
knitr::kable(tab)
```


## Apendice A

## Tabela descritiva

```{r, results='asis', echo=FALSE}
tab <- descricao[,c('serie','unidade', "cod", "trans")]
library(knitr)
kable(tab)
```


## Graficos das series

```{r graficos}

# series sem transformacao
series <- rawmacroseries

# padroniza
st <- zoo(apply(series, 2, function(x){(x-mean(x))/sd(x)}), order.by = index(series))

# series transformadas
series_trans <- macroseries
# padroniza
tr <- zoo(apply(series_trans, 2, function(x){(x-mean(x))/sd(x)}), order.by = index(series_trans))

# graficos

library(gridExtra)
library(nimcno)
for(i in 1:(floor(length(st[1,])/10)/2)){
  if(i!=(floor(length(st[1,])/10)/2)){
    p1 <- tsplot(st[,(10*(2*i-1)-9):(10*(2*i-1))])
    p2 <- tsplot(tr[,(10*(2*i-1)-9):(10*(2*i-1))])
    p3 <- tsplot(st[,(10*(2*i)-9):(10*(2*i))])
    p4 <- tsplot(tr[,(10*(2*i)-9):(10*(2*i))])
    grid.arrange(p1, p2, p3, p4, ncol = 4)
  }else{
    p1 <- tsplot(st[,(10*(2*i-1)-9):(10*(2*i-1)+1)])
    p2 <- tsplot(tr[,(10*(2*i-1)-9):(10*(2*i-1)+1)])
    p3 <- tsplot(st[,(10*(2*i)-9):(10*(2*i)+1)])
    p4 <- tsplot(tr[,(10*(2*i)-9):(10*(2*i)+1)])
    grid.arrange(p1, p2, p3, p4, ncol = 4)
  }
  
}

```


## Tabelas regressão

```{r regre}
library(stargazer)
# previsoes dos modelos
stargazer(summary(out.arima$fit))
tab <- summary(out.VAR$fit$fit)
stargazer(tab$varresult$ipca12$coefficients)

stargazer(out.DI$model$fit, out.DI_tp$model$fit, out.DI_tp$model$fit, out.DI_tf$model$fit, out.DI_tfp$model$fit,  type= "html", out="models.htm")


```
