#' @title Estimacao dos fatores com componentes principais
#' 
#' @description Estima os fatores usando componentes principais a 
#' partir da matriz X'X da series padronizadas
#' 
#' @details Estimacao dos factors com componentes principais segundo 
#' Stock e Watson (1998). Se norm=TRUE as colunas de X sao 
#' padronizada para ter media zero e desvio padrao 1
#' 
#' @param x matriz de preditores
#' @param norm logical indicando quando os dados sao padronizados.
#' Padrao TRUE
#' 
#' @return lambda(matrix): loadings; factors(mts): fatores estimado;
#' autovalor(vector): autovalores de x'x  
#' 
#' @import stats
#' 
#' @export
#' 

pc.stand <- function(x, norm=TRUE){
  
  # atributo temporal
  atr <- tsp(x)
  # padroniza os dados
  if(norm==TRUE){
    x <- apply(x, 2, function(x) (x - mean(x))/sd(x))
  }
  x <- ts(x, start=atr[1], frequency = atr[3])
  aux <- eigen(t(x)%*%x)
  vetor <- aux$vectors
  valor <- aux$values
  lambda <- sqrt(length(x[1,]))*vetor
  factors <- x %*% lambda/length(x[1,])
  factors <- ts(factors, start=atr[1], frequency = atr[3], names = paste("f",1:length(x[1,]), sep=""))
  return(list(lambda=lambda, factors=factors, autovalor=valor))
}

#' @title Estimacao dos fatores targeteds
#'
#' @description Estima os fatores usando componentes principais a 
#' partir da matriz X'X da series padronizadas
#' 
#' @details Estimacao de um fator sintetico com componentes principais 
#' segundo Dias et.al. (2010). Se norm=TRUE as colunas de X sao 
#' padronizadas para ter media zero e desvio padrao 1
#' 
#' @param x (mts) matriz de preditores 
#' @param y (ts) variavel a ser prevista  
#' @param h (int) numero do horizonte das previsoes 
#' @param norm logical indicando quando os dados sao padronizados. Padrao TRUE
#' 
#' @return lambda(matrix) loadings;  factor(ts) fator sintetico 
#' estimado; autovalor(vector) autovalores de (1/(N*(T-h)))x'x
#' 
#' @import stats
#' 
#' @export
#' 

pc.target <- function(x, y, h=12, norm=TRUE){

  # verifica cobertura temporal das series
  if(!identical(round(tsp(y),3), round(tsp(x), 3)))
    stop("series com coberturas temporais diferentes")
  # atributo temporal
  atr <- tsp(x)
  tempo <- time(x)
  Tn <- dim(x)[1]
  N <- dim(x)[2]
  # exclui as h ultimas observacoes
  xh<-window(x, end=tempo[(length(x[,1])-h)])
  # exclui as h primeiras observacoes
  yh<-window(y, start=tempo[(1+h)])
  # padroniza as series
  if(norm==TRUE){
    xh <- apply(xh, 2, function(x) (x - mean(x))/sd(x))
  }
  # estima os fatores
  aux <- eigen((1/(N*(T-h)))*t(xh)%*%xh)
  vetor <- aux$vectors
  valor <- aux$values
  lambda <- vetor
  factors <- x %*% lambda/N
  # calcula o fator sintetico
  covariance <- apply(factors, 2, function(x) (1/(T-h))*sum(y*x))
  ratio <- covariance*(valor/valor[1])/sum(covariance*(valor/valor[1]))
  f <- apply(factors, 1, function(x) sum(ratio*x))
  f <- cbind(f, f*NA)
  f <- ts(f, start=atr[1], frequency = atr[3], names=c("f1", "fna"))
  return(list(lambda=lambda, factor=f, autovalor=valor))
}

#' Funcao aplica teste t
#' 
#' calcula o valor p do vetor x na regressao dos dados y=lags(y) + x
#' em que y esta em dados
#' 
#' @param x vetor a ser testado
#' @param dados data frame com y e seus quatro lags
#' @param h horizonte de previsao 
#' 
#' @return pvalue com o valor p

hard.test <- function(x, dados, h=12){
  dados <- cbind(dados, c(rep(NA, h), x, rep(NA,3)))
  dados <- na.omit(dados)
  colnames(dados) <- c("y","y1","y2","y3","y4", "x")
  fit <- lm(y ~ y1 + y2 + y3 + y4 + x, data = dados)
  aux <- summary(fit)
  pvalue <- aux$coefficients[6,4]
  return(pvalue)
}

#' Selecao de preditores tagerteds
#'
#' Seleciona as variaveis apresentam poder preditivo 
#' de acordo com Bai and Ng (2008) 
#'
#' @param y (ts) serie a ser prevista
#' @param x (mts) matriz de preditores sem a variavel target a ser prevista
#' @param h (num) numero do horizonte das previsoes fora da amostra
#' @param alpha (num) o nivel de significancia critico 
#'
#' @return \code{sele} (vector) nome das colunas de x que foram selecionadas
#' 
#' @export
#' 

hard.threshold <- function(y, x, h=12, alpha=0.05){
  v <- colnames(x)
  dados <- data.frame(cbind(y, lag(y, -h), lag(y, -(h+1)), lag(y, -(h+2)), lag(y, -(h+3))))
  pvalue <- apply(x, 2, hard.test, dados=dados, h=12)
  sele <- v[which(pvalue<=alpha)]
  return(sele)  
}


#' @title Estima e Prever com Indice de Difussao (di)
#' 
#' @description Estima uma regressao de yh sobre k fatores e seus m lags e 
#' sobre yt e seus p lags
#' 
#' @details yh igual a yt ou a alguma transformacao de yt, esse modelo
#' de previsao se baseia em Stock e Watson 1998
#'
#' @param yh (ts) serie a ser prevista \eqn{y_{t+h}^h} 
#' @param yt (ts) preditor \eqn{y_{t-j+1}}
#' @param f (ts) fatores comuns estimados \eqn{F_{t-j+1}}
#' @param m numero de defasagens dos fatores
#' @param p numero de defasagens de yt
#' @param h horizonte de previsao
#' 
#' @return uma lista com: fit(dyn): o modelo estimado e 
#' fcast(ts): valor previsto para \eqn{yh_{t+h}}
#' 
#' @import dyn zoo utils
#' @export

di <- function(yh, yt, f, h, m, p){
  # verifica se as series temporais estao em concordancia
  if(!identical(round(tsp(yh),3), round(tsp(yt), 3)))
    stop("series yh e yt com inicio, fim ou frequencia diferente")
  if(!identical(round(tsp(yh),3), round(tsp(f), 3)))
    stop("series yh e f com inicio, fim ou frequencia diferente")
  # transforma para o formato zoo
  if (requireNamespace("zoo", quietly=TRUE)) {
    dados <- zoo::as.zoo(ts.intersect(yh, yt, f))
  } else {
    stop("Please install package 'zoo'.")
  } 
  # nomes dos fatores f
  v <- colnames(dados)[-(1:2)]
  # funcao lag
  L <- function(x, k = 1) stats::lag(x, -k)
  form <- as.formula(paste("yh ~ L(yt, h:(h+p-1)) + ",
                           paste("L(", v,", h:(h+m-1))",
                                 collapse=" + ", sep="")))
  fit <- dyn::dyn$lm(form, data=dados)
  fcast <- tail(na.omit(predict(fit, dados)), 1)
  return(list(fit=fit, fcast=fcast))
}

#' @title Seleciona o modelo di
#'
#' @description Seleciona o modelo Indice de Difussao com menor BIC
#'
#' @param yh serie a ser prevista \eqn{y_{t+h}^h} 
#' @param yt preditor \eqn{y_{t-j+1}}
#' @param f fatores comuns estimados \eqn{F_{t-j+1}}
#' @param m numero maximo de defasagens dos fatores
#' @param p numero maximo de defasagens de yt
#' @param h horizonte de previsao
#' @param k numero maximo de fatores 
#' 
#' @return best.fit o modelo selecionado dyn
#' 
#' @import stats
#' @export

di.selec <- function(yh, yt, f, h, k, m, p){
  bic.best <- Inf
  for(l in 1:k){
    for(i in 1:m){
      for(j in 1:p){
        model <- di(yh=yh, yt=yt, f=f[,1:l], h=h,m=i,p=j)
        bic <- BIC(model$fit)
        if(bic<bic.best){
          bic.best <- bic
          model.best <-model
        }
      }
    }
  }
  return(model.best)
}

#' @title Previsao fora da amostra com di
#' 
#' @description Realiza previsoes fora da amostra para yh com base no 
#' modelo di selecionado por BIC 
#' 
#' @param yh serie a ser prevista \eqn{y_{t+h}^h} 
#' @param yt preditor \eqn{y_{t-j+1}}
#' @param m numero maximo de defasagens dos fatores
#' @param p numero maximo de defasagens de yt
#' @param h horizonte de previsao
#' @param k numero maximo de fatores 
#' @param n numero de previsoes fora da amostra
#' @param x (mts) matriz de preditores
#'   
#' @return lista contendo \code{fcast} (ts) valores previstos; \code{model} 
#' (dyn) modelo estimado no fim da amostra
#' 
#' @import stats
#' 
#' @export

outsample.di <-function(yh, yt, x, k=1, m=3, p=3, n, h=12){
  # verifica se as series temporais estao em concordancia
  if(!identical(tsp(yh), tsp(yt)))
    stop("series com inicio, fim ou frequencia diferente")
  if(!identical(tsp(yh), tsp(x)))
    stop("series com inicio, fim ou frequencia diferente")
  
  # tamanho, data e atributos das series
  Tn <- dim(x)[1]
  date <- time(x)
  atr <- tsp(x)
  fcast <- vector()
  for(i in 1:n){
    # restringe os dados
    yh.ajuste <- window(yh, end=date[Tn-n-h+i])
    yt.ajuste <- window(yt, end=date[Tn-n-h+i])
    x.ajuste <- window(x, end=date[Tn-n-h+i])
    f <- pc.stand(x.ajuste)$factors[,1:k] 
    # estima o modelo
    model <- di.selec(yh=yh.ajuste, yt=yt.ajuste, f=f, h=h, k=k, m=m, p=p)
    # get prediction for ith value
    fcast[i] <- model$fcast
  }
  fcast <- ts(fcast, end=index(model$fcast), frequency = atr[3])
  return(list(fcast=fcast, model=model))
}


#' @title Previsao fora da amostra com di e preditores targeted
#' 
#' @description Realiza previsoes fora da amostra para yh com base no 
#' modelo DI selecionado por BIC e preditores targeted
#' 
#' @inheritParams outsample.di
#'   
#' @return lista contendo \code{fcast} (ts) valores previstos; \code{model} 
#' (dyn) modelo estimado no fim da amostra; \code{sele} (charc) vetor com o
#' nome do preditores selecionados 
#' 
#' @import stats
#' 
#' @export

outsample.ditp<-function(yh, yt, x, k=1, m=3, p=3, h=12, n){
  # verificar se as series apresentam tamanhos iguais
  if(!identical(tsp(yh), tsp(yt)))
    stop("series com inicio, fim ou frequencia diferente")
  if(!identical(tsp(yh), tsp(x)))
    stop("series com inicio, fim ou frequencia diferente")
  
  # tamanho, data e atributos das series
  Tn <- length(x[,1])
  date <- time(x)
  atr <- tsp(x)
  fcast <- vector()
  for(i in 1:n){
    # restringe os dados
    yh.ajuste <- window(yh, end=date[Tn-n-h+i])
    yt.ajuste <- window(yt, end=date[Tn-n-h+i])
    x.ajuste <- window(x, end=date[Tn-n-h+i])
    # Targeted predictors
    sele <- hard.threshold(y=yh.ajuste, x=x.ajuste, h=h, alpha=0.1)
    f <- pc.stand(x.ajuste[,sele])$factors[,1:k] 
    # estima o modelo
    model <- di.selec(yh=yh.ajuste, yt=yt.ajuste, f=f, h=h, k=k, m=m, p=p)
    # get prediction for ith value
    fcast[i] <- model$fcast
  }
  fcast <- ts(fcast, end=index(model$fcast), frequency = atr[3])
  return(list(fcast=fcast, model=model, sele=sele))
}

#' @title Previsao fora da amostra com DI e fatores targeted
#' 
#' @description Realiza previsoes fora da amostra para yh com base no 
#' modelo DI selecionado por BIC e com fatores tergeted
#' 
#' @param yh serie a ser prevista \eqn{y_{t+h}^h} 
#' @param yt preditor \eqn{y_{t-j+1}}
#' @param m numero maximo de defasagens dos fatores
#' @param p numero maximo de defasagens de yt
#' @param h horizonte de previsao
#' @param n numero de previsoes fora da amostra
#' @param x (mts) matriz de preditores
#'   
#' @return lista contendo \code{fcast} (ts) valores previstos; \code{model} 
#' (dyn) modelo estimado no fim da amostra
#' 
#' @import stats
#' 
#' @export

outsample.ditf<-function(yh, yt, x, m=3, p=3, n, h=12){
  # verificar se as series apresentam tamanhos iguais
  if(!identical(tsp(yh), tsp(yt)))
    stop("series com inicio, fim ou frequencia diferente")
  if(!identical(tsp(yh), tsp(x)))
    stop("series com inicio, fim ou frequencia diferente")
  
  # tamanho, data e atributos das series
  Tn <- length(x[,1])
  date <- time(x)
  atr <- tsp(x)
  fcast <- vector()
  for(i in 1:n){
    # restringe os dados
    yh.ajuste <- window(yh, end=date[Tn-n-h+i])
    yt.ajuste <- window(yt, end=date[Tn-n-h+i])
    x.ajuste <- window(x, end=date[Tn-n-h+i])
    # Factor target
    f <- pc.target(y=yh.ajuste, x=x.ajuste, h=h)$factor 
    # estima o modelo
    model <- di.selec(yh=yh.ajuste, yt=yt.ajuste, f = f, h=h, k=1, m=m, p=p)
    # get prediction for ith value
    fcast[i] <- model$fcast
  }
  fcast <- ts(fcast, end=index(model$fcast), frequency = atr[3])
  return(list(fcast=fcast, model=model))
}

#' Acumula series em percentual ao mes em m meses
#' 
#' Transforma uma serie mensal dada em percentual ao mes 
#' em uma serie mensal com percentual nos ultimos m meses
#'
#' @param x A time series univariate
#' @param m number of monthes
#' 
#' @return A time series univariate 
#' 
#' @import zoo stats
#' @export

acum<-function(x, m=12){
  # input:
  # x(ts): serie a ser acumulada
  #output: 
  # x12(ts): serie acumulada
  
  x <- zoo::as.zoo(x)
  x12 <- zoo::rollapplyr(x, width=m, function(x) (prod(1+x/100)-1)*100)
  x12 <- as.ts(x12)
  return(x12)
}

#' @title Previsao fora da amostra com DI com fatores e preeditores targeted
#' 
#' @description Realiza previsoes fora da amostra para yh com base no 
#' modelo DI selecionado por BIC e com fatores e preditores tergeted
#' 
#' @param yh serie a ser prevista \eqn{y_{t+h}^h} 
#' @param yt preditor \eqn{y_{t-j+1}}
#' @param m numero maximo de defasagens dos fatores
#' @param p numero maximo de defasagens de yt
#' @param h horizonte de previsao
#' @param n numero de previsoes fora da amostra
#' @param x (mts) matriz de preditores
#'   
#' @return lista contendo \code{fcast} (ts) valores previstos; \code{model} 
#' (dyn) modelo estimado no fim da amostra
#' 
#' @import stats
#' 
#' @export

outsample.ditfp <-function(yh, yt, x, m=3, p=3, n, h=12){
  # verificar se as series apresentam tamanhos iguais
  if(!identical(tsp(yh), tsp(yt)))
    stop("series com inicio, fim ou frequencia diferente")
  if(!identical(tsp(yh), tsp(x)))
    stop("series com inicio, fim ou frequencia diferente")
  
  # tamanho, data e atributos das series
  Tn <- length(x[,1])
  date <- time(x)
  atr <- tsp(x)
  fcast <- vector()
  for(i in 1:n){
    # restringe os dados
    yh.ajuste <- window(yh, end=date[Tn-n-h+i])
    yt.ajuste <- window(yt, end=date[Tn-n-h+i])
    x.ajuste <- window(x, end=date[Tn-n-h+i])
    # Targeted predictors
    sele <- hard.threshold(y=yh.ajuste, x=x.ajuste, h=h, alpha=0.1)
    # Targeted factor 
    f <- pc.target(y=yh.ajuste, x=x.ajuste[,sele], h=h)$factor 
    # estima o modelo
    model <- di.selec(yh=yh.ajuste, yt=yt.ajuste, f = f, h=h, k=1, m=m, p=p)
    # get prediction for ith value
    fcast[i] <- model$fcast
  }
  fcast <- ts(fcast, end=index(model$fcast), frequency = atr[3])
  return(list(fcast=fcast, model=model))
}
